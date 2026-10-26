/// The app's side of its workers: starting `tambour --worker`, talking to it, and keeping one in
/// reserve so that a cancelled or worn-out worker is replaced at once.
namespace Tambour

open System
open System.Diagnostics
open System.IO
open System.Net.Sockets
open System.Text
open System.Threading
open System.Threading.Channels
open System.Threading.Tasks
open Tambour.Protocol

/// How to start this app again in another role.
type Command = { File: string; Arguments: string list }

module Command =

    /// This app as it is running: its own executable, or `dotnet` and its assembly when it was
    /// started through `dotnet`.
    let ofThisApp () : Command =
        let exe = Environment.ProcessPath

        if Path.GetFileNameWithoutExtension exe = "tambour" then
            { File = exe; Arguments = [] }
        else
            let dotnet =
                if Path.GetFileNameWithoutExtension exe = "dotnet" then exe
                else
                    match Environment.GetEnvironmentVariable "DOTNET_HOST_PATH" with
                    | null
                    | "" -> "dotnet"
                    | path -> path

            { File = dotnet; Arguments = [ typeof<Command>.Assembly.Location ] }

/// One worker process and the connection to it.
type WorkerProcess private (proc: Process, connection: Connection, replies: ChannelReader<Reply>, log: StringBuilder) =

    let exited = proc.WaitForExitAsync()

    /// How long a worker has to connect before it is given up on.
    static member ConnectTimeout = TimeSpan.FromSeconds 60.0

    member _.Pid = proc.Id

    /// What the worker said, in order. The reader completes when the worker goes away.
    member _.Replies = replies

    /// What the process wrote to its own stdout and stderr, outside the protocol: a crash's
    /// message, a runtime's complaint.
    member _.Log = lock log (fun () -> log.ToString())

    member _.SendAsync(request: Request) : Task =
        connection.SendAsync(Codec.encodeRequest request, CancellationToken.None)

    /// Ends the process and everything it started. Its replies end with it.
    member _.Kill() =
        try
            proc.Kill true
        with _ ->
            ()

        (connection :> IDisposable).Dispose()

    /// Completes when the process has ended.
    member _.Exited: Task = exited

    /// Starts a worker and waits for it to connect. The worker ends when the process `app`
    /// does, this one unless stated.
    static member StartAsync(command: Command, ?app: int) : Task<WorkerProcess> =
        task {
            let app = defaultArg app Environment.ProcessId
            let path = Paths.socketPath $"worker-{Environment.ProcessId}-{Guid.NewGuid().ToString().Substring(0, 8)}"

            if File.Exists path then
                File.Delete path

            use listener = new Socket(AddressFamily.Unix, SocketType.Stream, ProtocolType.Unspecified)
            listener.Bind(UnixDomainSocketEndPoint path)
            listener.Listen 1

            let info =
                ProcessStartInfo(
                    command.File,
                    UseShellExecute = false,
                    RedirectStandardOutput = true,
                    RedirectStandardError = true,
                    CreateNoWindow = true
                )

            for a in command.Arguments @ [ "--worker"; path; string app ] do
                info.ArgumentList.Add a

            let log = StringBuilder()
            let proc = new Process(StartInfo = info)
            let append (e: DataReceivedEventArgs) = if not (isNull e.Data) then lock log (fun () -> log.AppendLine e.Data |> ignore)
            proc.OutputDataReceived.Add append
            proc.ErrorDataReceived.Add append
            proc.Start() |> ignore
            proc.BeginOutputReadLine()
            proc.BeginErrorReadLine()

            use timeout = new CancellationTokenSource(WorkerProcess.ConnectTimeout)
            let accept = listener.AcceptAsync(timeout.Token).AsTask()
            let! first = Task.WhenAny(accept, proc.WaitForExitAsync(timeout.Token))

            try
                File.Delete path
            with _ ->
                ()

            if not (obj.ReferenceEquals(first, accept)) || not accept.IsCompletedSuccessfully then
                try
                    proc.Kill true
                with _ ->
                    ()

                let said = lock log (fun () -> log.ToString().Trim())
                return failwith $"the worker did not connect: {said}"
            else
                let connection = new Connection(new NetworkStream(accept.Result, true))
                let channel = Channel.CreateUnbounded<Reply>(UnboundedChannelOptions(SingleWriter = true))

                let pump () : Task =
                    task {
                        let mutable reading = true

                        try
                            while reading do
                                match! connection.ReceiveAsync CancellationToken.None with
                                | Some bytes -> channel.Writer.TryWrite(Codec.decodeReply bytes) |> ignore
                                | None -> reading <- false

                            channel.Writer.TryComplete() |> ignore
                        with
                        | :? ProtocolException as e -> channel.Writer.TryComplete e |> ignore
                        | _ -> channel.Writer.TryComplete() |> ignore
                    }

                Task.Run(Func<Task> pump) |> ignore
                return new WorkerProcess(proc, connection, channel.Reader, log)
        }

/// The worker on duty and a warm spare, with `Init` done, behind it. Cancelling a script ends the
/// worker running it and puts the spare on duty; a worker whose heap has grown past a limit is
/// replaced the same way once it is idle, and so is one that dies. Replies come from the worker on
/// duty only.
///
/// The figure on screen came from the last script that ran to the end, and a worker put on duty
/// does not hold it. A worn-out worker's replacement runs that script again at once. After a
/// cancel or a death it is run again when the figure is next asked for, a control, a zoom, the
/// pointer, a selection or an export, before that request; a new script sent first makes it
/// unnecessary. Either way what it prints is not passed on, since it was once already.
type Workers(command: Command, init: Init, heapLimit: int64) =

    let replies = Channel.CreateUnbounded<Reply>()
    let gate = obj ()
    let mutable current: (WorkerProcess * Ready) option = None
    let mutable spare: Task<WorkerProcess * Ready> = null
    let mutable disposed = false
    /// The spare being put on duty, which a request sent meanwhile waits for.
    let mutable promoting: Task = Task.CompletedTask

    // What a worker put on duty is told again: the pane's size, the last script that ran to the
    // end, and the figure on screen.
    let mutable lastSize: LiveInput option = None
    let mutable lastGood: Evaluate option = None
    let mutable selected = -1
    /// The scripts sent and not yet reported, oldest first, each with whether its output is held
    /// back. The worker runs them in order, so the oldest is the one running.
    let runs = Collections.Generic.Queue<Evaluate * bool>()
    /// The worker on duty lacks the figure on screen, and gets it on the next request for it.
    let mutable stale = false
    let mutable recycleDue = false

    let start () : Task<WorkerProcess * Ready> =
        task {
            let! w = WorkerProcess.StartAsync command
            do! w.SendAsync(Request.Init init)
            let! first = w.Replies.ReadAsync().AsTask()

            match first with
            | Reply.Ready r -> return w, r
            | Reply.Failed message ->
                w.Kill()
                return failwith message
            | other ->
                w.Kill()
                return failwith $"the worker answered Init with {other}"
        }

    /// Sends requests to one worker in order.
    let sendAll (w: WorkerProcess) (requests: Request list) =
        task {
            for r in requests do
                do! w.SendAsync r
        }
        |> ignore

    let rec forward (w: WorkerProcess) : Task =
        task {
            let mutable reading = true

            while reading do
                let! more = w.Replies.WaitToReadAsync().AsTask()

                if not more then
                    reading <- false
                else
                    let mutable reply = Unchecked.defaultof<Reply>

                    while w.Replies.TryRead &reply do
                        lock gate (fun () ->
                            match current with
                            | Some(c, ready) when obj.ReferenceEquals(c, w) ->
                                let held =
                                    match reply with
                                    | Reply.Output _ -> runs.Count > 0 && snd (runs.Peek())
                                    | Reply.Evaluated e ->
                                        if runs.Count > 0 then
                                            let ran, _ = runs.Dequeue()
                                            if e.Ok then lastGood <- Some ran

                                        selected <- e.Selected
                                        if e.Heap > ready.Heap + heapLimit then recycleDue <- true
                                        false
                                    | _ -> false

                                if not held then replies.Writer.TryWrite reply |> ignore
                                if recycleDue && runs.Count = 0 then recycle ()
                            | _ -> ())

            // The worker went away. If it was on duty, nobody asked it to.
            lock gate (fun () ->
                match current with
                | Some(c, _) when obj.ReferenceEquals(c, w) && not disposed ->
                    let said = w.Log.Trim()
                    let tail = if said.Length > 2000 then said.Substring(said.Length - 2000) else said
                    let message = $"the worker stopped. {tail}".TrimEnd()
                    replies.Writer.TryWrite(Reply.Failed message) |> ignore
                    promote false |> ignore
                | _ -> ())
        }

    /// Puts the spare on duty and starts the next spare. Called holding `gate`.
    and promote (replay: bool) : Task =
        let next = spare
        spare <- start ()

        match current with
        | Some(old, _) -> old.Kill()
        | None -> ()

        current <- None
        runs.Clear()
        recycleDue <- false
        stale <- not replay && lastGood.IsSome

        promoting <-
            task {
                let! w, ready = next

                lock gate (fun () ->
                    current <- Some(w, ready)

                    let again =
                        [ match lastSize with
                          | Some s -> Request.Live s
                          | None -> ()
                          if replay then
                              match lastGood with
                              | Some e ->
                                  runs.Enqueue(e, true)
                                  Request.Evaluate e
                                  if selected > 0 then Request.Select selected
                              | None -> () ]

                    sendAll w again)

                forward w |> ignore
            }

        promoting

    /// Replaces a worn-out worker, once the spare is ready. Called holding `gate`.
    and recycle () =
        if not (isNull spare) && spare.IsCompletedSuccessfully then
            promote true |> ignore

    new(command: Command, init: Init) = new Workers(command, init, 200L * 1024L * 1024L)

    /// Starts the worker on duty and its spare, and returns once the one on duty is ready.
    member _.StartAsync() : Task =
        task {
            let first = start ()
            spare <- start ()
            let! w, ready = first
            lock gate (fun () -> current <- Some(w, ready))
            forward w |> ignore
        }

    /// What the worker on duty says.
    member _.Replies: ChannelReader<Reply> = replies.Reader

    /// The worker on duty's process id, or none while one is being put on duty.
    member _.Pid = lock gate (fun () -> current |> Option.map (fun (w, _) -> w.Pid))

    /// Sends a request to the worker on duty, or, while a spare is being put on duty, to that one
    /// once it is; after the last good script again, if the request needs the figure it drew.
    member _.SendAsync(request: Request) : Task =
        let target, pending, restore =
            lock gate (fun () ->
                let restore =
                    match request with
                    | Request.Evaluate e ->
                        runs.Enqueue(e, false)
                        stale <- false
                        []
                    // A new size is not a request for the figure: the worker keeps it for the next.
                    | Request.Live l when l.Kind = "size" ->
                        lastSize <- Some l
                        []
                    | _ when stale ->
                        stale <- false

                        match lastGood with
                        | Some e ->
                            runs.Enqueue(e, true)

                            [ Request.Evaluate e
                              match request with
                              | Request.Select _ -> ()
                              | _ -> if selected > 0 then Request.Select selected ]
                        | None -> []
                    | _ -> []

                match request with
                | Request.Select i -> selected <- i
                | _ -> ()

                current, promoting, restore)

        match target, restore with
        | Some(w, _), [] -> w.SendAsync request
        | _ ->
            task {
                do! pending

                match lock gate (fun () -> current) with
                | Some(w, _) ->
                    for r in restore @ [ request ] do
                        do! w.SendAsync r
                | None -> raise (InvalidOperationException "no worker is on duty")
            }

    /// Ends the worker on duty, whatever it is doing, and puts the spare in its place. Returns
    /// once the spare is on duty.
    member _.CancelAsync() : Task = lock gate (fun () -> promote false)

    interface IDisposable with
        member _.Dispose() =
            lock gate (fun () ->
                disposed <- true

                match current with
                | Some(w, _) -> w.Kill()
                | None -> ()

                current <- None)

            if not (isNull spare) then
                spare.ContinueWith(fun (t: Task<WorkerProcess * Ready>) -> if t.IsCompletedSuccessfully then (fst t.Result).Kill())
                |> ignore
