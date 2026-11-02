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

/// A role this app starts itself in, and the messages it takes and gives.
type Role<'request, 'reply> =
    { Flag: string
      Encode: 'request -> byte[]
      Decode: byte[] -> 'reply }

module Role =

    /// `tambour --worker`, which runs scripts.
    let worker: Role<Request, Reply> =
        { Flag = "--worker"
          Encode = Codec.encodeRequest
          Decode = Codec.decodeReply }

    /// `tambour --analysis`, the language service.
    let analysis: Role<AnalysisRequest, AnalysisReply> =
        { Flag = "--analysis"
          Encode = Codec.encodeAnalysisRequest
          Decode = Codec.decodeAnalysisReply }

module ProcessTree =

    /// The processes `pid` started that are still its own, as Linux lists them for each thread.
    let private childrenOf (pid: int) : int[] =
        try
            Directory.GetDirectories $"/proc/{pid}/task"
            |> Array.collect (fun task ->
                try
                    File.ReadAllText(Path.Combine(task, "children")).Split(' ', StringSplitOptions.RemoveEmptyEntries)
                with _ ->
                    [||])
            |> Array.map int
        with _ ->
            [||]

    /// Ends `proc` and everything it started. Not `Process.Kill true` on Linux: that stops each
    /// process before it kills it, and a process whose killer exits in between stays stopped for
    /// good, its watch on the app stopped with it.
    let rec kill (proc: Process) =
        if OperatingSystem.IsLinux() then
            // Its children are read while they are still its own; once it has gone, they are not.
            let children = childrenOf proc.Id
            let started = try proc.StartTime with _ -> DateTime.MaxValue

            try
                proc.Kill()
            with _ ->
                ()

            for pid in children do
                try
                    use child = Process.GetProcessById pid

                    // Not another process that has since taken the number.
                    if child.StartTime >= started then
                        kill child
                with _ ->
                    ()
        else
            try
                proc.Kill true
            with _ ->
                ()

/// One process of this app in another role, and the connection to it.
type ChildProcess<'request, 'reply> private
    (role: Role<'request, 'reply>, proc: Process, connection: Connection, replies: ChannelReader<'reply>, log: StringBuilder) =

    let exited = proc.WaitForExitAsync()

    /// How long a process has to connect before it is given up on.
    static member ConnectTimeout = TimeSpan.FromSeconds 60.0

    member _.Pid = proc.Id

    /// What the worker said, in order. The reader completes when the worker goes away.
    member _.Replies = replies

    /// What the process wrote to its own stdout and stderr, outside the protocol: a crash's
    /// message, a runtime's complaint.
    member _.Log = lock log (fun () -> log.ToString())

    member _.SendAsync(request: 'request) : Task =
        connection.SendAsync(role.Encode request, CancellationToken.None)

    /// Ends the process and everything it started. Its replies end with it.
    member _.Kill() =
        ProcessTree.kill proc
        (connection :> IDisposable).Dispose()

    /// Completes when the process has ended.
    member _.Exited: Task = exited

    /// Starts this app in the role and waits for it to connect. It ends when the process `app`
    /// does, this one unless stated.
    static member StartAsync(role: Role<'request, 'reply>, command: Command, ?app: int) : Task<ChildProcess<'request, 'reply>> =
        task {
            let app = defaultArg app Environment.ProcessId
            let path = Paths.socketPath $"{role.Flag.TrimStart('-')}-{Environment.ProcessId}-{Guid.NewGuid().ToString().Substring(0, 8)}"

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

            for a in command.Arguments @ [ role.Flag; path; string app ] do
                info.ArgumentList.Add a

            let log = StringBuilder()
            let proc = new Process(StartInfo = info)
            let append (e: DataReceivedEventArgs) = if not (isNull e.Data) then lock log (fun () -> log.AppendLine e.Data |> ignore)
            proc.OutputDataReceived.Add append
            proc.ErrorDataReceived.Add append
            proc.Start() |> ignore
            proc.BeginOutputReadLine()
            proc.BeginErrorReadLine()

            use timeout = new CancellationTokenSource(ChildProcess<'request, 'reply>.ConnectTimeout)
            let accept = listener.AcceptAsync(timeout.Token).AsTask()
            let! first = Task.WhenAny(accept, proc.WaitForExitAsync(timeout.Token))

            try
                File.Delete path
            with _ ->
                ()

            if not (obj.ReferenceEquals(first, accept)) || not accept.IsCompletedSuccessfully then
                ProcessTree.kill proc

                let said = lock log (fun () -> log.ToString().Trim())
                return failwith $"the {role.Flag.TrimStart('-')} did not connect: {said}"
            else
                let connection = new Connection(new NetworkStream(accept.Result, true))
                let channel = Channel.CreateUnbounded<'reply>(UnboundedChannelOptions(SingleWriter = true))

                let pump () : Task =
                    task {
                        let mutable reading = true

                        try
                            while reading do
                                match! connection.ReceiveAsync CancellationToken.None with
                                | Some bytes -> channel.Writer.TryWrite(role.Decode bytes) |> ignore
                                | None -> reading <- false

                            channel.Writer.TryComplete() |> ignore
                        with
                        | :? ProtocolException as e -> channel.Writer.TryComplete e |> ignore
                        | _ -> channel.Writer.TryComplete() |> ignore
                    }

                Task.Run(Func<Task> pump) |> ignore
                return new ChildProcess<'request, 'reply>(role, proc, connection, channel.Reader, log)
        }

/// A worker process.
type WorkerProcess = ChildProcess<Request, Reply>

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
    /// The tables sent, by name, the latest of each: a worker put on duty gets them first.
    let tables = Collections.Generic.Dictionary<string, Request>()
    let mutable lastGood: Evaluate option = None
    let mutable selected = -1
    /// The scripts sent and not yet reported, oldest first, each with whether its output is held
    /// back. The worker runs them in order, so the oldest is the one running.
    let runs = Collections.Generic.Queue<Evaluate * bool>()
    /// The worker on duty lacks the figure on screen, and gets it on the next request for it.
    let mutable stale = false
    let mutable recycleDue = false

    /// Starts a worker, on the thread pool: not on the thread that asked, whose dispatcher may be
    /// gone before the worker is ready (a window closed at once, a test over), leaving it running
    /// with no one to end it.
    let start () : Task<WorkerProcess * Ready> =
        Task.Run<WorkerProcess * Ready>(fun () -> task {
            let! w = WorkerProcess.StartAsync(Role.worker, command)
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
        })

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
                        [ yield! tables.Values
                          match lastSize with
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
        Task.Run<unit>(fun () -> task {
            let first = start ()
            spare <- start ()
            let! w, ready = first

            // Disposed while it started (a tab closed at once): the worker goes with it rather
            // than going on duty for no one.
            let kept =
                lock gate (fun () ->
                    if not disposed then current <- Some(w, ready)
                    not disposed)

            if kept then forward w |> ignore else w.Kill()
        })

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
                    // Columns are not a request for the figure either.
                    | Request.Table t ->
                        tables.[t.Name] <- request
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

            // A spare already started is killed here, before the app can exit, not on another thread.
            if not (isNull spare) then
                spare.ContinueWith(
                    (fun (t: Task<WorkerProcess * Ready>) -> if t.IsCompletedSuccessfully then (fst t.Result).Kill()),
                    TaskContinuationOptions.ExecuteSynchronously
                )
                |> ignore

/// A workspace's workers, started when it first runs a script and ended when it drops them, so
/// that a tab left in the background holds no process. Each time they start they are given what
/// the workspace has (its tables, from `replay`, and the figure's size) and, after a drop, the
/// last script run, before the request that started them. A request about the figure starts them
/// only where a script has run; before any has, there is nothing for it to act on, and it is
/// set aside. Their replies come through one reader however many times they were started. Their replies come through
/// one reader however many times they were started.
type LazyWorkers(command: Command, init: Init, replay: unit -> Request list) =
    let gate = obj ()
    let replies = Channel.CreateUnbounded<Reply>()
    let mutable current: (Workers * Task * CancellationTokenSource) option = None
    let mutable lastRun: Request option = None
    /// The figure's size, as last sent, given to workers as they start.
    let mutable lastSize: Request option = None
    let mutable disposed = false
    /// When they were last asked something or last answered.
    let mutable used = DateTime.UtcNow

    /// The workers, started if there are none: with the start's task, and whether they are new.
    let ensure () =
        lock gate (fun () ->
            match current with
            | Some(w, started, _) -> w, started, false
            | None ->
                let w = new Workers(command, init)
                let started = w.StartAsync()
                let stop = new CancellationTokenSource()

                // Their replies on to the workspace's reader, until they are dropped.
                task {
                    try
                        do! started

                        while true do
                            let! reply = w.Replies.ReadAsync(stop.Token).AsTask()
                            used <- DateTime.UtcNow
                            replies.Writer.TryWrite reply |> ignore
                    with _ ->
                        ()
                }
                |> ignore

                current <- Some(w, started, stop)
                w, started, true)

    /// Sends one request, once the one before it has gone: a start's tables and figure size reach
    /// the workers before any request made while they were starting.
    let sendOne (request: Request) : Task =
        task {
            used <- DateTime.UtcNow

            match request with
            | Request.Live l when l.Kind = "size" -> lastSize <- Some request
            | _ -> ()

            let starts =
                match request with
                | Request.Evaluate _ -> true
                | _ -> lastRun.IsSome || lock gate (fun () -> current.IsSome)

            if not disposed && starts then
                let w, started, fresh = ensure ()
                do! started

                if fresh then
                    for r in replay () @ Option.toList lastSize do
                        if not (obj.ReferenceEquals(r, request)) then do! w.SendAsync r

                    match request, lastRun with
                    | Request.Evaluate _, _
                    | _, None -> ()
                    | _, Some run -> do! w.SendAsync run

                match request with
                | Request.Evaluate _ -> lastRun <- Some request
                | _ -> ()

                do! w.SendAsync request
        }

    let mutable sending: Task = Task.CompletedTask

    /// The start of the workers there are, or nothing to wait for while there are none.
    member _.Started: Task =
        lock gate (fun () ->
            match current with
            | Some(_, started, _) -> started
            | None -> Task.CompletedTask)

    /// Whether there are workers now.
    member _.Live = lock gate (fun () -> current.IsSome)

    member _.Replies: ChannelReader<Reply> = replies.Reader

    /// How long since they were last asked something or last answered.
    member _.Idle = DateTime.UtcNow - used

    member _.SendAsync(request: Request) : Task =
        lock gate (fun () ->
            let next = sending.ContinueWith((fun (_: Task) -> sendOne request), TaskScheduler.Default).Unwrap()
            sending <- next
            next)

    member _.CancelAsync() : Task =
        match lock gate (fun () -> current) with
        | Some(w, _, _) -> w.CancelAsync()
        | None -> Task.CompletedTask

    /// Ends the workers, if there are any; the next request starts them again.
    member _.Drop() =
        match lock gate (fun () -> let c = current in current <- None; c) with
        | Some(w, _, stop) ->
            stop.Cancel()
            (w :> IDisposable).Dispose()
        | None -> ()

    interface IDisposable with
        member this.Dispose() =
            disposed <- true
            this.Drop()

/// The language service, as the editor asks it: one `tambour --analysis` process, started at once
/// and again if it dies, each request answered by the reply with its id.
type LanguageService(command: Command, init: AnalysisInit) =

    let gate = obj ()
    let pending = Collections.Generic.Dictionary<int, TaskCompletionSource<AnalysisReply>>()
    let mutable next = 0
    let mutable disposed = false

    let failAll (reason: string) =
        lock gate (fun () ->
            for t in pending.Values do
                t.TrySetException(InvalidOperationException reason) |> ignore

            pending.Clear())

    /// Starts the process, on the thread pool, as a worker is: its replies are read there rather
    /// than on the dispatcher of whoever made the service, which may not be the one running.
    let start () : Task<ChildProcess<AnalysisRequest, AnalysisReply>> =
        Task.Run<ChildProcess<AnalysisRequest, AnalysisReply>>(fun () -> task {
            let! p = ChildProcess<AnalysisRequest, AnalysisReply>.StartAsync(Role.analysis, command)
            do! p.SendAsync(AnalysisRequest.Start init)

            match! p.Replies.ReadAsync().AsTask() with
            | AnalysisReply.Started _ -> ()
            | AnalysisReply.Failed(_, message) ->
                p.Kill()
                failwith message
            | other ->
                p.Kill()
                failwith $"the language service answered Start with {other}"

            let pump () : Task =
                task {
                    let mutable reading = true

                    while reading do
                        let! more = p.Replies.WaitToReadAsync().AsTask()

                        if not more then
                            reading <- false
                        else
                            let mutable reply = Unchecked.defaultof<AnalysisReply>

                            while p.Replies.TryRead &reply do
                                let id =
                                    match reply with
                                    | AnalysisReply.Checked r -> r.Id
                                    | AnalysisReply.Completions r -> r.Id
                                    | AnalysisReply.Hovered r -> r.Id
                                    | AnalysisReply.Signatures r -> r.Id
                                    | AnalysisReply.Used r -> r.Id
                                    | AnalysisReply.Actions r -> r.Id
                                    | AnalysisReply.Acted r -> r.Id
                                    | AnalysisReply.Plotted r -> r.Id
                                    | AnalysisReply.Failed(id, _) -> id
                                    | AnalysisReply.Started _ -> -1

                                let waiting =
                                    lock gate (fun () ->
                                        match pending.TryGetValue id with
                                        | true, t ->
                                            pending.Remove id |> ignore
                                            Some t
                                        | _ -> None)

                                match waiting, reply with
                                | Some t, AnalysisReply.Failed(_, message) -> t.TrySetException(InvalidOperationException message) |> ignore
                                | Some t, _ -> t.TrySetResult reply |> ignore
                                | None, _ -> ()

                    failAll $"the language service stopped. {p.Log.Trim()}"
                }

            pump () |> ignore
            return p
        })

    let mutable child = start ()

    /// The process, started again if the last one stopped.
    let current () =
        lock gate (fun () ->
            if child.IsFaulted || (child.IsCompletedSuccessfully && child.Result.Exited.IsCompleted) then
                child <- start ()

            child)

    /// Sends a request built around a fresh id, and waits for its reply.
    member _.AskAsync(make: int -> AnalysisRequest) : Task<AnalysisReply> =
        task {
            let! p = current ()
            let id = Threading.Interlocked.Increment &next
            let reply = TaskCompletionSource<AnalysisReply>(TaskCreationOptions.RunContinuationsAsynchronously)
            lock gate (fun () -> pending.[id] <- reply)
            do! p.SendAsync(make id)
            return! reply.Task
        }

    /// Started and ready.
    member _.Ready: Task = child :> Task

    member this.CheckAsync(text: string, path: string) : Task<Checked> =
        task {
            match! this.AskAsync(fun id -> AnalysisRequest.Check { Id = id; Text = text; Path = path }) with
            | AnalysisReply.Checked c -> return c
            | other -> return failwith $"a check answered with {other}"
        }

    member this.CompleteAsync(text: string, path: string, line: int, column: int) : Task<Completions> =
        task {
            match! this.AskAsync(fun id -> AnalysisRequest.Complete { Id = id; Text = text; Path = path; Line = line; Column = column }) with
            | AnalysisReply.Completions c -> return c
            | other -> return failwith $"a completion answered with {other}"
        }

    member this.HoverAsync(text: string, path: string, line: int, column: int) : Task<Hovered> =
        task {
            match! this.AskAsync(fun id -> AnalysisRequest.Hover { Id = id; Text = text; Path = path; Line = line; Column = column }) with
            | AnalysisReply.Hovered h -> return h
            | other -> return failwith $"a hover answered with {other}"
        }

    member this.DescribeAsync(text: string, path: string, line: int, column: int, name: string) : Task<Hovered> =
        task {
            match! this.AskAsync(fun id -> AnalysisRequest.Describe { Id = id; Text = text; Path = path; Line = line; Column = column; Name = name }) with
            | AnalysisReply.Hovered h -> return h
            | other -> return failwith $"a description answered with {other}"
        }

    member this.SignatureAsync(text: string, path: string, line: int, column: int) : Task<Signatures> =
        task {
            match! this.AskAsync(fun id -> AnalysisRequest.Signature { Id = id; Text = text; Path = path; Line = line; Column = column }) with
            | AnalysisReply.Signatures s -> return s
            | other -> return failwith $"signature help answered with {other}"
        }

    member this.UsesAsync(text: string, path: string, line: int, column: int) : Task<Used> =
        task {
            match! this.AskAsync(fun id -> AnalysisRequest.Uses { Id = id; Text = text; Path = path; Line = line; Column = column }) with
            | AnalysisReply.Used u -> return u
            | other -> return failwith $"uses answered with {other}"
        }

    member this.ActionsAsync(text: string, path: string, figure: string) : Task<ActionsFor> =
        task {
            match! this.AskAsync(fun id -> AnalysisRequest.Actions { Id = id; Text = text; Path = path; Figure = figure }) with
            | AnalysisReply.Actions a -> return a
            | other -> return failwith $"the actions answered with {other}"
        }

    member this.ActAsync(text: string, path: string, figure: string, action: ActionSpec) : Task<Acted> =
        task {
            match! this.AskAsync(fun id -> AnalysisRequest.Act { Id = id; Text = text; Path = path; Figure = figure; Action = action }) with
            | AnalysisReply.Acted a -> return a
            | other -> return failwith $"an action answered with {other}"
        }

    member this.PlottedAsync(text: string, path: string, figure: string) : Task<PlottedFields> =
        task {
            match! this.AskAsync(fun id -> AnalysisRequest.Plotted { Id = id; Text = text; Path = path; Figure = figure }) with
            | AnalysisReply.Plotted p -> return p
            | other -> return failwith $"plotted answered with {other}"
        }

    interface IDisposable with
        member _.Dispose() =
            disposed <- true
            failAll "the language service was closed"

            // A process already started is killed here, before the app can exit, not on another thread.
            child.ContinueWith(
                (fun (t: Task<ChildProcess<AnalysisRequest, AnalysisReply>>) -> if t.IsCompletedSuccessfully then t.Result.Kill()),
                TaskContinuationOptions.ExecuteSynchronously
            )
            |> ignore
