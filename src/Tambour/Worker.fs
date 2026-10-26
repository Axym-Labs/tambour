/// `tambour --worker <socket>`: the process that runs scripts. It holds an F# interactive
/// session and the figures the last run bound, drives the one on screen through Marey's `Live`,
/// and sends its scenes back in Marey's wire format. The app never holds a figure.
module Tambour.Worker

open System
open System.Diagnostics
open System.Globalization
open System.IO
open System.Net.Sockets
open System.Text
open Marey
open Tambour.Protocol

/// Text written to the console, sent to the app a line at a time; what is left without a newline
/// goes when the run ends.
type private Streamed(stream: string, send: Reply -> unit) =
    inherit TextWriter()

    let pending = StringBuilder()

    let sendThrough (length: int) =
        if length > 0 then
            send (Reply.Output { Stream = stream; Text = pending.ToString(0, length) })
            pending.Remove(0, length) |> ignore

    let sendLines () =
        let mutable last = pending.Length - 1

        while last >= 0 && pending.[last] <> '\n' do
            last <- last - 1

        sendThrough (last + 1)

    override _.Encoding = Encoding.UTF8

    override _.Write(c: char) =
        lock pending (fun () ->
            pending.Append c |> ignore
            if c = '\n' then sendLines ())

    override _.Write(s: string) =
        if not (String.IsNullOrEmpty s) then
            lock pending (fun () ->
                pending.Append s |> ignore
                if s.Contains '\n' then sendLines ())

    override _.Write(buffer: char[], index: int, count: int) =
        if count > 0 then
            lock pending (fun () ->
                pending.Append(buffer, index, count) |> ignore
                if Array.IndexOf(buffer, '\n', index, count) >= 0 then sendLines ())

    override _.Flush() = lock pending (fun () -> sendThrough pending.Length)

/// The figure on screen and where the reader has taken it.
type private Shown =
    { Index: int
      Live: Live
      mutable State: LiveState
      mutable Scene: Node
      mutable Readout: ReadoutState option }

/// A live figure's scene is laid out by `Live` against Marey's own measurer, so its text is
/// anchored against that one, and the scene carries that measurer's metrics.
let private measurer = TextMeasure.approx

let private controlsOf (index: int) (live: Live) (state: LiveState) : ControlsState =
    let info (spec: ControlSpec) (value: float) =
        let blank =
            { Label = spec.Label
              Kind = ""
              Lo = 0.0
              Hi = 0.0
              Step = 0.0
              Samples = 0
              Loop = false
              Options = [||]
              Init = spec.Init
              Value = value
              Shown = (try spec.Show value with e -> e.Message) }

        match spec.Kind with
        | Slider(lo, hi, step) -> { blank with Kind = "slider"; Lo = lo; Hi = hi; Step = step }
        | Select options -> { blank with Kind = "select"; Options = options }
        | Checkbox -> { blank with Kind = "checkbox" }
        | Time(lo, hi, samples, loop) -> { blank with Kind = "time"; Lo = lo; Hi = hi; Samples = samples; Loop = loop }

    { Figure = index
      Controls = Array.map2 info live.Controls state.Values }

let private readoutOf (index: int) (live: Live) (state: LiveState) : ReadoutState =
    match Live.readout live state with
    | Some r ->
        { Figure = index
          Visible = true
          X0 = float r.Plot.X0
          Y0 = float r.Plot.Y0
          X1 = float r.Plot.X1
          Y1 = float r.Plot.Y1
          Right = r.Right
          Lines = r.Lines }
    | None ->
        { Figure = index
          Visible = false
          X0 = 0.0
          Y0 = 0.0
          X1 = 0.0
          Y1 = 0.0
          Right = false
          Lines = [||] }

let private payload (args: float[]) =
    "[" + String.Join(",", args |> Array.map (fun a -> a.ToString("R", CultureInfo.InvariantCulture))) + "]"

let private sinceStart () =
    (DateTime.Now - Process.GetCurrentProcess().StartTime).TotalMilliseconds

type private Worker(send: Reply -> unit, flush: unit -> unit) =
    let mutable session: Evaluation.Session option = None
    let mutable skia: SkiaSettings = { Dpi = float Space.screenDpi; FontFiles = [||] }
    let mutable figures: Evaluation.Found list = []
    let mutable shown: Shown option = None
    /// The size the app last gave, kept for the next figure.
    let mutable size = Live.DefaultSize

    /// A figure laid out at the current size: its first state and its scene.
    let layOut (index: int) (found: Evaluation.Found) =
        let state = Live.initAt size found.Live
        { Index = index
          Live = found.Live
          State = state
          Scene = Live.scene found.Live state
          Readout = None }

    let present (s: Shown) =
        send (Reply.Controls(controlsOf s.Index s.Live s.State))
        send (Reply.Scene(s.Index, Wire.encodeWith measurer s.Scene))

    let evaluated ok exn (diagnostics: Diagnostic[]) (timings: Timings) =
        flush ()

        send (
            Reply.Evaluated
                { Ok = ok
                  Figures = figures |> List.map (fun f -> { Name = f.Name; Controls = f.Live.Controls.Length }) |> Array.ofList
                  Selected = (match shown with Some s -> s.Index | None -> -1)
                  Diagnostics = diagnostics
                  Exception = Option.toObj exn
                  Timings = timings
                  Heap = GC.GetTotalMemory false }
        )

    let init (m: Init) =
        skia <- m.Skia
        let s = Evaluation.Session(m.References)
        let warm = s.Start m.Prelude
        // The path every run takes, once, so that the first run does not compile it.
        let state = Live.initAt size warm
        let node = Live.scene warm state
        Wire.encodeWith measurer node |> ignore
        Wire.encodePatchWith measurer (Scene.diff node node) |> ignore
        session <- Some s
        send (Reply.Ready { Elapsed = sinceStart (); Heap = GC.GetTotalMemory true })

    let evaluate (m: Evaluate) (s: Evaluation.Session) =
        let run = s.Run(m.Text, m.Path)
        let clock = Stopwatch.StartNew()

        // The figure that was on screen stays on screen by name, if the run still binds it.
        let selected =
            match shown with
            | Some old ->
                let name = (List.item old.Index figures).Name
                run.Found |> List.tryFindIndex (fun f -> f.Name = name) |> Option.defaultValue 0
            | None -> 0

        let laidOut =
            if not run.Ok || run.Found.IsEmpty then
                Ok None
            else
                try
                    Ok(Some(layOut selected (List.item selected run.Found)))
                with e ->
                    Error $"{(List.item selected run.Found).Name}: {e.Message}"

        let layout = clock.Elapsed.TotalMilliseconds
        clock.Restart()

        let scene =
            match laidOut with
            | Ok(Some s) -> Some(Wire.encodeWith measurer s.Scene)
            | _ -> None

        let timings =
            { Evaluate = run.Elapsed.TotalMilliseconds
              Layout = layout
              Encode = clock.Elapsed.TotalMilliseconds }

        match laidOut with
        | Error message ->
            // A figure that throws while it is laid out fails the run, as a script that throws does.
            evaluated false (Some message) run.Diagnostics timings
        | Ok next when run.Ok ->
            figures <- run.Found
            shown <- next
            evaluated true None run.Diagnostics timings

            match next, scene with
            | Some s, Some bytes ->
                send (Reply.Controls(controlsOf s.Index s.Live s.State))
                send (Reply.Scene(s.Index, bytes))
            | _ -> ()
        | Ok _ -> evaluated false run.Exception run.Diagnostics timings

    let select (index: int) =
        match List.tryItem index figures with
        | None -> send (Reply.Failed $"there is no figure {index}; the last run bound {figures.Length}")
        | Some found ->
            let s = layOut index found
            shown <- Some s
            present s

    let live (m: LiveInput) =
        match shown, LiveMsg.parse m.Kind (payload m.Args) with
        | Some s, Some msg ->
            let before = s.State
            let after = Live.step s.Live before msg
            s.State <- after
            size <- after.Size

            if not (Live.drawsAlike before after) then
                let node = Live.scene s.Live after
                let patch = Scene.diff s.Scene node
                s.Scene <- node
                send (Reply.Patch(s.Index, Wire.encodePatchWith measurer patch))

            if not (obj.ReferenceEquals(before.Values, after.Values)) then
                send (Reply.Controls(controlsOf s.Index s.Live after))

            let readout = readoutOf s.Index s.Live after

            let changed =
                match s.Readout with
                | Some r -> r.Visible <> readout.Visible || (readout.Visible && r <> readout)
                | None -> readout.Visible

            if changed then
                s.Readout <- Some readout
                send (Reply.Readout readout)
        // The pane's size, before there is a figure to draw at it.
        | None, Some(Resize(width, height)) when width > 0.0 && height > 0.0 && Double.IsFinite width && Double.IsFinite height ->
            size <- { Width = width * 1.0<px>; Height = height * 1.0<px> }
        // A message for no figure, or one this Marey does not know, is ignored, as Marey's own
        // hosts ignore a view one protocol version ahead.
        | _ -> ()

    let export (m: Export) =
        match shown with
        | None -> send (Reply.Failed "there is no figure on screen to export")
        | Some s ->
            let extent: Extent<px> = { Width = m.Width * 1.0<px>; Height = m.Height * 1.0<px> }
            let figure = Live.view s.Live s.State

            let skiaScene () =
                use skiaMeasurer = new SkiaTextMeasurer(dpi = skia.Dpi * 1.0<px / mm>, fontFiles = List.ofArray skia.FontFiles)
                Chart.toSceneWith skiaMeasurer extent figure

            let options () =
                { SkiaOptions.Default with
                    Dpi = skia.Dpi * 1.0<px / mm>
                    FontFiles = List.ofArray skia.FontFiles }

            match m.Format with
            | "png" -> send (Reply.Exported(s.Index, SkiaBackend.renderPngWith (options ()) extent (skiaScene ())))
            | "pdf" -> send (Reply.Exported(s.Index, SkiaBackend.renderPdfWith (options ()) extent (skiaScene ())))
            | "svg" -> send (Reply.Exported(s.Index, Encoding.UTF8.GetBytes(SvgBackend.render extent (Chart.toScene extent figure))))
            | other -> send (Reply.Failed $"no export writes {other}; png, pdf and svg do")

    /// Carries out one request. False when the worker cannot go on.
    member _.Handle(request: Request) : bool =
        match request, session with
        | Request.Init m, None ->
            try
                init m
                true
            with e ->
                send (Reply.Failed $"the worker did not start: {e.Message}")
                false
        | Request.Init _, Some _ ->
            send (Reply.Failed "the worker has already started")
            true
        | _, None ->
            send (Reply.Failed "the worker has not been started")
            true
        | request, Some s ->
            try
                match request with
                | Request.Evaluate m -> evaluate m s
                | Request.Select index -> select index
                | Request.Live m -> live m
                | Request.Export m -> export m
                | Request.Init _ -> ()
            with e ->
                flush ()
                send (Reply.Failed e.Message)

            true

/// Ends this process when the app's does. A worker stuck in a script never reads its socket
/// again, so it would not otherwise notice the app had gone.
let private outlive (app: int) =
    let watch () =
        try
            use p = Process.GetProcessById app
            p.WaitForExit()
        with _ ->
            ()

        Environment.Exit 0

    Threading.Thread(watch, IsBackground = true, Name = "app watch").Start()

/// Connects to the app's socket and answers it until it closes or the app's process `app` ends.
/// The exit code is 0 when the app went away, 1 when the worker could not start.
let run (socketPath: string) (app: int) : int =
    outlive app
    use socket = new Socket(AddressFamily.Unix, SocketType.Stream, ProtocolType.Unspecified)
    socket.Connect(UnixDomainSocketEndPoint socketPath)
    use connection = new Connection(new NetworkStream(socket, true))
    let send (reply: Reply) = connection.Send(Codec.encodeReply reply)
    let out = new Streamed("out", send)
    let err = new Streamed("err", send)
    Console.SetOut out
    Console.SetError err

    let flush () =
        Console.Out.Flush()
        Console.Error.Flush()

    let worker = Worker(send, flush)

    let rec loop () =
        match connection.Receive() with
        | None -> 0
        | Some bytes ->
            match (try Ok(Codec.decodeRequest bytes) with ProtocolException m -> Error m) with
            | Error message ->
                send (Reply.Failed message)
                loop ()
            | Ok request -> if worker.Handle request then loop () else 1

    try
        loop ()
    with :? IOException ->
        // The app went away.
        0
