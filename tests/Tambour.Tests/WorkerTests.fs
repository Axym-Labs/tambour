/// The worker as the app meets it: a real `tambour --worker` process behind a socket.
[<Xunit.Collection("workers")>]
module Tambour.Tests.WorkerTests

open System
open System.Diagnostics
open System.IO
open System.Threading
open System.Threading.Tasks
open Marey
open Tambour
open Tambour.Protocol
open Xunit

/// Timed tests run alone, so that a test starting workers beside them does not take their CPU.
[<CollectionDefinition("workers", DisableParallelization = true)>]
type WorkersCollection() = class end

let private command: Command =
    { File = Path.Combine(AppContext.BaseDirectory, (if OperatingSystem.IsWindows() then "tambour.exe" else "tambour"))
      Arguments = [] }

let private init: Init =
    { References = [||]
      Prelude = "open Marey\nopen Marey.UnitsOfMeasure\nopen Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols\n"
      Skia = { Dpi = float Space.screenDpi; FontFiles = [||] } }

/// A shared runner's CPU is slower and busier than a desk's, so CI holds the timed tests to three
/// times their limits: loose enough not to fail at random, tight enough to catch a slowdown.
let private limit (ms: float) =
    if Environment.GetEnvironmentVariable "GITHUB_ACTIONS" = "true" then 3.0 * ms else ms

let private log (line: string) =
    TestContext.Current.TestOutputHelper.WriteLine line

let private started (workers: Workers) =
    workers.StartAsync().Wait(TimeSpan.FromSeconds 120.0) |> ignore
    workers

/// Workers shared by the tests that need nothing but a worker, started once.
let private shared = lazy (started (new Workers(command, init)))

/// Reads replies until `pick` takes one, and returns it with everything read before it.
let private until (workers: Workers) (seconds: float) (pick: Reply -> 'a option) : Task<'a * Reply list> =
    task {
        use cts = new CancellationTokenSource(TimeSpan.FromSeconds seconds)
        let seen = ResizeArray()
        let mutable found = None

        try
            while found.IsNone do
                let! reply = workers.Replies.ReadAsync(cts.Token).AsTask()
                seen.Add reply
                found <- pick reply
        with :? OperationCanceledException ->
            failwithf "no such reply within %g s; read %A" seconds (List.ofSeq seen)

        return found.Value, List.ofSeq seen
    }

let private evaluated =
    function
    | Reply.Evaluated e -> Some e
    | _ -> None

let private scene =
    function
    | Reply.Scene(i, bytes) -> Some(i, bytes)
    | _ -> None

/// Throws away whatever an earlier test left unread.
let private drain (workers: Workers) =
    let mutable r = Unchecked.defaultof<Reply>

    while workers.Replies.TryRead &r do
        ()

let private run (workers: Workers) (text: string) =
    task {
        drain workers
        do! workers.SendAsync(Request.Evaluate { Text = text; Path = null })
        return! until workers 60.0 evaluated
    }

/// The fsi-worker spike's script: 2000 rows with units, a colour per sample.
let private twoThousand =
    """
type Row = { Time: float<s>; Displacement: float<m>; Sample: string }
let rows = Array.init 2000 (fun i -> let t = float i * 0.01<s> in { Time = t; Displacement = 0.5 * 9.81<m/s^2> * t * t; Sample = string (i % 3) })
let fig =
    plot rows {
        x U.second (fun r -> r.Time)
        y U.metre (fun r -> r.Displacement)
        color (fun r -> r.Sample)
        layer GeomPoint
    }
"""

let private live =
    """
let t = Array.init 50 (fun i -> float i * 0.1<s>)
let explorer =
    interact {
        let! g = Control.slider U.metre "g" 1.0<m> 10.0<m> 2.0<m>
        return Chart.lineIn U.second U.metre t (t |> Array.map (fun t -> g * float t * float t))
    }
"""

let private names (e: Evaluated) = e.Figures |> Array.map (fun f -> f.Name) |> List.ofArray

[<Fact>]
let ``a run's figure arrives as a scene Marey's wire format reads back`` () : Task =
    task {
        let workers = shared.Value
        let! e, _ = run workers twoThousand
        Assert.True(e.Ok, sprintf "%A" e)
        Assert.Equal<string list>([ "fig" ], names e)
        let! (index, bytes), _ = until workers 30.0 scene
        Assert.Equal(0, index)

        match Wire.decodeWith bytes with
        | Ok(node, _) ->
            // Every row is drawn: one mark per point, whatever form the marks take.
            let svg = SvgBackend.render (Chart.size 640.0 400.0) node
            log $"{bytes.Length} bytes on the wire, {svg.Length} characters of SVG"
            Assert.True(bytes.Length > 2000 * 8, $"the scene is {bytes.Length} bytes")
        | Error message -> failwith message
    }

[<Fact>]
let ``a figure the script no longer binds is no longer found, and it comes last`` () : Task =
    task {
        let workers = shared.Value
        let figure = "Chart.lineIn U.second U.metre [ 0.0<s>; 1.0<s> ] [ 0.0<m>; 1.0<m> ]"
        let! first, _ = run workers $"let one = {figure}\nlet two = {figure}\nlet n = 3\n{figure}\n"
        Assert.Equal<string list>([ "one"; "two"; "it" ], names first)
        let! second, _ = run workers $"let two = {figure}\n"
        Assert.Equal<string list>([ "two" ], names second)
    }

[<Fact>]
let ``a script that does not compile keeps the figures, with its error where it is`` () : Task =
    task {
        let workers = shared.Value
        let! good, _ = run workers "let kept = Chart.lineIn U.second U.metre [ 0.0<s>; 1.0<s> ] [ 0.0<m>; 1.0<m> ]\n"
        Assert.True good.Ok
        let! bad, _ = run workers "let a = 1\nlet b: int = \"text\"\n"
        Assert.False bad.Ok
        Assert.Null bad.Exception
        Assert.Equal<string list>([ "kept" ], names bad)
        let error = bad.Diagnostics |> Array.find (fun d -> d.Severity = "error")
        Assert.Equal(2, error.StartLine)
        Assert.Equal(13, error.StartColumn)
        let! thrown, _ = run workers "let c: int = failwith \"on purpose\"\n"
        Assert.False thrown.Ok
        Assert.Contains("on purpose", thrown.Exception)
    }

[<Fact>]
let ``what a script prints arrives before its run is reported`` () : Task =
    task {
        let workers = shared.Value
        let! _, seen = run workers "printfn \"one\"\neprintf \"two\"\nprintf \"three\"\n"

        let text stream =
            seen
            |> List.choose (function
                | Reply.Output o when o.Stream = stream -> Some o.Text
                | _ -> None)
            |> String.concat ""

        Assert.Equal("one\nthree", text "out")
        Assert.Equal("two", text "err")
    }

[<Fact>]
let ``a live figure answers a control, a zoom and the pointer`` () : Task =
    task {
        let workers = shared.Value
        let! e, _ = run workers live
        Assert.Equal<FigureInfo[]>([| { Name = "explorer"; Controls = 1 } |], e.Figures)
        let! _ = until workers 30.0 scene

        let send kind args =
            workers.SendAsync(Request.Live { Kind = kind; Args = args })

        do! send "set" [| 0.0; 5.0 |]

        let! controls, _ =
            until workers 30.0 (function
                | Reply.Controls c -> Some c
                | _ -> None)

        Assert.Equal(5.0, controls.Controls.[0].Value)
        Assert.Contains("5", controls.Controls.[0].Shown)

        do! send "zoom" [| 200.0; 100.0; 400.0; 300.0 |]

        let! _ =
            until workers 30.0 (function
                | Reply.Patch(0, bytes) -> Some bytes
                | _ -> None)

        // The pointer reads the figure without changing it: a readout, and no patch.
        do! send "hover" [| 320.0; 200.0 |]

        let! readout, seen =
            until workers 30.0 (function
                | Reply.Readout r -> Some r
                | _ -> None)

        Assert.True(readout.Visible)
        Assert.Equal(2, readout.Lines.Length)
        Assert.DoesNotContain(seen, (function Reply.Patch _ -> true | _ -> false))

        do! workers.SendAsync(Request.Export { Format = "png"; Width = 400.0; Height = 250.0 })

        let! png, _ =
            until workers 30.0 (function
                | Reply.Exported(0, bytes) -> Some bytes
                | _ -> None)

        Assert.Equal<byte[]>([| 0x89uy; byte 'P'; byte 'N'; byte 'G' |], png.[0..3])
    }

/// A re-run of an unchanged 2000-point script reaches the app's scene in under 100 ms,
/// measured from sending `Evaluate`.
[<Fact>]
let ``a re-run of 2000 points reaches the scene within 100 ms`` () : Task =
    task {
        let workers = shared.Value
        let! warm, _ = run workers twoThousand
        Assert.True(warm.Ok, sprintf "%A" warm)

        let times = ResizeArray()

        for _ in 1..20 do
            drain workers
            let clock = Stopwatch.StartNew()
            do! workers.SendAsync(Request.Evaluate { Text = twoThousand; Path = null })
            let! _, seen = until workers 30.0 scene
            times.Add clock.Elapsed.TotalMilliseconds

            match seen |> List.tryPick evaluated with
            | Some e when times.Count = 20 ->
                log $"last run: evaluate {e.Timings.Evaluate:F1} ms, layout {e.Timings.Layout:F1} ms, encode {e.Timings.Encode:F1} ms"
            | _ -> ()

        let sorted = times |> Seq.sort |> Array.ofSeq
        let median = sorted.[sorted.Length / 2]
        log $"Evaluate to Scene, 20 re-runs: median {median:F1} ms, min {sorted.[0]:F1}, max {sorted.[sorted.Length - 1]:F1}"
        Assert.True(median < limit 100.0, $"the median re-run took {median:F1} ms")
    }

/// Cancelling a script stuck in a loop returns a worker that runs the next script, in
/// under 0.5 s.
[<Fact>]
let ``cancel returns a usable worker within half a second`` () : Task =
    task {
        use workers = started (new Workers(command, init))
        let stuck = workers.Pid
        do! workers.SendAsync(Request.Evaluate { Text = "while true do ()\n"; Path = null })
        do! Task.Delay 500

        let clock = Stopwatch.StartNew()
        do! workers.CancelAsync()
        let! e, _ = run workers "let back = 1 + 1\n"
        let elapsed = clock.Elapsed.TotalMilliseconds
        log $"cancel to the next run reported: {elapsed:F1} ms"

        Assert.True(e.Ok)
        Assert.NotEqual(stuck, workers.Pid)
        Assert.True(elapsed < limit 500.0, $"the worker came back in {elapsed:F1} ms")
    }

[<Fact>]
let ``a worker past its heap limit is replaced while idle, and its script runs again`` () : Task =
    task {
        use workers = started (new Workers(command, init, 0L))
        let first = workers.Pid
        // The spare is ready by the time the first run is reported, so the replacement follows.
        do! Task.Delay 3000
        let! e, _ = run workers "let again = Chart.lineIn U.second U.metre [ 0.0<s>; 1.0<s> ] [ 0.0<m>; 1.0<m> ]\n"
        let! replayed, _ = until workers 60.0 evaluated
        Assert.Equal<string list>(names e, names replayed)
        Assert.NotEqual(first, workers.Pid)
    }

[<Fact>]
let ``a worker that dies is reported and replaced`` () : Task =
    task {
        use workers = started (new Workers(command, init))
        let first = workers.Pid
        do! workers.SendAsync(Request.Evaluate { Text = "exit 3\n"; Path = null })

        let! message, _ =
            until workers 30.0 (function
                | Reply.Failed m -> Some m
                | _ -> None)

        Assert.Contains("stopped", message)
        let! e, _ = run workers "let alive = 1\n"
        Assert.True e.Ok
        Assert.NotEqual(first, workers.Pid)
    }

[<Fact>]
let ``a worker stuck in a script ends when the app does`` () : Task =
    task {
        // Another worker stands in for the app: a process that lives until it is killed.
        let! app = WorkerProcess.StartAsync command
        let! worker = WorkerProcess.StartAsync(command, app.Pid)
        do! worker.SendAsync(Request.Init init)
        let! ready = worker.Replies.ReadAsync().AsTask()
        Assert.True(ready.IsReady, sprintf "%A" ready)
        do! worker.SendAsync(Request.Evaluate { Text = "while true do ()\n"; Path = null })
        do! Task.Delay 300
        app.Kill()
        let! ended = Task.WhenAny(worker.Exited, Task.Delay(TimeSpan.FromSeconds 10.0))

        if not (obj.ReferenceEquals(ended, worker.Exited)) then
            worker.Kill()
            failwith "the worker was still running 10 s after the app ended"
    }

[<Fact>]
let ``the worker does not load Avalonia`` () : Task =
    task {
        // Read from the process's mapped files, which Linux lists; the assemblies are the same
        // on Windows.
        if OperatingSystem.IsLinux() then
            let workers = shared.Value
            let! _, _ = run workers twoThousand
            let maps = File.ReadAllText $"/proc/{workers.Pid.Value}/maps"
            Assert.Contains("Marey.Plot.dll", maps)
            Assert.DoesNotContain("Avalonia", maps)
    }

[<Fact>]
let ``a package reference without an SDK says what to install`` () =
    let text = "let a = 1\n  #r \"nuget: Newtonsoft.Json\"\n"

    let raw =
        [| { Severity = "error"
             Code = "FS3216"
             Message = "package manager error"
             StartLine = 2
             StartColumn = 2
             EndLine = 2
             EndColumn = 30 } |]

    let explained = Evaluation.explainNuget text false false raw
    let d = Assert.Single explained
    Assert.Contains("needs the .NET SDK", d.Message)
    Assert.Equal((2, 2), (d.StartLine, d.StartColumn))
    Assert.Equal<Diagnostic[]>(raw, Evaluation.explainNuget text true false raw)
    Assert.Equal<Diagnostic[]>(raw, Evaluation.explainNuget "let a = 1\n" false false raw)
