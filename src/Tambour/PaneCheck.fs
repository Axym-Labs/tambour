/// `tambour --check-pane [--scaling s]`: the figure pane in a real window, on both paint paths,
/// against Marey's PNG export, and a million-point scatter timed from its bytes to the screen.
/// The exit code is 0 when every check holds.
module Tambour.PaneCheck

open System
open System.Diagnostics
open System.Threading.Tasks
open Avalonia
open Avalonia.Threading
open SkiaSharp
open Marey
open Marey.UnitsOfMeasure
open Tambour.Protocol

/// How far a frame is from its reference: the largest channel difference, the largest away from
/// an edge, and the mean over every channel of every pixel, all in 255ths. A pixel is away from an
/// edge where both frames are flat around it, each one's 5 × 5 neighbourhood within 3/255: two
/// rasterisers cover an edge differently, and a seam between overlapping marks is an edge in
/// whichever frame draws it more strongly.
type Comparison =
    { Worst: int
      WorstAwayFromEdge: int
      /// How many pixels away from an edge differ by more than 4/255.
      AwayOver4: int
      Mean: float }

/// The tolerance a pane's frame is held to against the export.
let holds (c: Comparison) = c.WorstAwayFromEdge <= 4 && c.Mean < 0.5

let private rgba (bitmap: SKBitmap) =
    use converted = new SKBitmap(SKImageInfo(bitmap.Width, bitmap.Height, SKColorType.Rgba8888, SKAlphaType.Unpremul))
    bitmap.CopyTo(converted, SKColorType.Rgba8888) |> ignore
    converted.Bytes

/// Compares two bitmaps of one size.
let compare (shown: SKBitmap) (reference: SKBitmap) : Comparison =
    if shown.Width <> reference.Width || shown.Height <> reference.Height then
        failwith $"a frame of {shown.Width} × {shown.Height} against a reference of {reference.Width} × {reference.Height}"

    let w, h = shown.Width, shown.Height
    let a, b = rgba shown, rgba reference

    let channel (bytes: byte[]) i c = int bytes.[4 * i + c]

    let difference (p: byte[]) i (q: byte[]) j =
        max (abs (channel p i 0 - channel q j 0)) (max (abs (channel p i 1 - channel q j 1)) (abs (channel p i 2 - channel q j 2)))

    let flatIn (bytes: byte[]) x y =
        let i = y * w + x
        let mutable ok = true

        for dy in -2..2 do
            for dx in -2..2 do
                let nx, ny = x + dx, y + dy

                if ok && nx >= 0 && ny >= 0 && nx < w && ny < h && difference bytes i bytes (ny * w + nx) > 3 then
                    ok <- false

        ok

    let flat x y = flatIn b x y && flatIn a x y

    let mutable worst = 0
    let mutable away = 0
    let mutable awayOver4 = 0
    let mutable total = 0L

    for y in 0 .. h - 1 do
        for x in 0 .. w - 1 do
            let i = y * w + x
            let d = difference a i b i
            total <- total + int64 (abs (channel a i 0 - channel b i 0) + abs (channel a i 1 - channel b i 1) + abs (channel a i 2 - channel b i 2))

            if d > worst then
                worst <- d

            if d > 4 || d > away then
                if flat x y then
                    away <- max away d
                    if d > 4 then awayOver4 <- awayOver4 + 1

    { Worst = worst
      WorstAwayFromEdge = away
      AwayOver4 = awayOver4
      Mean = float total / float (3 * w * h) }

/// What a scene looks like exported, at a scaling: `renderPng` at 1, and otherwise the same
/// painter on a raster canvas scaled as the window's is.
let reference (scaling: float) (extent: Extent<px>) (node: Node) : SKBitmap =
    if scaling = 1.0 then
        SKBitmap.Decode(SkiaBackend.renderPng extent node)
    else
        let w = int (Math.Round(float extent.Width * scaling))
        let h = int (Math.Round(float extent.Height * scaling))
        let bitmap = new SKBitmap(SKImageInfo(w, h, SKColorType.Rgba8888, SKAlphaType.Premul))
        use canvas = new SKCanvas(bitmap)
        canvas.Clear SKColors.White
        canvas.Scale(float32 scaling)
        use measurer = new SkiaTextMeasurer()
        SkiaPainter(canvas, measurer, true, None, outlines = true).Draw(SkiaPainter.RootTransform extent, node)
        canvas.Flush()
        bitmap

let private extent = Chart.size 800.0 500.0

/// A noisy sine of `n` points, as the GPU spike drew.
let private scatter (n: int) =
    let rng = Random 7
    let t = Array.init n (fun i -> float i * 1e-3<s>)
    let x = t |> Array.map (fun t -> 1.0<m> * sin (float t * 3.0) + 0.2<m> * rng.NextDouble())
    Chart.scatterIn U.second U.metre t x |> Chart.withTitle $"{n} points"

/// Scenes as the worker lays them out, against Marey's own measurer.
let private figures () =
    [ "line, 24 points", SelfCheck.figure ()
      "scatter, 3000 points", scatter 3000
      "scatter, 10^4 points", scatter 10_000 ]
    |> List.map (fun (label, figure) -> label, Chart.toScene extent figure)

/// The limit a million points has from its bytes' arrival to the screen. A shared CI runner,
/// painting in software, is held to three times it, as the timed tests are.
let private arrivalLimit =
    if Environment.GetEnvironmentVariable "GITHUB_ACTIONS" = "true" then 300.0 else 100.0

let private check (pane: FigurePane) (window: Controls.Window) : Task<bool> =
    task {
        let scaling = window.RenderScaling
        let mutable ok = true
        let say (line: string) = eprintfn "%s" line

        let presentedOnce (picture: Picture) (capture: bool) =
            let shown = TaskCompletionSource<SKBitmap>(TaskCreationOptions.RunContinuationsAsynchronously)
            let painted = TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)

            task {
                use _ =
                    pane.Presented.Subscribe(fun p -> if obj.ReferenceEquals(p, picture) then painted.TrySetResult() |> ignore)

                use _ =
                    pane.Painted.Subscribe(function
                        | PaintFailed e -> painted.TrySetException e |> ignore
                        | Painted _ -> ())

                pane.Picture <- picture

                if capture then
                    pane.CaptureNextFrame(fun bitmap -> shown.TrySetResult bitmap |> ignore)

                let! _ = Task.WhenAny(painted.Task, Task.Delay 20_000)

                if not painted.Task.IsCompleted then
                    failwith "no frame within 20 s"

                do! painted.Task

                if capture then
                    let! _ = Task.WhenAny(shown.Task, Task.Delay 20_000)
                    return Some shown.Task.Result
                else
                    return None
            }

        let mutable backend = "?"
        use _ = pane.Painted.Subscribe(function Painted b -> backend <- b | PaintFailed _ -> ())

        for label, node in figures () do
            let picture = Picture(node, extent, 0)
            // Twice: the first frame of a new path may come before its surface is settled.
            let! _ = presentedOnce picture false
            let again = Picture(node, extent, 0)
            let! shown = presentedOnce again true
            use shown = shown.Value
            use expected = reference scaling extent node

            match Environment.GetEnvironmentVariable "TAMBOUR_CHECK_OUT" with
            | null | "" -> ()
            | dir ->
                let name = label.Replace(" ", "").Replace(",", "-").Replace("^", "")
                let save (b: SKBitmap) (suffix: string) =
                    use data = b.Encode(SKEncodedImageFormat.Png, 100)
                    IO.File.WriteAllBytes(IO.Path.Combine(dir, $"{name}-{scaling}-{suffix}.png"), data.ToArray())
                save shown "pane"
                save expected "export"
            let c = compare shown expected
            let held = holds c
            ok <- ok && held

            let path =
                match picture.Path, pane.GpuFailure with
                | Gpu, Some reason -> $"GPU path drawn as vectors ({reason})"
                | Gpu, None -> "GPU path"
                | Vector, _ -> "vector path"

            say $"""{(if held then "holds" else "FAILS")}  {label}, {path}, scaling {scaling}, canvas {backend}: away from an edge at most {c.WorstAwayFromEdge}/255 (limit 4; {c.AwayOver4} pixels over), mean {c.Mean:F3}/255 (limit 0.5), largest {c.Worst}/255"""

        match pane.GpuFailure with
        | Some reason ->
            // The vectors would take seconds; there is nothing here to time.
            say $"not measured  10^6 points: no GPU renderer on this machine ({reason})"
            return ok
        | None ->

        // A million points, from the worker's bytes to the screen.
        let node = Chart.toScene extent (scatter 1_000_000)
        let bytes = Wire.encodeWith TextMeasure.approx node
        let frame = { Figure = 0; Width = float extent.Width; Height = float extent.Height; Bytes = bytes }
        let times = ResizeArray()

        for _ in 1..5 do
            let clock = Stopwatch.StartNew()

            match Picture.Decode frame with
            | Error e -> failwith e
            | Ok picture ->
                let! _ = presentedOnce picture false
                times.Add clock.Elapsed.TotalMilliseconds

        let sorted = times |> Seq.sort |> Array.ofSeq
        let median = sorted.[sorted.Length / 2]
        let held = median < arrivalLimit
        ok <- ok && held

        say $"""{(if held then "holds" else "FAILS")}  10^6 points, {bytes.Length / 1_000_000} MB, bytes to screen: median {median:F1} ms (limit {arrivalLimit}), {String.Join(", ", sorted |> Array.map (fun t -> t.ToString "F1"))} ms"""

        return ok
    }

let run (scaling: float option) : int =
    match scaling with
    | Some s -> Environment.SetEnvironmentVariable("AVALONIA_GLOBAL_SCALE_FACTOR", string s)
    | None -> ()

    let mutable exitCode = 1

    try
        let builder = App.builder ()

        // Avalonia declines Mesa's software GL by default and paints on the CPU instead; the check
        // wants the GL path wherever there is one, so a runner without a GPU checks it too.
        let builder =
            if OperatingSystem.IsLinux() then builder.With(X11PlatformOptions(GlxRendererBlacklist = ResizeArray<string>()))
            else builder

        builder.SetupWithoutStarting() |> ignore
        let pane = new FigurePane()
        let window = Controls.Window(Title = "Tambour pane check", Width = 800.0, Height = 500.0, CanResize = false, Content = pane)

        window.Opened.Add(fun _ ->
            Dispatcher.UIThread.Post(fun () ->
                task {
                    try
                        let! ok = check pane window
                        exitCode <- if ok then 0 else 1
                    with e ->
                        eprintfn "the check failed: %O" e
                        exitCode <- 1

                    window.Close()
                }
                |> ignore))

        window.Closed.Add(fun _ -> Dispatcher.UIThread.InvokeShutdown())
        window.Show()
        Dispatcher.UIThread.MainLoop(Threading.CancellationToken.None)
    with e ->
        eprintfn "the check failed before it began: %O" e
        exitCode <- 1

    exitCode
