/// Opens a real window, paints a figure in it through Marey's Skia painter, and exits: 0 when the
/// figure painted, 1 when it did not. It is what shows that this build's Avalonia and Marey run on
/// one SkiaSharp on this machine's graphics stack.
module Tambour.SelfCheck

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Threading
open Marey
open Marey.UnitsOfMeasure

let extent = Chart.size 800.0 500.0

/// The figure the check paints: a body released from rest.
let figure () =
    let t = Array.init 24 (fun i -> float i * 0.4<s>)
    let x = t |> Array.map (fun t -> 0.5 * 9.81<m/s^2> * t * t)
    Chart.lineIn U.second U.metre t x |> Chart.withTitle "Free fall"

let private timeout = TimeSpan.FromSeconds 20.0

let private versionOf (t: Type) =
    match t.Assembly.GetCustomAttributes(typeof<Reflection.AssemblyInformationalVersionAttribute>, false) with
    | [| :? Reflection.AssemblyInformationalVersionAttribute as v |] -> v.InformationalVersion.Split('+').[0]
    | _ -> string (t.Assembly.GetName().Version)

let run () : int =
    eprintfn "SkiaSharp %s, HarfBuzzSharp %s" (versionOf typeof<SkiaSharp.SKCanvas>) (versionOf typeof<HarfBuzzSharp.Buffer>)
    let exitCode = ref 1
    try
        let measurer = new SkiaTextMeasurer()
        let node = Chart.toSceneWith measurer extent (figure ())
        App.builder().SetupWithoutStarting() |> ignore
        let view = FigureView(node, extent, measurer)
        let window = Window(Title = "Tambour self-check", SizeToContent = SizeToContent.WidthAndHeight, Content = view)
        let finish code message =
            Dispatcher.UIThread.Post(fun () ->
                eprintfn "%s" message
                exitCode.Value <- code
                window.Close())
        // The first report wins: a paint arrives on the render thread, the timeout on the UI thread.
        let reported = ref 0
        let claim () = Threading.Interlocked.Exchange(&reported.contents, 1) = 0
        view.Painted.Add(fun result ->
            if claim () then
                match result with
                | Painted backend -> finish 0 $"painted the figure in a window (canvas: {backend})"
                | PaintFailed e -> finish 1 $"the figure did not paint: {e}")
        window.Opened.Add(fun _ ->
            DispatcherTimer.RunOnce((fun () ->
                if claim () then
                    finish 1 $"no paint within {timeout.TotalSeconds} s"), timeout) |> ignore)
        window.Closed.Add(fun _ -> Dispatcher.UIThread.InvokeShutdown())
        window.Show()
        Dispatcher.UIThread.MainLoop(Threading.CancellationToken.None)
    with e ->
        eprintfn "the self-check failed before painting: %O" e
        exitCode.Value <- 1
    exitCode.Value
