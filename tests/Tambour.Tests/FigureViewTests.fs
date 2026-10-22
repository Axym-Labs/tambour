module Tambour.Tests.FigureViewTests

open System.IO
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Headless.XUnit
open Marey
open SkiaSharp
open Tambour
open Xunit

let private decode (png: byte array) = SKBitmap.Decode png

let private capture (window: Window) =
    use frame = window.CaptureRenderedFrame()
    use stream = new MemoryStream()
    frame.Save(stream, Avalonia.Media.Imaging.PngBitmapEncoderOptions.Default)
    decode (stream.ToArray())

/// The largest channel difference, and the mean one over every channel of every pixel, in 255ths.
let private compare (a: SKBitmap) (b: SKBitmap) =
    let mutable worst = 0
    let mutable total = 0L
    for y in 0 .. a.Height - 1 do
        for x in 0 .. a.Width - 1 do
            let p, q = a.GetPixel(x, y), b.GetPixel(x, y)
            for d in [ abs (int p.Red - int q.Red); abs (int p.Green - int q.Green); abs (int p.Blue - int q.Blue) ] do
                worst <- max worst d
                total <- total + int64 d
    worst, float total / float (3 * a.Width * a.Height)

[<AvaloniaFact>]
let ``the figure pane paints what Marey's PNG export writes`` () =
    let measurer = new SkiaTextMeasurer()
    let node = Chart.toSceneWith measurer SelfCheck.extent (SelfCheck.figure ())
    let view = FigureView(node, SelfCheck.extent, measurer)
    let painted = ResizeArray()
    view.Painted.Add painted.Add
    let window = Window(SizeToContent = SizeToContent.WidthAndHeight, Content = view)
    window.Show()
    use shown = capture window
    use exported = decode (SkiaBackend.renderPng SelfCheck.extent node)
    Assert.Equal((exported.Width, exported.Height), (shown.Width, shown.Height))
    Assert.Contains(Painted "CPU", painted)
    // The two canvases antialias edges a little differently; nothing else may differ.
    let worst, mean = compare shown exported
    Assert.True(worst <= 4, $"a channel differs by {worst}/255")
    Assert.True(mean < 0.5, $"the mean channel difference is {mean}/255")
