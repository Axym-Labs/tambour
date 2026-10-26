module Tambour.Tests.FigurePaneTests

open System.IO
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Headless.XUnit
open Marey
open Marey.UnitsOfMeasure
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
    use measurer = new SkiaTextMeasurer()
    let node = Chart.toSceneWith measurer SelfCheck.extent (SelfCheck.figure ())
    let view = new FigurePane(Picture = Picture(node, SelfCheck.extent, 0))
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

/// A pane showing the self-check's figure in a window of its size, and every message it sends.
let private shown () =
    use measurer = new SkiaTextMeasurer()
    let node = Chart.toSceneWith measurer SelfCheck.extent (SelfCheck.figure ())
    let pane = new FigurePane(Picture = Picture(node, SelfCheck.extent, 0))
    let sent = ResizeArray<Tambour.Protocol.LiveInput>()
    pane.Input.Add sent.Add
    let window = Window(SizeToContent = SizeToContent.WidthAndHeight, Content = pane)
    window.Show()
    window.CaptureRenderedFrame().Dispose()
    sent.Clear()
    pane, window, sent

let private kinds (sent: ResizeArray<Tambour.Protocol.LiveInput>) = sent |> Seq.map (fun m -> m.Kind) |> List.ofSeq

[<AvaloniaFact>]
let ``a drag zooms to what it covers, and the pointer hovers on the way there`` () =
    let _, window, sent = shown ()
    window.MouseMove(Avalonia.Point(100.0, 100.0), Avalonia.Input.RawInputModifiers.None)
    window.MouseDown(Avalonia.Point(100.0, 100.0), Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
    window.MouseMove(Avalonia.Point(200.0, 150.0), Avalonia.Input.RawInputModifiers.LeftMouseButton)
    window.MouseUp(Avalonia.Point(300.0, 250.0), Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
    Assert.Equal<string list>([ "hover"; "zoom" ], kinds sent)
    Assert.Equal<float[]>([| 100.0; 100.0 |], sent.[0].Args)
    Assert.Equal<float[]>([| 100.0; 100.0; 300.0; 250.0 |], sent.[1].Args)

[<AvaloniaFact>]
let ``a shifted drag pans by each step, and a click that does not move zooms nothing`` () =
    let _, window, sent = shown ()
    let shift = Avalonia.Input.RawInputModifiers.Shift
    window.MouseDown(Avalonia.Point(100.0, 100.0), Avalonia.Input.MouseButton.Left, shift)
    window.MouseMove(Avalonia.Point(110.0, 95.0), shift ||| Avalonia.Input.RawInputModifiers.LeftMouseButton)
    window.MouseMove(Avalonia.Point(130.0, 95.0), shift ||| Avalonia.Input.RawInputModifiers.LeftMouseButton)
    window.MouseUp(Avalonia.Point(130.0, 95.0), Avalonia.Input.MouseButton.Left, shift)
    Assert.Equal<string list>([ "pan"; "pan" ], kinds sent)
    Assert.Equal<float[]>([| 100.0; 100.0; 10.0; -5.0 |], sent.[0].Args)
    Assert.Equal<float[]>([| 110.0; 95.0; 20.0; 0.0 |], sent.[1].Args)
    sent.Clear()
    window.MouseDown(Avalonia.Point(50.0, 50.0), Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
    window.MouseUp(Avalonia.Point(51.0, 50.0), Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
    Assert.Empty sent

[<AvaloniaFact>]
let ``a double click resets, and a new size is sent`` () =
    let pane, window, sent = shown ()
    for _ in 1..2 do
        window.MouseDown(Avalonia.Point(50.0, 50.0), Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
        window.MouseUp(Avalonia.Point(50.0, 50.0), Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)

    Assert.Contains("reset", kinds sent)
    sent.Clear()
    window.SizeToContent <- SizeToContent.Manual
    window.Width <- 600.0
    window.Height <- 300.0
    window.CaptureRenderedFrame().Dispose()
    let size = sent |> Seq.filter (fun m -> m.Kind = "size") |> Seq.last
    Assert.Equal<float[]>([| 600.0; 300.0 |], size.Args)
    Assert.NotNull pane.Picture

[<AvaloniaFact>]
let ``the readout is drawn over the plot it reads, and only for the figure on screen`` () =
    let pane, window, _ = shown ()
    use before = capture window

    let readout figure : Tambour.Protocol.ReadoutState =
        { Figure = figure
          Visible = true
          X0 = 100.0
          Y0 = 100.0
          X1 = 700.0
          Y1 = 400.0
          Right = false
          Lines = [| "x 1.2 s"; "y 3.4 m" |] }

    pane.Readout <- Some(readout 1)
    use other = capture window
    Assert.Equal(0, fst (compare other before))
    pane.Readout <- Some(readout 0)
    use after = capture window
    Assert.True(fst (compare after before) > 100, "the readout drew nothing")

/// A patch from the worker applied to the picture on screen gives the scene the worker holds.
[<Fact>]
let ``a patch applied to a picture gives the scene Live draws next`` () =
    let t = Array.init 20 (fun i -> float i * 0.1<s>)
    let live =
        interact {
            let! g = Control.slider U.metre "g" 1.0<m> 10.0<m> 2.0<m>
            return Chart.lineIn U.second U.metre t (t |> Array.map (fun t -> g * float t * float t))
        }

    let state = Live.init live
    let before = Live.scene live state
    let after' = Live.step live state (LiveMsg.Set(0, 7.0))
    let after = Live.scene live after'
    let frame (bytes: byte[]) : Tambour.Protocol.Frame = { Figure = 0; Width = 640.0; Height = 400.0; Bytes = bytes }
    let picture = Picture.Decode(frame (Wire.encodeWith TextMeasure.approx before)) |> Result.toOption |> Option.get
    let patched = picture.Apply(frame (Wire.encodePatchWith TextMeasure.approx (Scene.diff before after)))

    match patched with
    | Ok p -> Assert.Equal(after, p.Node)
    | Error e -> failwith e

/// At a window scaling of 2, vectors are drawn at twice the pixels, as sharp as an export at twice
/// the size, not an export stretched.
[<AvaloniaFact>]
let ``at a scaling of 2 the vectors are drawn at twice the pixels`` () =
    let pane, window, _ = shown ()
    window.SetRenderScaling 2.0
    use frame = capture window
    use expected = PaneCheck.reference 2.0 SelfCheck.extent pane.Picture.Node
    Assert.Equal((expected.Width, expected.Height), (frame.Width, frame.Height))
    let c = PaneCheck.compare frame expected
    Assert.True(PaneCheck.holds c, sprintf "%A" c)
