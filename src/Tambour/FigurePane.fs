namespace Tambour

open System
open System.Globalization
open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Media
open Avalonia.Media.Imaging
open Avalonia.Platform
open Avalonia.Rendering.SceneGraph
open Avalonia.Skia
open SkiaSharp
open Tambour.Protocol

/// What a paint of the figure pane came to: the canvas's GPU backend ("OpenGL", or "CPU" for a
/// raster canvas), or the exception the painter raised.
type PaintResult =
    | Painted of backend: string
    | PaintFailed of exn

/// How a picture is painted. Below `Marey.Html.CanvasThreshold` drawn primitives, as vectors on the
/// window's own canvas, through the painter Marey's PNG export uses; from there on, through
/// Marey's GPU renderer offscreen, as an image. A figure changes painter at the size it changes
/// from SVG to canvas in a notebook.
type PaintPath =
    | Vector
    | Gpu

/// A decoded scene at the extent it was laid out at. Compared by reference: a picture of a
/// million points is not compared mark by mark to learn that it changed.
[<AllowNullLiteral>]
type Picture(node: Marey.Node, extent: Marey.Extent<Marey.px>, figure: int) =
    let primitives = lazy (Marey.Scene.primitiveCount node)
    member _.Node = node
    member _.Extent = extent
    member _.Figure = figure
    member _.Primitives = primitives.Value

    member this.Path = if this.Primitives >= Marey.Html.CanvasThreshold then Gpu else Vector

    /// A scene from the worker's bytes.
    static member Decode(frame: Frame) : Result<Picture, string> =
        Marey.Wire.decodeWith frame.Bytes
        |> Result.map (fun (node, _) -> Picture(node, { Width = frame.Width * 1.0<Marey.px>; Height = frame.Height * 1.0<Marey.px> }, frame.Figure))

    /// This picture with a patch from the worker applied.
    member this.Apply(frame: Frame) : Result<Picture, string> =
        match Marey.Wire.decodePatchWith frame.Bytes with
        | Ok(patch, _) ->
            Ok(Picture(Marey.Scene.apply patch node, { Width = frame.Width * 1.0<Marey.px>; Height = frame.Height * 1.0<Marey.px> }, frame.Figure))
        | Error e -> Error e

/// Paints a laid-out Marey scene on Avalonia's own Skia canvas, through the same painter that
/// writes Marey's PNG and PDF exports. The canvas carries the window's scaling, so a vector
/// figure is sharp at any.
type private SceneDrawOp(bounds: Rect, node: Marey.Node, extent: Marey.Extent<Marey.px>, measurer: Marey.SkiaTextMeasurer, failed: exn -> unit) =
    interface ICustomDrawOperation with
        member _.Bounds = bounds
        member _.HitTest _ = false
        member _.Equals(_: ICustomDrawOperation) = false
        member _.Dispose() = ()

        member _.Render(context) =
            match context.TryGetFeature<ISkiaSharpApiLeaseFeature>() with
            | null -> failed (InvalidOperationException "the renderer offers no Skia canvas")
            | feature ->
                try
                    use lease = feature.Lease()
                    // Text as outlines, as the PNG export draws it, so that the pane shows what an
                    // export writes rather than what this platform's font engine hints.
                    let painter = Marey.SkiaPainter(lease.SkCanvas, measurer, true, None, outlines = true)
                    painter.Draw(Marey.SkiaPainter.RootTransform extent, node)
                with e ->
                    failed e

/// The last operation of a frame: waits for the GPU to finish what the frame drew, reports the
/// paint, and hands a copy of the pane's pixels to whoever asked for one.
type private FrameEndOp(bounds: Rect, report: PaintResult -> unit, presented: unit -> unit, capture: (SKBitmap -> unit) option, failure: exn option ref) =
    interface ICustomDrawOperation with
        member _.Bounds = bounds
        member _.HitTest _ = false
        member _.Equals(_: ICustomDrawOperation) = false
        member _.Dispose() = ()

        member _.Render(context) =
            match failure.Value with
            | Some e -> report (PaintFailed e)
            | None ->
                match context.TryGetFeature<ISkiaSharpApiLeaseFeature>() with
                | null -> report (PaintFailed(InvalidOperationException "the renderer offers no Skia canvas"))
                | feature ->
                    try
                        use lease = feature.Lease()
                        lease.SkCanvas.Flush()

                        match lease.GrContext with
                        | null -> ()
                        | gr ->
                            gr.Flush()
                            gr.Submit true |> ignore

                        match capture, lease.SkSurface with
                        | Some _, null -> report (PaintFailed(InvalidOperationException "the lease offers no surface to read"))
                        | Some deliver, surface ->
                            // The pane's rectangle on the surface, in device pixels.
                            let m = lease.SkCanvas.TotalMatrix
                            let r = m.MapRect(SKRect(0.0f, 0.0f, float32 bounds.Width, float32 bounds.Height))
                            let ri = SKRectI(int (round r.Left), int (round r.Top), int (round r.Right), int (round r.Bottom))
                            use image = surface.Snapshot ri
                            deliver (SKBitmap.FromImage image)
                        | None, _ -> ()

                        report (Painted(if isNull lease.GrContext then "CPU" else string lease.GrContext.Backend))
                        presented ()
                    with e ->
                        report (PaintFailed e)

/// The figure on screen. It paints the picture it is given at that picture's extent, draws the
/// worker's readout over it, and turns the pointer into a live figure's messages: a drag zooms to
/// what it covers, a shifted drag pans, a double click resets, motion hovers, and a new size is
/// sent as one.
type FigurePane() as this =
    inherit Control()

    let measurer = new Marey.SkiaTextMeasurer()
    let painted = Event<PaintResult>()
    let presented = Event<Picture>()
    let input = Event<LiveInput>()
    let picked = Event<Rect>()
    let mutable marks: Point[] = [||]
    let mutable picking = false

    let mutable picture: Picture = null
    let mutable readout: ReadoutState option = None
    let mutable image: WriteableBitmap = null
    let mutable imageFailure: exn option = None
    /// The window's scaling the image was drawn at.
    let mutable imageScale = 1.0
    let mutable scalingWatch: IDisposable = null
    /// Made when a picture first needs it: a GL context of its own, on a thread of its own.
    let gpu = lazy (try Ok(new Marey.GpuRenderer(instancedMarkers = true)) with e -> Error e.Message)
    let mutable capture: (SKBitmap -> unit) option = None

    // The pointer: where a drag began, the band it covers, the last point of a pan.
    let mutable pressed: Point option = None
    let mutable panning = false
    let mutable last = Point()
    let mutable band: Rect option = None

    let send kind (args: float[]) = input.Trigger { Kind = kind; Args = args }

    let scaling () =
        match TopLevel.GetTopLevel this with
        | null -> 1.0
        | top -> top.RenderScaling

    /// The image the GPU path draws, at the picture's own extent and the window's scaling, so a
    /// HiDPI window shows it as sharp as the vectors rather than stretched.
    let renderImage () =
        if not (isNull image) then
            image.Dispose()
            image <- null

        imageFailure <- None

        if not (isNull picture) && picture.Path = Gpu then
            match gpu.Force() with
            | Error reason -> imageFailure <- Some(InvalidOperationException $"the GPU renderer is unavailable: {reason}")
            | Ok renderer ->
                try
                    let scale = scaling ()
                    let img = renderer.RenderImageAt scale picture.Extent picture.Node
                    // At 96 dpi the bitmap's size is its pixels, which `Render` takes whole as the
                    // source and draws into the figure's px.
                    let bitmap = new WriteableBitmap(PixelSize(img.Width, img.Height), Avalonia.Vector(96.0, 96.0), PixelFormat.Rgba8888, AlphaFormat.Unpremul)
                    use fb = bitmap.Lock()
                    let row = img.Width * 4

                    for y in 0 .. img.Height - 1 do
                        Runtime.InteropServices.Marshal.Copy(img.Rgba, y * row, fb.Address + nativeint (y * fb.RowBytes), row)

                    image <- bitmap
                    imageScale <- scale
                with e ->
                    imageFailure <- Some e

    /// Draws the image again when the window's scaling is no longer the one it was drawn at: the
    /// pane shown before the window had one, or the window moved to a screen of another scale.
    let rescale () =
        if not (isNull picture) && picture.Path = Gpu && scaling () <> imageScale then
            renderImage ()
            this.InvalidateVisual()

    do
        this.ClipToBounds <- true
        this.Focusable <- true
        this.ActualThemeVariantChanged.Add(fun _ -> this.InvalidateVisual())

        this.SizeChanged.Add(fun e ->
            if e.NewSize.Width > 0.0 && e.NewSize.Height > 0.0 then
                send "size" [| e.NewSize.Width; e.NewSize.Height |])

    /// Raised on the render thread after each paint.
    [<CLIEvent>]
    member _.Painted = painted.Publish

    /// Raised on the render thread when a picture's frame has been drawn and the GPU has finished it.
    [<CLIEvent>]
    member _.Presented = presented.Publish

    /// A point picked (Ctrl+click), as a rectangle of no size, or an area (Alt+drag), in the
    /// figure's px.
    [<CLIEvent>]
    member _.Picked = picked.Publish

    /// Points marked in the figure, in its px, as picked rows are.
    member _.Marks
        with get () = marks
        and set (value: Point[]) =
            marks <- value
            this.InvalidateVisual()

    /// A reader's message for the worker.
    [<CLIEvent>]
    member _.Input = input.Publish

    /// The picture on screen, or null for none.
    member _.Picture
        with get () = picture
        and set (value: Picture) =
            if not (obj.ReferenceEquals(value, picture)) then
                picture <- value
                renderImage ()
                this.InvalidateVisual()

    /// What the pointer reads, from the worker.
    member _.Readout
        with get () = readout
        and set (value: ReadoutState option) =
            readout <- value
            this.InvalidateVisual()

    /// Why the GPU path drew vectors instead, if it did.
    member _.GpuFailure = imageFailure |> Option.map (fun e -> e.Message)

    /// How the picture on screen is painted, if there is one.
    member _.PaintPath = if isNull picture then None else Some picture.Path

    /// Asks for the pane's pixels as the next frame presents them, at the window's scaling.
    member _.CaptureNextFrame(deliver: SKBitmap -> unit) =
        capture <- Some deliver
        this.InvalidateVisual()

    /// The picture's extent; a layout that stretches the pane gives it the rest, and the new size
    /// goes to the worker.
    override _.MeasureOverride(_: Size) =
        match picture with
        | null -> Size(0.0, 0.0)
        | p -> Size(float p.Extent.Width, float p.Extent.Height)

    override _.Render(dc) =
        let bounds = Rect(this.Bounds.Size)
        let look = Palette.forVariant this.ActualThemeVariant
        // The figure is drawn as its script's theme draws it, on the app's own surround.
        dc.FillRectangle(SolidColorBrush look.Surface, bounds)

        match picture with
        | null -> ()
        | p ->
            let figure = Rect(0.0, 0.0, float p.Extent.Width, float p.Extent.Height)

            // Set on the render thread by the painter, read there by the frame's last operation.
            let failure = ref None
            let vectors () = dc.Custom(new SceneDrawOp(figure, p.Node, p.Extent, measurer, fun e -> failure.Value <- Some e))

            match p.Path, image with
            | Vector, _ -> vectors ()
            | Gpu, null ->
                // No GPU here: the vectors, slower but the same figure.
                vectors ()
            | Gpu, bitmap -> dc.DrawImage(bitmap, Rect(bitmap.Size), figure)

            match band with
            | Some r ->
                dc.DrawRectangle(SolidColorBrush look.Roles.Selected, Pen(SolidColorBrush look.Accent, Tokens.border), r)
            | None -> ()

            let ring = Pen(SolidColorBrush look.Roles.Pick, 2.0 * Tokens.border)

            for m in marks do
                if Double.IsFinite m.X && Double.IsFinite m.Y then
                    dc.DrawEllipse(null, ring, m, 7.0, 7.0)

            match readout with
            | Some r when r.Visible && r.Figure = p.Figure && r.Lines.Length > 0 ->
                let text =
                    FormattedText(String.Join("\n", r.Lines), CultureInfo.InvariantCulture, FlowDirection.LeftToRight, Typeface.Default, Tokens.Type.small, SolidColorBrush look.Text)

                let x = if r.Right then r.X1 - 4.0 - text.Width - 8.0 else r.X0 + 4.0
                let box = Rect(x, r.Y0 + 4.0, text.Width + 8.0, text.Height + 4.0)
                dc.DrawRectangle(SolidColorBrush(look.Raised, 0.92), Pen(SolidColorBrush look.Line, 0.5), box, 3.0, 3.0)
                dc.DrawText(text, Point(box.X + 4.0, box.Y + 2.0))
            | _ -> ()

            let deliver = capture
            capture <- None
            dc.Custom(new FrameEndOp(bounds, painted.Trigger, (fun () -> presented.Trigger p), deliver, failure))

    override _.OnPointerPressed(e) =
        base.OnPointerPressed e
        let point = e.GetCurrentPoint this

        if point.Properties.IsLeftButtonPressed then
            if e.KeyModifiers.HasFlag KeyModifiers.Control then
                picked.Trigger(Rect(point.Position, Size(0.0, 0.0)))
            elif e.ClickCount = 2 then
                pressed <- None
                band <- None
                send "reset" [||]
            else
                picking <- e.KeyModifiers.HasFlag KeyModifiers.Alt
                pressed <- Some point.Position
                last <- point.Position
                panning <- e.KeyModifiers.HasFlag KeyModifiers.Shift
                e.Pointer.Capture this |> ignore

    override _.OnPointerMoved(e) =
        base.OnPointerMoved e
        let p = e.GetPosition this

        match pressed with
        | Some _ when panning ->
            send "pan" [| last.X; last.Y; p.X - last.X; p.Y - last.Y |]
            last <- p
        | Some start ->
            band <- Some(Rect(start, p).Normalize())
            this.InvalidateVisual()
        | None -> send "hover" [| p.X; p.Y |]

    override _.OnPointerReleased(e) =
        base.OnPointerReleased e
        let p = e.GetPosition this

        match pressed with
        | Some start when picking -> picked.Trigger(Rect(start, p).Normalize())
        | Some start when not panning && (abs (p.X - start.X) >= 3.0 || abs (p.Y - start.Y) >= 3.0) ->
            send "zoom" [| start.X; start.Y; p.X; p.Y |]
        | _ -> ()

        picking <- false

        pressed <- None
        band <- None
        e.Pointer.Capture null |> ignore
        this.InvalidateVisual()

    override _.OnAttachedToVisualTree(e) =
        base.OnAttachedToVisualTree e

        match TopLevel.GetTopLevel this with
        | null -> ()
        | top ->
            let handler = EventHandler(fun _ _ -> rescale ())
            top.ScalingChanged.AddHandler handler
            scalingWatch <- { new IDisposable with member _.Dispose() = top.ScalingChanged.RemoveHandler handler }

        rescale ()

    override _.OnDetachedFromVisualTree(e) =
        base.OnDetachedFromVisualTree e

        if not (isNull scalingWatch) then
            scalingWatch.Dispose()
            scalingWatch <- null

    override _.OnPointerExited(e) =
        base.OnPointerExited e

        if pressed.IsNone then
            send "leave" [||]

    interface IDisposable with
        member _.Dispose() =
            if not (isNull image) then
                image.Dispose()

            if gpu.IsValueCreated then
                match gpu.Value with
                | Ok r -> (r :> IDisposable).Dispose()
                | Error _ -> ()

            (measurer :> IDisposable).Dispose()
