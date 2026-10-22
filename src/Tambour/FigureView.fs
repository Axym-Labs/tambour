namespace Tambour

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Media
open Avalonia.Platform
open Avalonia.Rendering.SceneGraph
open Avalonia.Skia

/// What a paint of the figure pane came to: the canvas's GPU backend ("OpenGL", or "CPU" for a
/// raster canvas), or the exception the painter raised.
type PaintResult =
    | Painted of backend: string
    | PaintFailed of exn

/// Paints a laid-out Marey scene on Avalonia's own Skia canvas, through the same painter that
/// writes Marey's PNG and PDF exports.
type private SceneDrawOp(bounds: Rect, node: Marey.Node, extent: Marey.Extent<Marey.px>, measurer: Marey.SkiaTextMeasurer,
                         report: PaintResult -> unit) =
    interface ICustomDrawOperation with
        member _.Bounds = bounds
        member _.HitTest _ = false
        member _.Equals(_: ICustomDrawOperation) = false
        member _.Dispose() = ()
        member _.Render(context) =
            match context.TryGetFeature<ISkiaSharpApiLeaseFeature>() with
            | null -> report (PaintFailed(InvalidOperationException "the renderer offers no Skia canvas"))
            | feature ->
                try
                    use lease = feature.Lease()
                    // Text as outlines, as the PNG export draws it, so that the pane shows what an
                    // export writes rather than what this platform's font engine hints.
                    let painter = Marey.SkiaPainter(lease.SkCanvas, measurer, true, None, outlines = true)
                    painter.Draw(Marey.SkiaPainter.RootTransform extent, node)
                    let backend = if isNull lease.GrContext then "CPU" else string lease.GrContext.Backend
                    report (Painted backend)
                with e ->
                    report (PaintFailed e)

/// A control showing one scene at its laid-out extent.
type FigureView(node: Marey.Node, extent: Marey.Extent<Marey.px>, measurer: Marey.SkiaTextMeasurer) =
    inherit Control()

    let painted = Event<PaintResult>()

    /// Raised on the render thread after each paint.
    [<CLIEvent>]
    member _.Painted = painted.Publish

    override _.MeasureOverride _ = Size(float extent.Width, float extent.Height)

    override this.Render(context) =
        let bounds = Rect(0.0, 0.0, float extent.Width, float extent.Height)
        context.Custom(new SceneDrawOp(bounds, node, extent, measurer, painted.Trigger))
