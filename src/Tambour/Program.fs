module Tambour.Program

open System.Runtime.CompilerServices
open Avalonia
open Avalonia.Controls

let private usage = "usage: tambour [<script or folder> | --self-check | --check-pane [--scaling <s>] | --check-analysis <script> | --check-package | --check-scaling | --thumbnail <bundle> <png> [<size>] | --worker <socket> <app pid> | --analysis <socket> <app pid>]"

// Each role is its own method, kept out of `main`, so that starting one loads only what that role
// touches: .NET loads an assembly when a method that names it is compiled, and the worker never
// names Avalonia.

[<MethodImpl(MethodImplOptions.NoInlining)>]
let private window (path: string option) =
    let settings = Paths.configDirectory ()
    App.SettingsDirectory <- settings
    let preferences = Preferences.read settings
    App.Theme <- preferences.Theme
    Preferences.Live.Current <- preferences

    // The app's desktop entry and icon, by which a Wayland shell shows its windows: written as the
    // window is made, once the app's assets can be read.
    let entry () =
        if System.OperatingSystem.IsLinux() then
            use stream = Platform.AssetLoader.Open(System.Uri "avares://tambour/Assets/tambour.png")
            use bytes = new System.IO.MemoryStream()
            stream.CopyTo bytes
            let icon = bytes.ToArray()

            System.Threading.Tasks.Task.Run(fun () ->
                try
                    Association.entry (Association.dataDirectory ()) (Association.executable ()) icon |> ignore
                with _ ->
                    ())
            |> ignore

    App.MainWindow <-
        fun () ->
            (try entry () with _ -> ())
            Shell.MainWindow.Start path :> Window

    // Each screen's scaling, which Avalonia under XWayland cannot find out for itself.
    Scaling.apply () |> ignore
    App.builder().StartWithClassicDesktopLifetime [||]

[<MethodImpl(MethodImplOptions.NoInlining)>]
let private selfCheck () = SelfCheck.run ()

[<MethodImpl(MethodImplOptions.NoInlining)>]
let private checkPane (scaling: float option) = PaneCheck.run scaling

[<MethodImpl(MethodImplOptions.NoInlining)>]
let private worker (socket: string) (app: int) = Worker.run socket app

[<MethodImpl(MethodImplOptions.NoInlining)>]
let private checkPackage () = PackageCheck.run ()

[<MethodImpl(MethodImplOptions.NoInlining)>]
let private checkScaling () = Scaling.check ()

/// A bundle's figure for a file manager's thumbnailer: written as a PNG at most `size` px on its
/// longer side; 1 where the bundle keeps none.
[<MethodImpl(MethodImplOptions.NoInlining)>]
let private thumbnail (bundle: string) (out: string) (size: int option) =
    match Bundle.thumbnail bundle with
    | None -> 1
    | Some png ->
        use image = SkiaSharp.SKBitmap.Decode png
        let longer = max image.Width image.Height

        match size with
        | Some s when s > 0 && s < longer ->
            let scale = float s / float longer
            let info = SkiaSharp.SKImageInfo(max 1 (int (float image.Width * scale)), max 1 (int (float image.Height * scale)))
            use smaller = image.Resize(info, SkiaSharp.SKSamplingOptions(SkiaSharp.SKCubicResampler.Mitchell))
            use data = smaller.Encode(SkiaSharp.SKEncodedImageFormat.Png, 100)
            System.IO.File.WriteAllBytes(out, data.ToArray())
        | _ -> System.IO.File.WriteAllBytes(out, png)

        0

[<MethodImpl(MethodImplOptions.NoInlining)>]
let private checkAnalysis (script: string) = AnalysisCheck.run script

[<MethodImpl(MethodImplOptions.NoInlining)>]
let private analysis (socket: string) (app: int) = Analysis.run socket app

[<EntryPoint>]
let main argv =
    match argv with
    | [||] -> window None
    | [| path |] when not (path.StartsWith "-") -> window (Some path)
    | [| "--self-check" |] -> selfCheck ()
    | [| "--check-pane" |] -> checkPane None
    | [| "--check-analysis"; script |] -> checkAnalysis script
    | [| "--check-package" |] -> checkPackage ()
    | [| "--check-scaling" |] -> checkScaling ()
    | [| "--thumbnail"; bundle; out |] -> thumbnail bundle out None
    | [| "--thumbnail"; bundle; out; size |] when fst (System.Int32.TryParse size) -> thumbnail bundle out (Some(int size))
    | [| "--check-pane"; "--scaling"; s |] when fst (System.Double.TryParse(s, System.Globalization.CultureInfo.InvariantCulture)) ->
        checkPane (Some(System.Double.Parse(s, System.Globalization.CultureInfo.InvariantCulture)))
    | [| "--worker"; socket; app |] when fst (System.Int32.TryParse app) -> worker socket (int app)
    | [| "--analysis"; socket; app |] when fst (System.Int32.TryParse app) -> analysis socket (int app)
    | [| "--help" | "-h" |] ->
        printfn "%s" usage
        0
    | _ ->
        eprintfn "%s" usage
        2
