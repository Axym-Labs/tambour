module Tambour.Program

open System.Runtime.CompilerServices
open Avalonia
open Avalonia.Controls

let private usage = "usage: tambour [--self-check | --check-pane [--scaling <s>] | --check-analysis <script> | --worker <socket> <app pid> | --analysis <socket> <app pid>]"

// Each role is its own method, kept out of `main`, so that starting one loads only what that role
// touches: .NET loads an assembly when a method that names it is compiled, and the worker never
// names Avalonia.

[<MethodImpl(MethodImplOptions.NoInlining)>]
let private window (argv: string[]) =
    App.MainWindow <- fun () -> new Shell.MainWindow() :> Window
    App.builder().StartWithClassicDesktopLifetime argv

[<MethodImpl(MethodImplOptions.NoInlining)>]
let private selfCheck () = SelfCheck.run ()

[<MethodImpl(MethodImplOptions.NoInlining)>]
let private checkPane (scaling: float option) = PaneCheck.run scaling

[<MethodImpl(MethodImplOptions.NoInlining)>]
let private worker (socket: string) (app: int) = Worker.run socket app

[<MethodImpl(MethodImplOptions.NoInlining)>]
let private checkAnalysis (script: string) = AnalysisCheck.run script

[<MethodImpl(MethodImplOptions.NoInlining)>]
let private analysis (socket: string) (app: int) = Analysis.run socket app

[<EntryPoint>]
let main argv =
    match argv with
    | [||] -> window argv
    | [| "--self-check" |] -> selfCheck ()
    | [| "--check-pane" |] -> checkPane None
    | [| "--check-analysis"; script |] -> checkAnalysis script
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
