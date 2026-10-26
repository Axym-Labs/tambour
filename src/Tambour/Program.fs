module Tambour.Program

open System.Runtime.CompilerServices
open Avalonia
open Avalonia.Controls

let private usage = "usage: tambour [--self-check | --worker <socket> <app pid>]"

// Each role is its own method, kept out of `main`, so that starting one loads only what that role
// touches: .NET loads an assembly when a method that names it is compiled, and the worker never
// names Avalonia.

[<MethodImpl(MethodImplOptions.NoInlining)>]
let private window (argv: string[]) =
    App.MainWindow <- fun () ->
        let measurer = new Marey.SkiaTextMeasurer()
        let node = Marey.Chart.toSceneWith measurer SelfCheck.extent (SelfCheck.figure ())
        Window(Title = "Tambour", SizeToContent = SizeToContent.WidthAndHeight,
               Content = FigureView(node, SelfCheck.extent, measurer))
    App.builder().StartWithClassicDesktopLifetime argv

[<MethodImpl(MethodImplOptions.NoInlining)>]
let private selfCheck () = SelfCheck.run ()

[<MethodImpl(MethodImplOptions.NoInlining)>]
let private worker (socket: string) (app: int) = Worker.run socket app

[<EntryPoint>]
let main argv =
    match argv with
    | [||] -> window argv
    | [| "--self-check" |] -> selfCheck ()
    | [| "--worker"; socket; app |] when fst (System.Int32.TryParse app) -> worker socket (int app)
    | [| "--help" | "-h" |] ->
        printfn "%s" usage
        0
    | _ ->
        eprintfn "%s" usage
        2
