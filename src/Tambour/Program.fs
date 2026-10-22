module Tambour.Program

open Avalonia
open Avalonia.Controls

let private usage = "usage: tambour [--self-check]"

[<EntryPoint>]
let main argv =
    match argv with
    | [||] ->
        App.MainWindow <- fun () ->
            let measurer = new Marey.SkiaTextMeasurer()
            let node = Marey.Chart.toSceneWith measurer SelfCheck.extent (SelfCheck.figure ())
            Window(Title = "Tambour", SizeToContent = SizeToContent.WidthAndHeight,
                   Content = FigureView(node, SelfCheck.extent, measurer))
        App.builder().StartWithClassicDesktopLifetime argv
    | [| "--self-check" |] -> SelfCheck.run ()
    | [| "--help" | "-h" |] ->
        printfn "%s" usage
        0
    | _ ->
        eprintfn "%s" usage
        2
