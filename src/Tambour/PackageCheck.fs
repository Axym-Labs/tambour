/// `tambour --check-package`: the app as a user gets it, end to end in a real window. It opens a
/// script, draws it, shows its unit error once one is typed, takes that back, applies an action,
/// takes that back, and exports a PNG, which must be what Marey's `renderPng` draws of the same
/// figure, evaluated here on the bundled FCS. The exit code is 0 when every step held. Run on an
/// unpacked package in a sandbox with no .NET (`tools/no-dotnet.sh`), it is the release check.
module Tambour.PackageCheck

open System
open System.Diagnostics
open System.IO
open System.Linq
open System.Threading.Tasks
open Avalonia
open Avalonia.Controls
open Avalonia.Threading
open Avalonia.VisualTree
open SkiaSharp
open Tambour.Protocol

let private script =
    "let t = Array.init 50 (fun i -> float i * 0.1<s>)\nlet fig = Chart.lineIn U.second U.metre t (t |> Array.map (fun t -> 1.0<m> * exp (-t / 2.0<s>)))\n"

/// The edit that plots metres under seconds.
let private wrongUnit = "U.metre t", "U.second t"

let private say (line: string) = eprintfn "%s" line

/// Waits on the UI thread until `ready` holds, or fails after `seconds`.
let private until (seconds: float) (what: string) (ready: unit -> bool) : Task =
    task {
        let clock = Stopwatch.StartNew()

        while not (ready ()) do
            if clock.Elapsed.TotalSeconds > seconds then
                failwith $"{what} did not happen within {seconds} s"

            do! Task.Delay 20
    }

/// The figure the script binds, evaluated here rather than by a worker.
let private reference (path: string) : Marey.Figure =
    let session = Evaluation.Session [||]
    session.Start Shell.prelude |> ignore
    let run = session.Run(script, path)

    match run.Found with
    | found :: _ when run.Ok -> found.Live.View [||]
    | _ -> failwith "the script did not evaluate here"

let private check (window: Shell.MainWindow) (path: string) : Task<bool> =
    task {
        let clock = Stopwatch.StartNew()
        let step (what: string) = say $"holds  {what} ({clock.Elapsed.TotalSeconds:F1} s)"
        let pane = window.GetVisualDescendants().OfType<FigurePane>().Single()
        let editor = window.GetVisualDescendants().OfType<ScriptEditor>().Single()

        do! until 60.0 "the script drawn" (fun () -> not (isNull pane.Picture) && editor.Text = script)
        step "opened the script and drew it"

        let wrong, right = wrongUnit
        let at = editor.Text.IndexOf wrong
        editor.Editor.Document.Replace(at, wrong.Length, right)
        do! until 30.0 "the unit error shown" (fun () -> editor.Problems |> Array.exists (fun d -> d.Code = "FS0001"))
        step $"""showed the unit error: {(editor.Problems |> Array.find (fun d -> d.Code = "FS0001")).Message.Split('\n').[0]}"""

        window.Post Shell.Undo
        do! until 30.0 "the error taken back" (fun () -> editor.Text = script && editor.Problems.Length = 0)
        step "took the error back"

        let drawn = pane.Picture
        window.Post(Shell.Act { Kind = "title"; Text = "Decay"; Lo = 0.0; Hi = 0.0 })
        do! until 30.0 "the action written" (fun () -> editor.Text.Contains "Chart.withTitle \"Decay\"")
        do! until 30.0 "the action drawn" (fun () -> not (obj.ReferenceEquals(pane.Picture, drawn)))
        step "applied an action and drew it"

        let titled = pane.Picture
        window.Post Shell.Undo
        do! until 30.0 "the action taken back" (fun () -> editor.Text = script)
        do! until 30.0 "the figure drawn again" (fun () -> not (obj.ReferenceEquals(pane.Picture, titled)))
        step "took the action back"

        // Off the UI thread: FCS waits synchronously, which deadlocks on the UI thread's context.
        let! figure = Task.Run(fun () -> reference path)
        let png = Path.Combine(Path.GetDirectoryName path, "export.png")
        window.Post(Shell.ExportTo(png, "png", 640.0, 400.0))
        do! until 30.0 "the export written" (fun () -> File.Exists png && (try File.ReadAllBytes(png).Length > 0 with _ -> false))
        let extent = Marey.Chart.size 640.0 400.0
        use measurer = new Marey.SkiaTextMeasurer()
        let expected = Marey.SkiaBackend.renderPng extent (Marey.Chart.toSceneWith measurer extent figure)
        use a = SKBitmap.Decode(File.ReadAllBytes png)
        use b = SKBitmap.Decode expected
        let c = PaneCheck.compare a b

        if c.Worst = 0 then
            step "exported a PNG that renderPng draws, pixel for pixel"
            return true
        else
            say $"FAILS  the exported PNG differs from renderPng by up to {c.Worst}/255"
            return false
    }

let run () : int =
    let mutable exitCode = 1

    try
        let dir = Directory.CreateTempSubdirectory "tambour-check"
        let path = Path.Combine(dir.FullName, "decay.fsx")
        File.WriteAllText(path, script)

        App.MainWindow <- fun () ->
            let window = new Shell.MainWindow(Shell.example, Some path)

            window.Opened.Add(fun _ ->
                Dispatcher.UIThread.Post(fun () ->
                    task {
                        try
                            let! ok = check window path
                            exitCode <- if ok then 0 else 1
                        with e ->
                            say $"FAILS  {e.Message}"
                            exitCode <- 1

                        window.Close()
                    }
                    |> ignore))

            window :> Window

        App.builder().StartWithClassicDesktopLifetime [||] |> ignore
    with e ->
        say $"the check failed before it began: {e}"
        exitCode <- 1

    exitCode
