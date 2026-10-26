/// Opening, saving, a change made on disk, and export, in the window with real workers behind it.
[<Xunit.Collection("workers")>]
module Tambour.Tests.FilesTests

open System
open System.Diagnostics
open System.IO
open System.Linq
open System.Threading
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Headless.XUnit
open Avalonia.Input
open Avalonia.Threading
open Avalonia.VisualTree
open Marey
open Marey.UnitsOfMeasure
open SkiaSharp
open Tambour
open Xunit

let private pumpUntil (seconds: float) (what: string) (ready: unit -> bool) =
    let clock = Stopwatch.StartNew()

    while not (ready ()) do
        if clock.Elapsed.TotalSeconds > seconds then
            failwith $"{what} did not happen within {seconds} s"

        Dispatcher.UIThread.RunJobs()
        Thread.Sleep 10

let private decay =
    "let t = Array.init 50 (fun i -> float i * 0.1<s>)\nlet fig = Chart.lineIn U.second U.metre t (t |> Array.map (fun t -> 1.0<m> * exp (-t / 2.0<s>)))\n"

/// The same figure, built here, for the export to be compared with.
let private decayFigure () =
    let t = Array.init 50 (fun i -> float i * 0.1<s>)
    Chart.lineIn U.second U.metre t (t |> Array.map (fun t -> 1.0<m> * exp (-t / 2.0<s>)))

[<AvaloniaFact>]
let ``a project opens, saves, takes a change from disk back, and exports what renderPng draws`` () =
    let dir = (Directory.CreateTempSubdirectory "tambour-project").FullName
    let script = Path.Combine(dir, "decay.fsx")
    File.WriteAllText(script, decay)
    File.WriteAllText(Path.Combine(dir, "another.fsx"), "let x = 1\n")

    Project.write dir { Project.defaults "decay.fsx" with Marey = "0.9.0" }

    let window = new Shell.MainWindow(Shell.example, Some dir)
    window.Show()
    pumpUntil 60.0 "the workers' start" (fun () -> window.Started.IsCompleted)
    let pane = window.GetVisualDescendants().OfType<FigurePane>().Single()
    let editor = window.GetVisualDescendants().OfType<ScriptEditor>().Single()
    pumpUntil 30.0 "the project's figure" (fun () -> not (isNull pane.Picture) && editor.Text = decay)
    Assert.Equal("decay.fsx — Tambour", window.Title)
    Assert.True(window.GetVisualDescendants().OfType<TextBlock>().Any(fun t -> not (isNull t.Text) && t.Text.Contains "last run on Marey 0.9.0"))

    // Typed, the title marks it unsaved; saved, the file and the project say what is there now.
    editor.Editor.CaretOffset <- editor.Text.Length
    editor.Editor.TextArea.PerformTextInput "// saved\n"
    pumpUntil 10.0 "the unsaved mark" (fun () -> window.Title = "decay.fsx • — Tambour")
    window.KeyPress(Key.S, RawInputModifiers.Control, PhysicalKey.S, null)
    pumpUntil 10.0 "the save" (fun () -> File.ReadAllText script = decay + "// saved\n")
    pumpUntil 10.0 "the mark cleared" (fun () -> window.Title = "decay.fsx — Tambour")
    Assert.Equal(Project.mareyVersion, (Project.read dir).Value.Marey)

    // A change on disk is reloaded as one entry of the history, and taken back by Ctrl+Z.
    let saved = File.ReadAllText script
    File.WriteAllText(script, saved.Replace("2.0<s>", "4.0<s>"))
    pumpUntil 10.0 "the reload" (fun () -> editor.Text.Contains "4.0<s>")
    window.KeyPress(Key.Z, RawInputModifiers.Control, PhysicalKey.Z, null)
    pumpUntil 10.0 "the reload taken back" (fun () -> editor.Text = saved)

    // Exported, the figure is what Marey's own PNG export draws of it, once the run that the undo
    // started has drawn it: exported until it is, rather than waiting on a scene that may already
    // have arrived.
    let extent = Chart.size 640.0 400.0
    use measurer = new SkiaTextMeasurer()
    use expected = SKBitmap.Decode(SkiaBackend.renderPng extent (Chart.toSceneWith measurer extent (decayFigure ())))
    let clock = Stopwatch.StartNew()
    let mutable worst = Int32.MaxValue
    let mutable attempt = 0

    while worst <> 0 && clock.Elapsed.TotalSeconds < 30.0 do
        attempt <- attempt + 1
        let png = Path.Combine(dir, $"decay-{attempt}.png")
        window.Post(Shell.ExportTo(png, "png", 640.0, 400.0))
        pumpUntil 30.0 "the export" (fun () -> File.Exists png && (try File.ReadAllBytes(png).Length > 0 with _ -> false))
        use exported = SKBitmap.Decode(File.ReadAllBytes png)
        worst <- (PaneCheck.compare exported expected).Worst
        if worst <> 0 then Thread.Sleep 200

    Assert.Equal(0, worst)
    window.Close()
