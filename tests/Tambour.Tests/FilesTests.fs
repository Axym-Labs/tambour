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

/// An export shows that it is under way and is cancelled like a run: Cancel ends it, nothing is
/// written, and the next export is.
[<AvaloniaFact>]
let ``an export shows its progress and Cancel ends it`` () =
    let dir = (Directory.CreateTempSubdirectory "tambour-export-cancel").FullName
    let script = Path.Combine(dir, "decay.fsx")
    File.WriteAllText(script, decay)

    let window = new Shell.MainWindow("", Some dir)
    window.Show()
    pumpUntil 60.0 "the workers' start" (fun () -> window.Started.IsCompleted)
    let pane = window.GetVisualDescendants().OfType<FigurePane>().Single()
    pumpUntil 30.0 "the figure" (fun () -> not (isNull pane.Picture))
    let status (text: string) = window.GetVisualDescendants().OfType<TextBlock>().Any(fun t -> not (isNull t.Text) && t.Text = text)
    let busy () = window.GetVisualDescendants().OfType<ProgressBar>().Any(fun b -> b.IsVisible && b.IsIndeterminate)

    // Cancel straight after: no reply can have been taken yet, as they come on this thread.
    let cancelled = Path.Combine(dir, "cancelled.png")
    window.Post(Shell.ExportTo(cancelled, "png", 640.0, 400.0))
    Assert.True(status "exporting cancelled.png…", "no word of the export under way")
    Assert.True(busy (), "no sign of the export under way")
    window.Post Shell.Cancel
    Assert.True(status "export cancelled")
    Assert.False(busy ())
    // Its reply, if it was already on the way, is let go.
    pumpUntil 30.0 "the figure again" (fun () -> not (isNull pane.Picture))
    Thread.Sleep 500
    Dispatcher.UIThread.RunJobs()
    Assert.False(File.Exists cancelled)

    // The worker is back for the next one, which is written.
    let after = Path.Combine(dir, "after.png")
    window.Post(Shell.ExportTo(after, "png", 640.0, 400.0))
    pumpUntil 30.0 "the next export" (fun () -> File.Exists after && (try File.ReadAllBytes(after).Length > 0 with _ -> false))
    pumpUntil 5.0 "the status" (fun () -> status "exported after.png" && not (busy ()))
    window.Close()

/// The tabs on the right: the figure's, then one per table.
let private sheets (window: #Window) =
    window.GetVisualDescendants().OfType<TabControl>().Single(fun t -> t.Name = "sheets")

[<AvaloniaFact>]
let ``a table's edits run the figure again, rename a column in the prelude, undo, and save`` () =
    let dir = (Directory.CreateTempSubdirectory "tambour-tables").FullName
    let t = Array.init 30 (fun i -> float i * 0.1)

    let trials: Tambour.Data.Table =
        { Name = "trials"
          Columns =
            [ { Name = "Time"; Unit = "s"; Values = Tambour.Data.Floats t }
              { Name = "Displacement"; Unit = "m"; Values = Tambour.Data.Floats(t |> Array.map (fun t -> 4.9 * t * t)) } ] }

    Tambour.Data.Csv.writeFile (Path.Combine(dir, "trials.csv")) trials
    let script = "#load \"prelude.fsx\"\nopen Prelude\n\nlet fig =\n    plot trials {\n        x U.second (fun r -> r.Time)\n        y U.metre (fun r -> r.Displacement)\n        layer GeomPoint\n    }\n"
    File.WriteAllText(Path.Combine(dir, "figure.fsx"), script)
    Project.write dir (Project.defaults "figure.fsx")

    let window = new Shell.MainWindow(Shell.example, Some dir)
    window.Show()
    pumpUntil 60.0 "the workers' start" (fun () -> window.Started.IsCompleted)
    let pane = window.GetVisualDescendants().OfType<FigurePane>().Single()
    pumpUntil 60.0 "the table's figure" (fun () -> not (isNull pane.Picture))
    Assert.Contains("Displacement: float<m>", File.ReadAllText(Path.Combine(dir, "prelude.fsx")))

    // A cell typed into, in the table's tab: the figure runs again and the title marks it.
    window.Post(Shell.SelectTab 1)
    pumpUntil 10.0 "the table's tab" (fun () -> window.GetVisualDescendants().OfType<TableGrid>().Any())
    let current () = window.GetVisualDescendants().OfType<TableGrid>().Single()
    let grid = current ()
    let drawn = pane.Picture
    grid.Select({ Column = 1; Row = 0 }, { Column = 1; Row = 0 })
    grid.TypeInto "100"
    pumpUntil 30.0 "the figure run with the new value" (fun () -> not (obj.ReferenceEquals(pane.Picture, drawn)))
    Assert.EndsWith("• — Tambour", window.Title)

    let value () =
        match grid.Table with
        | Some t ->
            match t.Columns.[1].Values with
            | Tambour.Data.Floats a -> a.[0]
            | _ -> nan
        | None -> nan

    Assert.Equal(100.0, value ())
    window.KeyPress(Key.Z, RawInputModifiers.Control, PhysicalKey.Z, null)
    pumpUntil 10.0 "the value taken back" (fun () -> value () = 0.0)

    // A column renamed: the prelude says so, and the script no longer compiles until it is taken back.
    window.Post(Shell.EditHeader("trials", 1))
    window.Post(Shell.Typed("header name", "Height"))
    window.Post(Shell.ApplyHeader)
    let prelude () = File.ReadAllText(Path.Combine(dir, "prelude.fsx"))
    pumpUntil 10.0 "the prelude written again" (fun () -> prelude().Contains "Height: float<m>")
    pumpUntil 30.0 "the script's error" (fun () -> window.GetVisualDescendants().OfType<TextBlock>().Any(fun b -> not (isNull b.Text) && b.Text = "the script does not compile"))
    window.KeyPress(Key.Z, RawInputModifiers.Control, PhysicalKey.Z, null)
    pumpUntil 10.0 "the rename taken back" (fun () -> prelude().Contains "Displacement: float<m>")

    // And the script runs again, against the prelude as it was.
    let output () =
        window.GetVisualDescendants().OfType<TextBox>()
        |> Seq.map (fun b -> b.Text)
        |> Seq.filter (fun t -> not (isNull t) && t.Contains "FS0039")
        |> String.concat " | "

    (try
        pumpUntil 30.0 "the script run cleanly again" (fun () ->
            window.GetVisualDescendants().OfType<TextBlock>().Any(fun b -> not (isNull b.Text) && b.Text.StartsWith "ran in") && output () = "")
     with e -> failwith $"{e.Message}: {output ()}")

    // Saved: the table's file holds the value typed again.
    // The grid is the one there before the header's form opened and closed.
    Assert.True(obj.ReferenceEquals(grid, current ()), "the grid was made again")
    grid.Select({ Column = 1; Row = 2 }, { Column = 1; Row = 2 })
    grid.TypeInto "7.25"
    pumpUntil 10.0 "the value" (fun () -> grid.Table.Value.Columns.[1].Values.Equals(Tambour.Data.Values.replace 2 (Tambour.Data.Floats [| 7.25 |]) trials.Columns.[1].Values))
    let status () = window.GetVisualDescendants().OfType<TextBlock>() |> Seq.map (fun b -> b.Text) |> Seq.filter (fun t -> not (isNull t) && (t.StartsWith "ran" || t.StartsWith "running" || t.Contains "compile")) |> String.concat " | "

    (try
        pumpUntil 30.0 "the edited table's run, clean" (fun () -> status().StartsWith "ran in" && output () = "")
     with e -> failwith $"{e.Message}: status {status ()}; {output ()}")

    window.KeyPress(Key.S, RawInputModifiers.Control, PhysicalKey.S, null)
    pumpUntil 10.0 "the save" (fun () -> not (window.Title.Contains "•"))

    (try
        pumpUntil 30.0 "the saved script run cleanly" (fun () ->
            window.GetVisualDescendants().OfType<TextBlock>().Any(fun b -> not (isNull b.Text) && b.Text.StartsWith "ran in") && output () = "")
     with e -> failwith $"{e.Message}: {output ()}")

    match Environment.GetEnvironmentVariable "TAMBOUR_CHECK_OUT" with
    | null | "" -> ()
    | out ->
        use frame = window.CaptureRenderedFrame()
        frame.Save(Path.Combine(out, "shell-table.png"), Avalonia.Media.Imaging.PngBitmapEncoderOptions.Default)

    match Tambour.Data.Csv.readFile (Path.Combine(dir, "trials.csv")) with
    | Ok back -> Assert.Equal(Tambour.Data.Values.replace 2 (Tambour.Data.Floats [| 7.25 |]) trials.Columns.[1].Values, back.Columns.[1].Values)
    | Error e -> failwith e

    // A table imported from elsewhere gets a tab of its own, and goes when removed.
    let outside = Path.Combine(Path.GetTempPath(), $"readings-{Guid.NewGuid():N}.csv")
    File.WriteAllText(outside, "a,b\n1,2\n3,4\n")
    window.Post(Shell.Import outside)
    pumpUntil 10.0 "the imported table's tab" (fun () -> (sheets window).Items.OfType<TabItem>().Count() = 3)
    let name = (sheets window).Items.OfType<TabItem>().Last().Header :?> string
    Assert.StartsWith("readings", name)
    window.Post(Shell.RemoveTable name)
    pumpUntil 10.0 "the table removed" (fun () -> (sheets window).Items.OfType<TabItem>().Count() = 2)
    File.Delete outside
    window.Close()

[<AvaloniaFact>]
let ``two columns selected are plotted, and a row picked in the figure is marked in the grid`` () =
    let dir = (Directory.CreateTempSubdirectory "tambour-plotpick").FullName
    let t = Array.init 30 (fun i -> float i * 0.1)

    let trials: Tambour.Data.Table =
        { Name = "trials"
          Columns =
            [ { Name = "Time"; Unit = "s"; Values = Tambour.Data.Floats t }
              { Name = "Height"; Unit = "m"; Values = Tambour.Data.Floats(t |> Array.map (fun t -> 50.0 - 4.9 * t * t)) } ] }

    Tambour.Data.Csv.writeFile (Path.Combine(dir, "trials.csv")) trials
    File.WriteAllText(Path.Combine(dir, "figure.fsx"), "// Figures of the trials.\n")
    Project.write dir (Project.defaults "figure.fsx")

    let window = new Shell.MainWindow(Shell.example, Some dir)
    window.Show()
    pumpUntil 60.0 "the workers' start" (fun () -> window.Started.IsCompleted)
    let editor = window.GetVisualDescendants().OfType<ScriptEditor>().Single()
    pumpUntil 30.0 "the project's script" (fun () -> editor.Text.StartsWith "// Figures")

    // Both columns selected in the grid, then plotted: a block written, run, and shown.
    window.Post(Shell.SelectTab 1)
    pumpUntil 10.0 "the table's tab" (fun () -> window.GetVisualDescendants().OfType<TableGrid>().Any())
    let grid = window.GetVisualDescendants().OfType<TableGrid>().Single()
    grid.Select({ Column = 0; Row = 0 }, { Column = 1; Row = 0 })
    window.Post Shell.PlotSelected
    pumpUntil 10.0 "the block written" (fun () -> editor.Text.Contains "plot trials {")
    Assert.Contains("x U.second (fun r -> r.Time)", editor.Text)
    Assert.Contains("y U.metre (fun r -> r.Height)", editor.Text)
    let pane = window.GetVisualDescendants().OfType<FigurePane>().Single()
    pumpUntil 60.0 "the figure" (fun () -> not (isNull pane.Picture))

    // An area around the whole plot picks every row; each is ringed in the figure.
    window.Post(Shell.Pick(Avalonia.Rect(0.0, 0.0, float pane.Picture.Extent.Width, float pane.Picture.Extent.Height)))
    pumpUntil 30.0 "every row ringed" (fun () -> pane.Marks.Length = 30)

    // Ctrl+click on one ring picks its row alone, and the grid marks it.
    let ring = pane.Marks.[7]
    let at = (Avalonia.VisualExtensions.TranslatePoint(pane, ring, window)).Value
    window.MouseDown(at, MouseButton.Left, RawInputModifiers.Control)
    window.MouseUp(at, MouseButton.Left, RawInputModifiers.Control)
    pumpUntil 30.0 "the row picked" (fun () -> pane.Marks.Length = 1)
    window.Post(Shell.SelectTab 1)
    pumpUntil 10.0 "the grid shown again" (fun () -> window.GetVisualDescendants().OfType<TableGrid>().Any())
    Assert.Equal<Set<int>>(set [ 7 ], window.GetVisualDescendants().OfType<TableGrid>().Single().Marked)
    window.Close()

/// Columns with no unit, as a CSV from elsewhere has them, plotted: the block written for a float
/// and for an int column compiles and draws, as it does for columns with units.
[<AvaloniaFact>]
let ``columns without units are plotted, and the block compiles`` () =
    let dir = (Directory.CreateTempSubdirectory "tambour-unitless").FullName

    let runs: Tambour.Data.Table =
        { Name = "runs"
          Columns =
            [ { Name = "step"; Unit = null; Values = Tambour.Data.Ints(Array.init 20 id) }
              { Name = "auc"; Unit = null; Values = Tambour.Data.Floats(Array.init 20 (fun i -> 0.5 + 0.02 * float i)) }
              { Name = "loss"; Unit = null; Values = Tambour.Data.Floats(Array.init 20 (fun i -> 1.0 / float (i + 1))) } ] }

    Tambour.Data.Csv.writeFile (Path.Combine(dir, "runs.csv")) runs
    File.WriteAllText(Path.Combine(dir, "figure.fsx"), "// Figures of the runs.\n")
    Project.write dir (Project.defaults "figure.fsx")
    let window = new Shell.MainWindow("", Some dir)
    window.Show()
    pumpUntil 60.0 "the workers' start" (fun () -> window.Started.IsCompleted)
    let editor () = window.GetVisualDescendants().OfType<ScriptEditor>().Single()
    pumpUntil 30.0 "the project's script" (fun () -> (editor ()).Text.StartsWith "// Figures")
    let status () = window.GetVisualDescendants().OfType<TextBlock>() |> Seq.map (fun b -> b.Text) |> Seq.filter (fun t -> not (isNull t) && (t.StartsWith "ran in" || t.Contains "compile")) |> String.concat " | "
    let pane () = window.GetVisualDescendants().OfType<FigurePane>().Single()

    for columns, figure in [ [ 1; 2 ], "fig"; [ 0; 1 ], "fig2" ] do
        window.Post(Shell.SelectTab 1)
        pumpUntil 10.0 "the table's tab" (fun () -> window.GetVisualDescendants().OfType<TableGrid>().Any())
        let grid = window.GetVisualDescendants().OfType<TableGrid>().Single()
        grid.Select({ Column = List.head columns; Row = 0 }, { Column = List.last columns; Row = 0 })
        window.Post Shell.PlotSelected
        pumpUntil 10.0 "the block written" (fun () -> (editor ()).Text.Contains $"let {figure} =")

        (try
            pumpUntil 30.0 "a clean run" (fun () -> status().StartsWith "ran in")
         with e -> failwith $"{e.Message}: {status ()}; {(editor ()).Text}")

        pumpUntil 30.0 "the figure" (fun () -> not (isNull (pane ()).Picture))

    window.Close()

/// A project with tables but no script yet opens a new script, not the prelude Tambour wrote for
/// the tables; a project's own script is opened before it, whatever their names.
[<Fact>]
let ``a project opens its own script or a new one, never the prelude`` () =
    let dir = (Directory.CreateTempSubdirectory "tambour-noscript").FullName
    File.WriteAllText(Path.Combine(dir, "prelude.fsx"), "module Prelude\n")
    Assert.Equal(Path.Combine(dir, "figure.fsx"), (Project.resolve dir).Script)
    Assert.Empty(Project.scripts dir)

    File.WriteAllText(Path.Combine(dir, "zeta.fsx"), "let x = 1\n")
    Assert.Equal(Path.Combine(dir, "zeta.fsx"), (Project.resolve dir).Script)
    Assert.Equal<string[]>([| "zeta.fsx" |], Project.scripts dir)
