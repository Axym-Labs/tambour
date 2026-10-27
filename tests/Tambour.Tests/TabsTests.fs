/// Tabs, each a workspace, and the question before unsaved work is dropped (gate 15 (a), and of
/// (b) what needs no tables): in the window with real workers behind it.
[<Xunit.Collection("workers")>]
module Tambour.Tests.TabsTests

open System
open System.Diagnostics
open System.IO
open System.Linq
open System.Threading
open System.Threading.Tasks
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Headless.XUnit
open Avalonia.Input
open Avalonia.Threading
open Avalonia.VisualTree
open Tambour
open Xunit

let private pumpUntil (seconds: float) (what: string) (ready: unit -> bool) =
    let clock = Stopwatch.StartNew()

    while not (ready ()) do
        if clock.Elapsed.TotalSeconds > seconds then
            failwith $"{what} did not happen within {seconds} s"

        Dispatcher.UIThread.RunJobs()
        Thread.Sleep 10

let private press (window: Window) (key: Key) (modifiers: RawInputModifiers) =
    window.KeyPress(key, modifiers, PhysicalKey.None, null)
    window.KeyRelease(key, modifiers, PhysicalKey.None, null)
    Dispatcher.UIThread.RunJobs()

/// The editor of the workspace shown.
let private editorOf (window: Shell.MainWindow) =
    window.Active.GetVisualDescendants().OfType<ScriptEditor>().Single()

let private typeInto (window: Shell.MainWindow) (text: string) =
    let editor = editorOf window
    editor.Editor.CaretOffset <- editor.Text.Length
    editor.Editor.TextArea.PerformTextInput text
    Dispatcher.UIThread.RunJobs()

/// Closes the window, answering Don't save, and waits until it has closed, workers and all.
let private closeAll (window: Shell.MainWindow) =
    let closed = ref false
    window.Closed.Add(fun _ -> closed.Value <- true)
    window.Ask <- fun _ -> Task.FromResult Discard
    window.Close()
    pumpUntil 5.0 "the window closed" (fun () -> closed.Value)

/// Answers the question before unsaved work is dropped, counting each time it is asked.
let private answering (window: Shell.MainWindow) (answer: Unsaved) =
    let asked = ref 0

    window.Ask <-
        fun names ->
            asked.Value <- asked.Value + 1
            Assert.NotEmpty names
            Task.FromResult answer

    asked

[<AvaloniaFact>]
let ``each tab is a workspace of its own, and switching keeps each as it was`` () =
    let window = new Shell.MainWindow("let first = 1\n")
    window.Show()
    pumpUntil 60.0 "the workers' start" (fun () -> window.Started.IsCompleted)

    press window Key.N RawInputModifiers.Control
    Assert.Equal(2, window.Workspaces.Length)
    Assert.Same(window.Workspaces.[1], window.Active)
    Assert.Equal("", (editorOf window).Text)
    typeInto window "let second = 2\n"
    let caret = (editorOf window).Editor.CaretOffset

    // Ctrl+Tab round the ends, and back: each editor has its own text, and the caret stays.
    press window Key.Tab RawInputModifiers.Control
    Assert.Same(window.Workspaces.[0], window.Active)
    Assert.Equal("let first = 1\n", (editorOf window).Text)
    press window Key.Tab (RawInputModifiers.Control ||| RawInputModifiers.Shift)
    Assert.Same(window.Workspaces.[1], window.Active)
    Assert.Equal("let second = 2\n", (editorOf window).Text)
    Assert.Equal(caret, (editorOf window).Editor.CaretOffset)
    Assert.StartsWith("untitled •", window.Title)

    match Environment.GetEnvironmentVariable "TAMBOUR_CHECK_OUT" with
    | null
    | "" -> ()
    | dir ->
        use frame = window.CaptureRenderedFrame()
        frame.Save(Path.Combine(dir, "tabs.png"), Avalonia.Media.Imaging.PngBitmapEncoderOptions.Default)

    // A tab's history is its own: Undo here leaves the other tab's text alone.
    window.Post Shell.Undo
    Dispatcher.UIThread.RunJobs()
    Assert.Equal("", (editorOf window).Text)
    Assert.Equal("let first = 1\n", window.Workspaces.[0].GetVisualDescendants().OfType<ScriptEditor>().Single().Text)

    closeAll window

/// Gate 15 (a) for a tab: Ctrl+W on unsaved work asks; Cancel keeps the tab, its text and its
/// history; Don't save closes it; Save writes the file first.
[<AvaloniaFact>]
let ``closing a tab asks first, and Cancel keeps everything`` () =
    let dir = (Directory.CreateTempSubdirectory "tambour-tabs").FullName
    let file = Path.Combine(dir, "kept.fsx")
    File.WriteAllText(file, "let kept = 1\n")
    let window = new Shell.MainWindow("", Some file)
    window.Show()
    pumpUntil 60.0 "the workers' start" (fun () -> window.Started.IsCompleted)
    pumpUntil 10.0 "the script" (fun () -> (editorOf window).Text = "let kept = 1\n")

    // Nothing unsaved: Ctrl+W closes without asking, and an empty tab takes its place.
    press window Key.N RawInputModifiers.Control
    let asked = answering window Keep
    press window Key.W RawInputModifiers.Control
    pumpUntil 5.0 "the tab closed" (fun () -> window.Workspaces.Length = 1)
    Assert.Equal(0, asked.Value)

    // Unsaved, and Cancel: the tab stays, as it was.
    typeInto window "// more\n"
    let text = (editorOf window).Text
    press window Key.W RawInputModifiers.Control
    Dispatcher.UIThread.RunJobs()
    Assert.Equal(1, asked.Value)
    Assert.Equal(1, window.Workspaces.Length)
    Assert.Equal(text, (editorOf window).Text)
    window.Post Shell.Undo
    Dispatcher.UIThread.RunJobs()
    Assert.Equal("let kept = 1\n", (editorOf window).Text)
    window.Post Shell.Redo
    Dispatcher.UIThread.RunJobs()

    // Save: the file is written, then the tab goes.
    let asked = answering window SaveIt
    press window Key.W RawInputModifiers.Control
    pumpUntil 5.0 "the saved tab closed" (fun () -> window.Workspaces.Single().Path.IsNone)
    Assert.Equal(1, asked.Value)
    Assert.Equal(text, File.ReadAllText file)

    // Don't save: the tab goes, and the file keeps what was saved.
    window.Post(Shell.OpenTab file)
    pumpUntil 10.0 "the script again" (fun () -> (editorOf window).Text = text)
    typeInto window "// dropped\n"
    let asked = answering window Discard
    press window Key.W RawInputModifiers.Control
    pumpUntil 5.0 "the tab dropped" (fun () -> window.Workspaces.Single().Path.IsNone)
    Assert.Equal(1, asked.Value)
    Assert.Equal(text, File.ReadAllText file)
    closeAll window

/// Gate 15 (a) for the window: Quit and closing ask about each tab's unsaved work; Cancel keeps
/// the window open with its tabs as they were.
[<AvaloniaFact>]
let ``quitting asks about every unsaved tab, and Cancel keeps the window`` () =
    let window = new Shell.MainWindow("")
    window.Show()
    pumpUntil 60.0 "the workers' start" (fun () -> window.Started.IsCompleted)
    let closed = ref false
    window.Closed.Add(fun _ -> closed.Value <- true)
    typeInto window "let a = 1\n"
    press window Key.N RawInputModifiers.Control
    typeInto window "let b = 2\n"

    let asked = answering window Keep
    press window Key.Q RawInputModifiers.Control
    pumpUntil 5.0 "the question" (fun () -> asked.Value = 1)
    Dispatcher.UIThread.RunJobs()
    Assert.False closed.Value
    Assert.Equal(2, window.Workspaces.Length)
    Assert.Equal("let a = 1\n", window.Workspaces.[0].GetVisualDescendants().OfType<ScriptEditor>().Single().Text)

    // Don't save, asked once for each unsaved tab.
    let asked = answering window Discard
    window.Close()
    pumpUntil 5.0 "the window closed" (fun () -> closed.Value)
    Assert.Equal(2, asked.Value)

/// Opening a file goes to its tab where one shows it, takes an untouched tab, and otherwise opens
/// a new one.
[<AvaloniaFact>]
let ``opening takes an untouched tab, and finds a tab already showing the file`` () =
    let dir = (Directory.CreateTempSubdirectory "tambour-open").FullName
    let a = Path.Combine(dir, "a.fsx")
    let b = Path.Combine(dir, "b.fsx")
    File.WriteAllText(a, "let a = 1\n")
    File.WriteAllText(b, "let b = 2\n")
    let window = new Shell.MainWindow("")
    window.Show()
    pumpUntil 60.0 "the workers' start" (fun () -> window.Started.IsCompleted)

    window.Post(Shell.OpenTab a)
    pumpUntil 10.0 "a in the untouched tab" (fun () -> window.Active.Path = Some a)
    Assert.Equal(1, window.Workspaces.Length)

    window.Post(Shell.OpenTab b)
    pumpUntil 10.0 "b in a tab of its own" (fun () -> window.Workspaces.Length = 2 && window.Active.Path = Some b)

    window.Post(Shell.OpenTab a)
    Dispatcher.UIThread.RunJobs()
    Assert.Equal(2, window.Workspaces.Length)
    Assert.Equal(Some a, window.Active.Path)
    closeAll window

let private trial (name: string) (column: string) : Tambour.Data.Table =
    { Name = name
      Columns = [ { Name = column; Unit = null; Values = Tambour.Data.Floats [| 1.0; 2.0 |] } ] }

/// A workspace's directory: its script, its tables in `tables/`, and `workspace.json`. It opens
/// with those tables, its prelude reads them from there, and saving keeps them there.
[<AvaloniaFact>]
let ``a workspace keeps its tables in tables/ and its settings in workspace.json`` () =
    let dir = (Directory.CreateTempSubdirectory "tambour-workspace").FullName
    Project.makeWorkspace dir
    Tambour.Data.Csv.writeFile (Path.Combine(dir, "tables", "runs.csv")) (trial "runs" "auc")
    File.WriteAllText(Path.Combine(dir, "figure.fsx"), Shell.newScriptWithTables + "let n = runs.Length\n")
    let window = new Shell.MainWindow("", Some dir)
    window.Show()
    pumpUntil 60.0 "the workers' start" (fun () -> window.Started.IsCompleted)
    pumpUntil 10.0 "the script" (fun () -> (editorOf window).Text.Contains "runs.Length")

    let prelude = File.ReadAllText(Path.Combine(dir, "prelude.fsx"))
    Assert.Contains("\"tables/runs.csv\"", prelude)
    Assert.False(File.Exists(Path.Combine(dir, "runs.csv")))

    typeInto window "// saved\n"
    press window Key.S RawInputModifiers.Control
    pumpUntil 10.0 "the save" (fun () -> window.Active.Unsaved.IsEmpty)
    Assert.True(File.Exists(Path.Combine(dir, Project.WorkspaceFile)))
    Assert.False(File.Exists(Path.Combine(dir, Project.FileName)))
    Assert.True(File.Exists(Path.Combine(dir, "tables", "runs.csv")))
    Assert.False(File.Exists(Path.Combine(dir, "runs.csv")))
    closeAll window

/// A new tab given a table is saved as a workspace of its own: a directory named as its script.
[<AvaloniaFact>]
let ``a new workspace with a table saves as a directory of its own`` () =
    let parent = (Directory.CreateTempSubdirectory "tambour-new").FullName
    let window = new Shell.MainWindow("")
    window.Show()
    pumpUntil 60.0 "the workers' start" (fun () -> window.Started.IsCompleted)
    window.Post Shell.NewTable
    pumpUntil 10.0 "the table" (fun () -> not window.Active.Unsaved.IsEmpty)
    window.Post(Shell.SaveAs(Path.Combine(parent, "paper.fsx")))
    let dir = Path.Combine(parent, "paper")
    pumpUntil 10.0 "the save" (fun () -> window.Active.Unsaved.IsEmpty)
    Assert.Equal(Some(Path.Combine(dir, "paper.fsx")), window.Active.Path)
    Assert.True(File.Exists(Path.Combine(dir, "paper.fsx")))
    Assert.True(File.Exists(Path.Combine(dir, "tables", "table.csv")))
    Assert.True(File.Exists(Path.Combine(dir, Project.WorkspaceFile)))
    Assert.Contains("\"tables/table.csv\"", File.ReadAllText(Path.Combine(dir, "prelude.fsx")))
    closeAll window

/// Gate 15 (b): two tabs with different tables; each script compiles against its own, and a name
/// from the other's tables is not in scope.
[<AvaloniaFact>]
let ``two tabs each compile against their own tables only`` () =
    let workspace (name: string) (table: Tambour.Data.Table) (script: string) =
        let dir = Path.Combine((Directory.CreateTempSubdirectory "tambour-two").FullName, name)
        Directory.CreateDirectory dir |> ignore
        Project.makeWorkspace dir
        Tambour.Data.Csv.writeFile (Path.Combine(dir, "tables", table.Name + ".csv")) table
        File.WriteAllText(Path.Combine(dir, "figure.fsx"), Shell.newScriptWithTables + script)
        dir

    let a = workspace "a" (trial "alpha" "x") "let n = alpha.Length\nlet m = beta.Length\n"
    let b = workspace "b" (trial "beta" "y") "let n = beta.Length\nlet m = alpha.Length\n"
    let window = new Shell.MainWindow("", Some a)
    window.Show()
    pumpUntil 60.0 "the workers' start" (fun () -> window.Started.IsCompleted)
    window.Post(Shell.OpenTab b)
    pumpUntil 10.0 "the second tab" (fun () -> window.Workspaces.Length = 2)

    let problems () =
        pumpUntil 10.0 "the tab's editor" (fun () -> window.Active.GetVisualDescendants().OfType<ScriptEditor>().Any())
        let editor = editorOf window
        pumpUntil 60.0 "the check" (fun () -> editor.Problems.Length > 0)
        editor.Problems

    // Each knows its own table (line 4, under the three lines of the prelude's load) and not the
    // other's (line 5): b's tab is shown first, then a's.
    for other in [ "alpha"; "beta" ] do
        let found = problems ()
        Assert.All(found, fun p -> Assert.Equal(5, p.StartLine))
        Assert.Contains(found, fun p -> p.Message.Contains other)
        press window Key.Tab RawInputModifiers.Control

    closeAll window

/// Gate 15 (b): fifteen open tabs hold no workers until one runs, and then only that one; workers
/// start when a tab first runs.
[<AvaloniaFact>]
let ``fifteen tabs hold workers only where something ran`` () =
    let window = new Shell.MainWindow("let a = 1\n")
    window.Show()

    for _ in 2..15 do
        press window Key.N RawInputModifiers.Control

    Assert.Equal(15, window.Workspaces.Length)
    Assert.All(window.Workspaces, fun w -> Assert.False w.HasWorkers)

    typeInto window "let b = 2\n"
    press window Key.F5 RawInputModifiers.None
    pumpUntil 30.0 "the run" (fun () -> window.Active.HasWorkers && window.Active.Started.IsCompleted)
    Assert.Equal(1, window.Workspaces |> List.filter (fun w -> w.HasWorkers) |> List.length)
    closeAll window

/// A tab in the background gives its workers up once they are idle past the limit, and keeps its
/// figure; its next run starts them again with its tables, so a script that reads one still runs.
[<AvaloniaFact>]
let ``a tab in the background drops idle workers, and its next run has its tables again`` () =
    let limit, check = Shell.Workspace.IdleLimit, Shell.Workspace.IdleCheck
    Shell.Workspace.IdleLimit <- TimeSpan.FromMilliseconds 300.0
    Shell.Workspace.IdleCheck <- TimeSpan.FromMilliseconds 100.0

    try
        let dir = (Directory.CreateTempSubdirectory "tambour-idle").FullName
        Project.makeWorkspace dir
        Tambour.Data.Csv.writeFile (Path.Combine(dir, "tables", "runs.csv")) (trial "runs" "auc")

        let script =
            Shell.newScriptWithTables
            + "let fig =\n    plot runs {\n        x U.one (fun r -> r.auc)\n        y U.one (fun r -> r.auc)\n        layer GeomPoint\n    }\n"

        File.WriteAllText(Path.Combine(dir, "figure.fsx"), script)
        let window = new Shell.MainWindow("", Some dir)
        window.Show()
        let pane () = window.Active.GetVisualDescendants().OfType<FigurePane>().Single()
        pumpUntil 60.0 "the figure" (fun () -> window.Active.HasWorkers && not (isNull (pane ()).Picture))
        let first = window.Active

        // In the background and idle: its workers go.
        press window Key.N RawInputModifiers.Control
        pumpUntil 5.0 "the workers dropped" (fun () -> not first.HasWorkers)

        // Shown again: the figure is still there, with no worker behind it.
        press window Key.Tab RawInputModifiers.Control
        Assert.Same(first, window.Active)
        pumpUntil 5.0 "the pane" (fun () -> window.Active.GetVisualDescendants().OfType<FigurePane>().Any())
        let kept = (pane ()).Picture
        Assert.NotNull kept
        Assert.False first.HasWorkers

        // Run again: started, given the table, and the script reads it.
        press window Key.F5 RawInputModifiers.None
        pumpUntil 30.0 "the figure redrawn" (fun () -> not (obj.ReferenceEquals((pane ()).Picture, kept)) && not (isNull (pane ()).Picture))
        Assert.True first.HasWorkers
        Assert.DoesNotContain("•", window.Title)
        closeAll window
    finally
        Shell.Workspace.IdleLimit <- limit
        Shell.Workspace.IdleCheck <- check
