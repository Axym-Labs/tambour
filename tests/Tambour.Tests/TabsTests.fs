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

/// The editor of the workspace shown, once its tab is laid out.
let private editorOf (window: Shell.MainWindow) =
    pumpUntil 10.0 "the tab's editor" (fun () -> window.Active.GetVisualDescendants().OfType<ScriptEditor>().Any())
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
    pumpUntil 5.0 "the saved tab closed, the start view in its place" (fun () -> window.Starting)
    Assert.Equal(1, asked.Value)
    Assert.Equal(text, File.ReadAllText file)

    // Don't save: the tab goes, and the file keeps what was saved.
    window.Open file
    pumpUntil 10.0 "the script again" (fun () -> (editorOf window).Text = text)
    typeInto window "// dropped\n"
    let asked = answering window Discard
    press window Key.W RawInputModifiers.Control
    pumpUntil 5.0 "the tab dropped" (fun () -> window.Starting)
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
    File.WriteAllText(Path.Combine(dir, "figure.fsx"), "let n = runs.Length\n")
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
        File.WriteAllText(Path.Combine(dir, "figure.fsx"), script)
        dir

    let a = workspace "a" (trial "alpha" "x") "let n = alpha.Length\nlet m = beta.Length\n"
    let b = workspace "b" (trial "beta" "y") "let n = beta.Length\nlet m = alpha.Length\n"
    let window = new Shell.MainWindow("", Some a)
    window.Show()
    pumpUntil 60.0 "the workers' start" (fun () -> window.Started.IsCompleted)
    window.Post(Shell.OpenTab b)
    pumpUntil 10.0 "the second tab" (fun () -> window.Workspaces.Length = 2)

    let problems () =
        let editor = editorOf window
        pumpUntil 60.0 "the check" (fun () -> editor.Problems.Length > 0)
        editor.Problems

    // Each knows its own table (line 1), given it with no line of the script's, and not the
    // other's (line 2): b's tab is shown first, then a's.
    for other in [ "alpha"; "beta" ] do
        let found = problems ()
        Assert.All(found, fun p -> Assert.Equal(2, p.StartLine))
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
            "let fig =\n    plot runs {\n        x U.one (fun r -> r.auc)\n        y U.one (fun r -> r.auc)\n        layer GeomPoint\n    }\n"

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

/// Gate 15 (d): what was opened is listed most recent first, kept in the user's settings for the
/// next start; the list drops what is gone from disk only when asked, and nothing still there.
[<AvaloniaFact>]
let ``the recent list is kept across starts and drops only what is gone`` () =
    let settings = (Directory.CreateTempSubdirectory "tambour-settings").FullName
    let before = App.SettingsDirectory
    App.SettingsDirectory <- settings

    try
        let dir = (Directory.CreateTempSubdirectory "tambour-recent").FullName
        let a = Path.Combine(dir, "a.fsx")
        let b = Path.Combine(dir, "b.fsx")
        File.WriteAllText(a, "let a = 1\n")
        File.WriteAllText(b, "let b = 2\n")
        let window = new Shell.MainWindow("")
        window.Show()
        window.Post(Shell.OpenTab a)
        window.Post(Shell.OpenTab b)
        window.Post(Shell.OpenTab a)
        pumpUntil 10.0 "both opened" (fun () -> window.Workspaces.Length = 2)
        Assert.Equal<string list>([ a; b ], window.Recent)
        closeAll window

        // The next start has them, and a file deleted since is dropped only on Remove missing.
        File.Delete b
        let again = new Shell.MainWindow("")
        again.Show()
        Assert.Equal<string list>([ a; b ], again.Recent)
        again.Post(Shell.ForgetRecent true)
        Assert.Equal<string list>([ a ], again.Recent)
        Assert.Equal<string list>([ a ], (Preferences.read settings).Recent)
        closeAll again
    finally
        App.SettingsDirectory <- before

/// Gate 15 (d): the open tabs come back at the next start, in their order with the one shown; a
/// script gone since is dropped with a note; with no tab open the start view is shown, and its
/// New script opens one.
[<AvaloniaFact>]
let ``the open tabs come back at the next start, and with none the start view`` () =
    let settings = (Directory.CreateTempSubdirectory "tambour-settings").FullName
    let before = App.SettingsDirectory
    App.SettingsDirectory <- settings

    try
        let a = Path.Combine((Directory.CreateTempSubdirectory "tambour-a").FullName, "a.fsx")
        let b = Path.Combine((Directory.CreateTempSubdirectory "tambour-b").FullName, "b.fsx")
        File.WriteAllText(a, "let a = 1\n")
        File.WriteAllText(b, "let b = 2\n")
        let window = new Shell.MainWindow("", Some a)
        window.Show()
        window.Open b
        pumpUntil 10.0 "both opened" (fun () -> window.Workspaces |> List.map (fun w -> w.Path) = [ Some a; Some b ])
        window.ShowTab 0
        Dispatcher.UIThread.RunJobs()
        closeAll window

        let again = Shell.MainWindow.Start None
        again.Show()
        pumpUntil 10.0 "the tabs again" (fun () -> again.Workspaces |> List.map (fun w -> w.Path) = [ Some a; Some b ])
        Assert.Equal(Some a, again.Active.Path)
        pumpUntil 10.0 "a's script" (fun () -> (editorOf again).Text = "let a = 1\n")
        closeAll again

        // b gone: a alone, and a note naming b.
        File.Delete b
        let third = Shell.MainWindow.Start None
        third.Show()
        pumpUntil 10.0 "a again" (fun () -> third.Workspaces |> List.map (fun w -> w.Path) = [ Some a ])
        let texts () = third.Active.GetVisualDescendants().OfType<TextBlock>() |> Seq.map (fun t -> t.Text) |> Seq.filter (fun t -> not (isNull t)) |> List.ofSeq
        pumpUntil 5.0 "the note" (fun () -> texts () |> List.exists (fun t -> t.Contains "no longer there: b.fsx"))

        // The last tab closed: the start view, kept so for the next start.
        press third Key.W RawInputModifiers.Control
        pumpUntil 5.0 "the start view" (fun () -> third.Starting)
        Assert.Empty((Preferences.read settings).Tabs)
        let shown () = third.GetVisualDescendants().OfType<TextBlock>() |> Seq.filter (fun t -> t.IsEffectivelyVisible) |> Seq.map (fun t -> t.Text) |> List.ofSeq
        pumpUntil 5.0 "its entries" (fun () -> [ "New script"; "Open…"; "Open folder…"; "a.fsx" ] |> List.forall (fun e -> List.contains e (shown ())))

        match Environment.GetEnvironmentVariable "TAMBOUR_CHECK_OUT" with
        | null
        | "" -> ()
        | out ->
            use frame = third.CaptureRenderedFrame()
            frame.Save(Path.Combine(out, "start.png"), Avalonia.Media.Imaging.PngBitmapEncoderOptions.Default)

        press third Key.N RawInputModifiers.Control
        pumpUntil 5.0 "a new tab" (fun () -> third.Workspaces.Length = 1 && not third.Starting)
        closeAll third

        let fourth = Shell.MainWindow.Start None
        Assert.True(fourth.Starting)
        fourth.Close()
    finally
        App.SettingsDirectory <- before

/// A tab moved to a window of its own keeps its script, its history and its run; moved back, the
/// window it went to closes with its last tab; and the language service they share outlives the
/// window the tab came from.
[<AvaloniaFact>]
let ``a tab moved to a window of its own keeps its work, and comes back`` () =
    let window = new Shell.MainWindow("let a = 1\n")
    window.Show()
    pumpUntil 60.0 "the workers' start" (fun () -> window.Started.IsCompleted)
    typeInto window "// typed\n"
    let moved = window.Active
    let text = (editorOf window).Text
    press window Key.N RawInputModifiers.Control
    pumpUntil 5.0 "a second tab" (fun () -> window.Workspaces.Length = 2)
    window.ShowTab 0

    let other = window.MoveToNewWindow moved
    pumpUntil 5.0 "the move" (fun () -> other.Workspaces = [ moved ] && window.Workspaces.Length = 1)
    Assert.Equal(text, (editorOf other).Text)
    other.Post Shell.Undo
    pumpUntil 5.0 "its history, there" (fun () -> (editorOf other).Text = "let a = 1\n")
    other.Post Shell.Redo
    pumpUntil 5.0 "and again" (fun () -> (editorOf other).Text = text)
    let ran (w: Shell.MainWindow) () = w.Active.GetVisualDescendants().OfType<TextBlock>() |> Seq.exists (fun b -> not (isNull b.Text) && b.Text.StartsWith "ran in")
    other.Post Shell.Run
    pumpUntil 30.0 "a run in its new window" (ran other)

    // Back: the window it went to closes, as it has no tab left.
    let closed = ref false
    other.Closed.Add(fun _ -> closed.Value <- true)
    other.MoveTo(moved, window)
    pumpUntil 5.0 "the way back" (fun () -> closed.Value && window.Workspaces.Length = 2 && obj.ReferenceEquals(window.Active, moved))

    // Out again, and the first window closed: the language service still checks the moved script.
    let third = window.MoveToNewWindow moved
    pumpUntil 5.0 "the second move" (fun () -> third.Workspaces = [ moved ])
    closeAll window
    typeInto third "let wrong: int = \"text\"\n"
    let problems () = third.Active.GetVisualDescendants().OfType<TextBlock>() |> Seq.exists (fun b -> not (isNull b.Text) && b.Text.StartsWith "1 error")
    pumpUntil 30.0 "the error, found after the first window closed" problems
    closeAll third

/// A script in a project's directory, opened on its own, opens in its project, with its tables.
[<AvaloniaFact>]
let ``a script opened on its own in a workspace has its tables`` () =
    let dir = (Directory.CreateTempSubdirectory "tambour-inside").FullName
    Project.makeWorkspace dir
    Tambour.Data.Csv.writeFile (Path.Combine(dir, "tables", "runs.csv")) (trial "runs" "auc")
    let script = Path.Combine(dir, "second.fsx")
    File.WriteAllText(script, "let n = runs.Length\n")
    let window = new Shell.MainWindow("", Some script)
    window.Show()
    pumpUntil 10.0 "the script" (fun () -> (editorOf window).Text.Contains "runs.Length")
    Assert.Equal(Some dir, window.Active.Project)
    let ran () = window.Active.GetVisualDescendants().OfType<TextBlock>() |> Seq.exists (fun b -> not (isNull b.Text) && b.Text.StartsWith "ran in")
    pumpUntil 60.0 "a clean run against its tables" ran
    closeAll window

/// Save all saves every tab with unsaved work; Close project closes the tabs of that project and
/// leaves the others.
[<AvaloniaFact>]
let ``save all saves every tab, and close project closes only its tabs`` () =
    let project = (Directory.CreateTempSubdirectory "tambour-project").FullName
    File.WriteAllText(Path.Combine(project, "one.fsx"), "let one = 1\n")
    let other = Path.Combine((Directory.CreateTempSubdirectory "tambour-other").FullName, "other.fsx")
    File.WriteAllText(other, "let other = 1\n")
    let window = new Shell.MainWindow("", Some project)
    window.Show()
    pumpUntil 10.0 "the project" (fun () -> (editorOf window).Text = "let one = 1\n")
    typeInto window "// one\n"
    window.Post(Shell.OpenTab other)
    pumpUntil 10.0 "the other" (fun () -> window.Workspaces.Length = 2 && (editorOf window).Text = "let other = 1\n")
    typeInto window "// other\n"

    press window Key.S (RawInputModifiers.Control ||| RawInputModifiers.Alt)
    pumpUntil 10.0 "both saved" (fun () -> window.Workspaces |> List.forall (fun w -> w.Unsaved.IsEmpty))
    Assert.Equal("let one = 1\n// one\n", File.ReadAllText(Path.Combine(project, "one.fsx")))
    Assert.Equal("let other = 1\n// other\n", File.ReadAllText other)

    press window Key.Tab RawInputModifiers.Control
    Assert.Equal(Some project, window.Active.Project)
    window.Post Shell.CloseProject
    pumpUntil 5.0 "the project's tab closed" (fun () -> window.Workspaces.Length = 1)
    Assert.Equal(Some other, window.Active.Path)
    closeAll window

/// Save as puts the workspace in a directory of its own, named as the script, with its tables.
[<AvaloniaFact>]
let ``save as makes a new workspace of the tab, its tables with it`` () =
    let dir = (Directory.CreateTempSubdirectory "tambour-saveas").FullName
    Project.makeWorkspace dir
    Tambour.Data.Csv.writeFile (Path.Combine(dir, "tables", "runs.csv")) (trial "runs" "auc")
    File.WriteAllText(Path.Combine(dir, "figure.fsx"), "let x = 1\n")
    let parent = (Directory.CreateTempSubdirectory "tambour-saveas-to").FullName
    let window = new Shell.MainWindow("", Some dir)
    window.Show()
    pumpUntil 10.0 "the workspace" (fun () -> (editorOf window).Text = "let x = 1\n")
    window.Post(Shell.SaveAs(Path.Combine(parent, "copy.fsx")))
    let copy = Path.Combine(parent, "copy")
    pumpUntil 10.0 "the copy" (fun () -> File.Exists(Path.Combine(copy, "copy.fsx")))
    Assert.Equal("let x = 1\n", File.ReadAllText(Path.Combine(copy, "copy.fsx")))
    Assert.True(File.Exists(Path.Combine(copy, "tables", "runs.csv")))
    Assert.True(File.Exists(Path.Combine(dir, "tables", "runs.csv")))
    Assert.Equal(Some copy, window.Active.Project)
    closeAll window

/// The project's scripts in the file list, and what it does to them: duplicate, rename (the
/// project's settings and the tab showing it follow), and delete, which asks first, naming each
/// file; Cancel removes none, and Delete exactly those named, closing a tab that showed one.
[<AvaloniaFact>]
let ``the file list duplicates, renames and deletes a project's scripts`` () =
    let dir = (Directory.CreateTempSubdirectory "tambour-files").FullName
    let one = Path.Combine(dir, "one.fsx")
    File.WriteAllText(one, "let one = 1\n")
    File.WriteAllText(Path.Combine(dir, "two.fsx"), "let two = 2\n")
    Project.write dir (Project.defaults "one.fsx")
    let window = new Shell.MainWindow("", Some dir)
    window.Show()
    pumpUntil 10.0 "the project" (fun () -> (editorOf window).Text = "let one = 1\n")
    let listed () = window.Active.GetVisualDescendants().OfType<TextBlock>() |> Seq.map (fun t -> t.Text) |> Seq.filter (fun t -> not (isNull t)) |> List.ofSeq
    pumpUntil 5.0 "the list" (fun () -> listed () |> List.exists (fun t -> t.StartsWith "two.fsx"))

    // Duplicate: a copy beside it, listed.
    window.Post(Shell.DuplicateFile(Path.Combine(dir, "two.fsx")))
    pumpUntil 5.0 "the copy listed" (fun () -> listed () |> List.exists (fun t -> t.StartsWith "two copy.fsx"))
    Assert.Equal("let two = 2\n", File.ReadAllText(Path.Combine(dir, "two copy.fsx")))

    match Environment.GetEnvironmentVariable "TAMBOUR_CHECK_OUT" with
    | null
    | "" -> ()
    | out ->
        use frame = window.CaptureRenderedFrame()
        frame.Save(Path.Combine(out, "files.png"), Avalonia.Media.Imaging.PngBitmapEncoderOptions.Default)

    // Rename the shown script: moved, named so in the settings, and the tab follows it.
    window.AskName <- fun _ current -> Assert.Equal("one", current); Task.FromResult(Some "first")
    window.Post(Shell.RenameFile one)
    let first = Path.Combine(dir, "first.fsx")
    pumpUntil 5.0 "the tab following" (fun () -> window.Active.Path = Some first)
    Assert.False(File.Exists one)
    Assert.Equal("let one = 1\n", File.ReadAllText first)
    Assert.Equal("first.fsx", (Project.read dir).Value.Script)

    // Delete, cancelled: nothing goes.
    let asked = ref []
    window.ConfirmDelete <- fun files -> asked.Value <- files; Task.FromResult false
    window.Post(Shell.DeleteFile first)
    pumpUntil 5.0 "the question" (fun () -> not asked.Value.IsEmpty)
    Assert.Equal<string list>([ first ], asked.Value)
    Assert.True(File.Exists first)

    // Delete: exactly the file named goes, and its tab with it.
    window.ConfirmDelete <- fun _ -> Task.FromResult true
    window.Post(Shell.DeleteFile first)
    pumpUntil 5.0 "the tab closed" (fun () -> window.Starting)
    Assert.False(File.Exists first)
    Assert.Equal<string list>([ "two copy.fsx"; "two.fsx"; Project.FileName ], Directory.GetFiles dir |> Array.map Path.GetFileName |> Array.sort |> List.ofArray |> List.sortBy (fun f -> f.EndsWith ".json", f))
    closeAll window

/// Every file under a directory, by its path there, with its bytes.
let private filesUnder (dir: string) =
    Directory.GetFiles(dir, "*", SearchOption.AllDirectories)
    |> Array.map (fun f -> Path.GetRelativePath(dir, f), File.ReadAllBytes f)
    |> Array.sortBy fst
    |> List.ofArray

/// A table's uses in a script are its prelude binding and its rows' type, in code only.
[<Fact>]
let ``a table's uses are renamed in code, not in strings, comments or other records' fields`` () =
    let script = "let n = runs.Length\nlet r: Run = Prelude.runs.[0]\n// runs\nlet s = \"runs\"\nlet a = x.runs + reruns\n"
    let renamed, uses = Tables.renameUses script "runs" "trials"
    Assert.Equal(3, uses)
    Assert.Equal("let n = trials.Length\nlet r: Trial = Prelude.trials.[0]\n// runs\nlet s = \"runs\"\nlet a = x.runs + reruns\n", renamed)
    Assert.Equal(("let t = ``two words``\n", 1), Tables.renameUses "let t = runs\n" "runs" "two words")

/// Gate 15 (c), tables: a table renamed has its file moved on the next save, the prelude written
/// again and the script's uses renamed, as one entry; one undo takes it all back, and the files
/// saved then are byte for byte those before. A duplicate is a table of its own.
[<AvaloniaFact>]
let ``a table's rename follows into the prelude and the script, and one undo takes it back`` () =
    let dir = (Directory.CreateTempSubdirectory "tambour-tablename").FullName
    Project.makeWorkspace dir
    Tambour.Data.Csv.writeFile (Path.Combine(dir, "tables", "runs.csv")) (trial "runs" "auc")
    let script = "let n = runs.Length\n// runs, in a comment\nlet s = \"runs\"\n"
    File.WriteAllText(Path.Combine(dir, "figure.fsx"), script)
    let window = new Shell.MainWindow("", Some dir)
    window.Show()
    pumpUntil 60.0 "the workers' start" (fun () -> window.Started.IsCompleted)
    let editor = editorOf window
    pumpUntil 10.0 "the script" (fun () -> editor.Text = script)
    window.Post Shell.Save
    pumpUntil 10.0 "the first save" (fun () -> File.Exists(Path.Combine(dir, Project.WorkspaceFile)))
    let before = filesUnder dir

    let asked = ref ""
    window.AskName <- fun _ current -> Assert.Equal("runs", current); Task.FromResult(Some "trials")
    window.Confirm <- fun question _ _ -> asked.Value <- question; Task.FromResult true
    window.Post(Shell.RenameTable "runs")
    pumpUntil 10.0 "the uses renamed" (fun () -> editor.Text.Contains "trials.Length")
    Assert.Contains("1 place", asked.Value)
    Assert.Contains("// runs, in a comment", editor.Text)
    Assert.Contains("\"runs\"", editor.Text)
    let prelude () = File.ReadAllText(Path.Combine(dir, "prelude.fsx"))
    Assert.Contains("\"tables/trials.csv\"", prelude ())
    Assert.Contains("let mutable trials: Trial[]", prelude ())
    let status () = window.Active.GetVisualDescendants().OfType<TextBlock>() |> Seq.exists (fun b -> not (isNull b.Text) && b.Text.StartsWith "ran in")
    pumpUntil 30.0 "a clean run on the renamed table" status

    press window Key.S RawInputModifiers.Control
    pumpUntil 10.0 "the save" (fun () -> window.Active.Unsaved.IsEmpty)
    Assert.True(File.Exists(Path.Combine(dir, "tables", "trials.csv")))
    Assert.False(File.Exists(Path.Combine(dir, "tables", "runs.csv")))

    // One undo: the table's name and the script's use, both.
    window.Post Shell.Undo
    pumpUntil 10.0 "the undo" (fun () -> editor.Text = script)
    Assert.Contains("\"tables/runs.csv\"", prelude ())
    press window Key.S RawInputModifiers.Control
    pumpUntil 10.0 "the save after the undo" (fun () -> window.Active.Unsaved.IsEmpty)
    let after = filesUnder dir
    Assert.Equal<string list>(List.map fst before, List.map fst after)

    for (name, bytes), (_, again) in List.zip before after do
        Assert.True((bytes = again), $"{name} is not as it was")

    // A duplicate: a second table, shown in its own tab, its own file on saving.
    window.Post(Shell.DuplicateTable "runs")
    pumpUntil 10.0 "the duplicate" (fun () -> not window.Active.Unsaved.IsEmpty)
    press window Key.S RawInputModifiers.Control
    pumpUntil 10.0 "the duplicate saved" (fun () -> File.Exists(Path.Combine(dir, "tables", "runs_copy.csv")))
    Assert.Equal<byte[]>(File.ReadAllBytes(Path.Combine(dir, "tables", "runs.csv")), File.ReadAllBytes(Path.Combine(dir, "tables", "runs_copy.csv")))
    closeAll window

/// Gate 15 (c), undo: a script's rename and duplicate are on the history, and taken back to the
/// files before, byte for byte, the tab and the settings following. A copy changed since is not
/// deleted by an undo.
[<AvaloniaFact>]
let ``a script's rename and duplicate are undone to the files before`` () =
    let dir = (Directory.CreateTempSubdirectory "tambour-fileundo").FullName
    let one = Path.Combine(dir, "one.fsx")
    let copy = Path.Combine(dir, "two copy.fsx")
    File.WriteAllText(one, "let one = 1\n")
    File.WriteAllText(Path.Combine(dir, "two.fsx"), "let two = 2\n")
    Project.write dir (Project.defaults "one.fsx")
    let window = new Shell.MainWindow("", Some dir)
    window.Show()
    pumpUntil 10.0 "the project" (fun () -> (editorOf window).Text = "let one = 1\n")
    let before = filesUnder dir

    window.AskName <- fun _ _ -> Task.FromResult(Some "first")
    window.Post(Shell.RenameFile one)
    let first = Path.Combine(dir, "first.fsx")
    pumpUntil 5.0 "the rename" (fun () -> window.Active.Path = Some first)
    window.Post(Shell.DuplicateFile(Path.Combine(dir, "two.fsx")))
    pumpUntil 5.0 "the copy" (fun () -> File.Exists copy)

    window.Post Shell.Undo
    pumpUntil 5.0 "the copy taken back" (fun () -> not (File.Exists copy))
    window.Post Shell.Undo
    pumpUntil 5.0 "the rename taken back" (fun () -> window.Active.Path = Some one)
    let after = filesUnder dir
    Assert.Equal<string list>(List.map fst before, List.map fst after)

    for (name, bytes), (_, again) in List.zip before after do
        Assert.True((bytes = again), $"{name} is not as it was")

    // Done again, then the copy changed on disk: its undo is refused, and nothing goes.
    window.Post Shell.Redo
    pumpUntil 5.0 "the rename redone" (fun () -> window.Active.Path = Some first)
    window.Post Shell.Redo
    pumpUntil 5.0 "the copy redone" (fun () -> File.Exists copy)
    Assert.Equal("first.fsx", (Project.read dir).Value.Script)
    File.AppendAllText(copy, "// changed\n")
    window.Post Shell.Undo
    pumpUntil 5.0 "the refusal" (fun () -> window.Active.GetVisualDescendants().OfType<TextBlock>() |> Seq.exists (fun b -> not (isNull b.Text) && b.Text.Contains "not undone"))
    Assert.True(File.Exists copy)
    Assert.True(File.Exists first)
    closeAll window

/// A workspace's only script, deleted, takes the workspace with it, and the question names each
/// of its files.
[<AvaloniaFact>]
let ``deleting a workspace's only script names its tables, settings and prelude`` () =
    let dir = (Directory.CreateTempSubdirectory "tambour-only").FullName
    Project.makeWorkspace dir
    Tambour.Data.Csv.writeFile (Path.Combine(dir, "tables", "runs.csv")) (trial "runs" "auc")
    let script = Path.Combine(dir, "figure.fsx")
    File.WriteAllText(script, "let x = 1\n")
    Project.write dir (Project.defaults "figure.fsx")
    let window = new Shell.MainWindow("", Some dir)
    window.Show()
    pumpUntil 10.0 "the workspace" (fun () -> (editorOf window).Text = "let x = 1\n")
    let asked = ref []
    window.ConfirmDelete <- fun files -> asked.Value <- files; Task.FromResult false
    window.Post(Shell.DeleteFile script)
    pumpUntil 5.0 "the question" (fun () -> not asked.Value.IsEmpty)

    Assert.Equal<string list>(
        [ script
          Path.Combine(dir, "tables", "runs.csv")
          Path.Combine(dir, Project.WorkspaceFile)
          Path.Combine(dir, "prelude.fsx") ],
        asked.Value
    )

    Assert.True(File.Exists(Path.Combine(dir, "tables", "runs.csv")))
    closeAll window

/// Gate 16 (b), kept: a table's column layout is written to its workspace and back at the next
/// start; and the full view gives the right side the whole window until Escape.
[<AvaloniaFact>]
let ``a table's columns as laid out come back at the next start, and the full view leaves on Escape`` () =
    let dir = (Directory.CreateTempSubdirectory "tambour-layout").FullName
    Project.makeWorkspace dir
    Tambour.Data.Csv.writeFile (Path.Combine(dir, "tables", "runs.csv")) (trial "runs" "auc")
    File.WriteAllText(Path.Combine(dir, "figure.fsx"), "let n = runs.Length\n")
    let laid: GridLayout = { Widths = Map.ofList [ "auc", 150.0 ]; Hidden = Set.empty; Collapsed = set [ "auc" ] }

    let window = new Shell.MainWindow("", Some dir)
    window.Show()
    pumpUntil 60.0 "the workers' start" (fun () -> window.Started.IsCompleted)
    pumpUntil 10.0 "the script" (fun () -> (editorOf window).Text.Contains "runs.Length")
    window.Post(Shell.SelectTab 1)
    window.Post(Shell.LaidOut("runs", laid))
    pumpUntil 5.0 "the layout written" (fun () -> File.ReadAllText(Path.Combine(dir, Project.WorkspaceFile)).Contains "Collapsed")

    // The full view: the editor gets no width, and Escape gives it back.
    let editor = editorOf window
    window.Post Shell.ToggleFullView
    pumpUntil 5.0 "the full view" (fun () -> window.CaptureRenderedFrame().Dispose(); editor.Bounds.Width = 0.0)
    // The editor itself stays: hidden, not made anew.
    Assert.Same(editor, editorOf window)
    // Escape from the table's grid, where the focus is in the full view.
    (window.Active.GetVisualDescendants().OfType<TableGrid>() |> Seq.head).Focus() |> ignore
    // Escape cancels a run first; with none running it leaves the full view.
    pumpUntil 30.0 "the first run's end" (fun () ->
        window.Active.GetVisualDescendants().OfType<TextBlock>() |> Seq.exists (fun b -> not (isNull b.Text) && b.Text.StartsWith "ran in"))
    press window Key.Escape RawInputModifiers.None
    pumpUntil 5.0 "the full view left" (fun () -> window.CaptureRenderedFrame().Dispose(); editor.Bounds.Width > 0.0)
    closeAll window

    let again = new Shell.MainWindow("", Some dir)
    again.Show()
    pumpUntil 60.0 "the workers' start again" (fun () -> again.Started.IsCompleted)
    again.Post(Shell.SelectTab 1)
    pumpUntil 10.0 "the grid with its layout" (fun () ->
        again.Active.GetVisualDescendants().OfType<TableGrid>() |> Seq.exists (fun g -> g.Layout = laid))
    closeAll again

/// A table renamed keeps its column layout under the new name, in the grid and the workspace,
/// and an undo takes it back to the old name with the table.
[<AvaloniaFact>]
let ``a table's rename carries its column layout, and an undo brings it back`` () =
    let dir = (Directory.CreateTempSubdirectory "tambour-layout-rename").FullName
    Project.makeWorkspace dir
    Tambour.Data.Csv.writeFile (Path.Combine(dir, "tables", "runs.csv")) (trial "runs" "auc")
    File.WriteAllText(Path.Combine(dir, "figure.fsx"), "let n = 1\n")
    let laid: GridLayout = { Widths = Map.ofList [ "auc", 150.0 ]; Hidden = Set.empty; Collapsed = set [ "auc" ] }
    let workspace () = File.ReadAllText(Path.Combine(dir, Project.WorkspaceFile))

    let window = new Shell.MainWindow("", Some dir)
    window.Show()
    pumpUntil 60.0 "the workers' start" (fun () -> window.Started.IsCompleted)
    pumpUntil 10.0 "the script" (fun () -> (editorOf window).Text.Contains "let n")
    window.Post(Shell.SelectTab 1)
    window.Post(Shell.LaidOut("runs", laid))
    pumpUntil 5.0 "the layout written" (fun () -> workspace().Contains "Collapsed")

    let grid () = window.Active.GetVisualDescendants().OfType<TableGrid>() |> Seq.tryHead
    window.Post(Shell.RenameTableNow("runs", "trials", false))
    pumpUntil 5.0 "the layout under the new name" (fun () -> workspace().Contains "\"trials\"" && not (workspace().Contains "\"runs\""))
    pumpUntil 5.0 "the grid with its layout" (fun () -> grid () |> Option.exists (fun g -> g.Layout = laid))

    window.Post Shell.Undo
    pumpUntil 5.0 "the layout back under the old name" (fun () -> workspace().Contains "\"runs\"" && not (workspace().Contains "\"trials\""))
    pumpUntil 5.0 "the grid with its layout again" (fun () -> grid () |> Option.exists (fun g -> g.Layout = laid))
    closeAll window

/// The window's place and size, and which panes are open, are kept in the settings and back at
/// the next start; a pane closed in one tab is closed in the next one opened.
[<AvaloniaFact>]
let ``the window's place, size and open panes come back at the next start`` () =
    let before = App.SettingsDirectory
    let settings = (Directory.CreateTempSubdirectory "tambour-place").FullName
    App.SettingsDirectory <- settings

    try
        let window = Shell.MainWindow.Start None
        window.Show()
        window.Width <- 900.0
        window.Height <- 600.0
        window.Position <- Avalonia.PixelPoint(120, 90)
        Dispatcher.UIThread.RunJobs()

        // The panel under the script closed in a tab: closed in the next tab too.
        press window Key.N RawInputModifiers.Control
        pumpUntil 5.0 "a tab" (fun () -> window.Workspaces.Length = 1)
        window.Post Shell.ToggleBottom
        Dispatcher.UIThread.RunJobs()
        Assert.False((Preferences.read settings).BottomOpen)
        press window Key.N RawInputModifiers.Control
        pumpUntil 5.0 "a second tab" (fun () -> window.Workspaces.Length = 2)
        let bottom () = window.Active.GetVisualDescendants().OfType<TabControl>() |> Seq.tryFind (fun t -> t.Items.OfType<TabItem>().Any(fun i -> string i.Header = "Output"))
        pumpUntil 5.0 "the second tab's panes" (fun () -> (bottom ()).IsSome)
        Assert.False((bottom ()).Value.IsVisible, "the panel closed in one tab is open in the next")
        closeAll window

        Assert.Equal(Some({ X = 120; Y = 90; Width = 900.0; Height = 600.0; Maximized = false }: Preferences.Placement), (Preferences.read settings).Window)

        let again = Shell.MainWindow.Start None
        again.Show()
        Dispatcher.UIThread.RunJobs()
        Assert.Equal((900.0, 600.0), (again.Width, again.Height))
        Assert.Equal(Avalonia.PixelPoint(120, 90), again.Position)
        again.Close()
    finally
        App.SettingsDirectory <- before

/// The dividers between the panes are rules of one pixel, and still taken by a drag a little to
/// either side of the rule.
[<AvaloniaFact>]
let ``a divider is a one-pixel rule that a drag beside it still moves`` () =
    let window = new Shell.MainWindow("let x = 1\n")
    window.Show()
    window.Width <- 1200.0
    window.Height <- 800.0
    editorOf window |> ignore
    Dispatcher.UIThread.RunJobs()

    let splitter =
        window.Active.GetVisualDescendants().OfType<GridSplitter>()
        |> Seq.find (fun s -> Grid.GetColumn s = 1 && s.IsVisible)

    let grid = splitter.Parent :?> Grid
    Assert.Equal(1.0, grid.ColumnDefinitions.[1].ActualWidth)
    let before = grid.ColumnDefinitions.[0].ActualWidth

    // Three pixels left of the rule: inside the grab, outside the line.
    let at = (Avalonia.VisualExtensions.TranslatePoint(splitter, Avalonia.Point(1.0, splitter.Bounds.Height / 2.0), window)).Value
    Assert.True(splitter.Bounds.Width > 4.0, $"the grab is {splitter.Bounds.Width} px wide")
    window.MouseMove at
    window.MouseDown(at, MouseButton.Left, RawInputModifiers.None)
    window.MouseMove(at + Avalonia.Point(100.0, 0.0), RawInputModifiers.LeftMouseButton)
    window.MouseUp(at + Avalonia.Point(100.0, 0.0), MouseButton.Left, RawInputModifiers.None)
    Dispatcher.UIThread.RunJobs()
    Assert.InRange(grid.ColumnDefinitions.[0].ActualWidth - before, 90.0, 110.0)
    Assert.Equal(1.0, grid.ColumnDefinitions.[1].ActualWidth)

    window.Ask <- fun _ -> Task.FromResult Discard
    window.Close()

/// The start view has the menus that need no tab: File with the recent list, Edit with the
/// preferences, and Help; its items and keys do what the tabs' do.
[<AvaloniaFact>]
let ``the start view has a menu bar of what needs no tab`` () =
    let window = Shell.MainWindow.Start None
    window.Show()
    Dispatcher.UIThread.RunJobs()
    Assert.True(window.Starting)

    let menu = window.GetVisualDescendants().OfType<Menu>() |> Seq.exactlyOne
    let rec leaves (items: obj seq) = [ for i in items do match i with :? MenuItem as m -> yield m; yield! leaves (m.Items |> Seq.cast) | _ -> () ]
    let headers = menu.Items |> Seq.cast<MenuItem> |> Seq.map (fun m -> string m.Header) |> List.ofSeq
    Assert.Equal<string list>([ "_File"; "_Edit"; "_Help" ], headers)
    let named = leaves (menu.Items |> Seq.cast) |> List.map (fun m -> string m.Header)
    for h in [ "_New script"; "_Open…"; "Open _folder…"; "Open _recent"; "_Quit"; "_Preferences…"; "_About Tambour" ] do
        Assert.Contains(h, named)
    let preferences = leaves (menu.Items |> Seq.cast) |> List.find (fun m -> string m.Header = "_Preferences…")
    Assert.Equal("Ctrl+OemComma", string preferences.InputGesture)

    // Ctrl+, opens the preferences over the start view.
    press window Key.OemComma RawInputModifiers.Control
    pumpUntil 5.0 "the preferences" (fun () -> window.OwnedWindows.Count = 1)
    window.OwnedWindows.[0].Close()
    Dispatcher.UIThread.RunJobs()

    // New script from the menu: a tab, and the start view gone.
    let fresh = leaves (menu.Items |> Seq.cast) |> List.find (fun m -> string m.Header = "_New script")
    fresh.RaiseEvent(Avalonia.Interactivity.RoutedEventArgs MenuItem.ClickEvent)
    pumpUntil 5.0 "a tab" (fun () -> window.Workspaces.Length = 1)
    Assert.False(window.Starting)
    closeAll window
