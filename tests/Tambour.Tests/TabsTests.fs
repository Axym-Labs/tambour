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

/// Answers the question before unsaved work is dropped, counting each time it is asked.
let private answering (window: Shell.MainWindow) (answer: Shell.Unsaved) =
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

    // A tab's history is its own: Undo here leaves the other tab's text alone.
    window.Post Shell.Undo
    Dispatcher.UIThread.RunJobs()
    Assert.Equal("", (editorOf window).Text)
    Assert.Equal("let first = 1\n", window.Workspaces.[0].GetVisualDescendants().OfType<ScriptEditor>().Single().Text)

    answering window Shell.Discard |> ignore
    window.Close()

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
    let asked = answering window Shell.Keep
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
    let asked = answering window Shell.SaveIt
    press window Key.W RawInputModifiers.Control
    pumpUntil 5.0 "the saved tab closed" (fun () -> window.Workspaces.Single().Path.IsNone)
    Assert.Equal(1, asked.Value)
    Assert.Equal(text, File.ReadAllText file)

    // Don't save: the tab goes, and the file keeps what was saved.
    window.Post(Shell.OpenTab file)
    pumpUntil 10.0 "the script again" (fun () -> (editorOf window).Text = text)
    typeInto window "// dropped\n"
    let asked = answering window Shell.Discard
    press window Key.W RawInputModifiers.Control
    pumpUntil 5.0 "the tab dropped" (fun () -> window.Workspaces.Single().Path.IsNone)
    Assert.Equal(1, asked.Value)
    Assert.Equal(text, File.ReadAllText file)
    window.Close()

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

    let asked = answering window Shell.Keep
    press window Key.Q RawInputModifiers.Control
    pumpUntil 5.0 "the question" (fun () -> asked.Value = 1)
    Dispatcher.UIThread.RunJobs()
    Assert.False closed.Value
    Assert.Equal(2, window.Workspaces.Length)
    Assert.Equal("let a = 1\n", window.Workspaces.[0].GetVisualDescendants().OfType<ScriptEditor>().Single().Text)

    // Don't save, asked once for each unsaved tab.
    let asked = answering window Shell.Discard
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
    window.Close()
