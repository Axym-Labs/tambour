/// The window as a user meets it, with real workers behind it.
[<Xunit.Collection("workers")>]
module Tambour.Tests.ShellTests

open System
open System.Diagnostics
open System.IO
open System.Linq
open System.Threading
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Headless.XUnit
open Avalonia.Input
open Avalonia.Input.Platform
open Avalonia.Threading
open Avalonia.VisualTree
open Tambour
open Xunit

/// Runs the UI thread's work until `ready` holds, or fails after `seconds`.
let private pumpUntil (seconds: float) (what: string) (ready: unit -> bool) =
    let clock = Stopwatch.StartNew()

    while not (ready ()) do
        if clock.Elapsed.TotalSeconds > seconds then
            failwith $"{what} did not happen within {seconds} s"

        Dispatcher.UIThread.RunJobs()
        Thread.Sleep 10

let private save (window: Window) (name: string) =
    match Environment.GetEnvironmentVariable "TAMBOUR_CHECK_OUT" with
    | null
    | "" -> ()
    | dir ->
        use frame = window.CaptureRenderedFrame()
        frame.Save(Path.Combine(dir, name), Avalonia.Media.Imaging.PngBitmapEncoderOptions.Default)

[<AvaloniaFact>]
let ``the window runs its script on F5 and shows the figure with its control`` () =
    let window = new Shell.MainWindow()
    window.Show()
    pumpUntil 60.0 "the workers' start" (fun () -> window.Started.IsCompleted)
    let pane = window.GetVisualDescendants().OfType<FigurePane>().Single()
    window.KeyPress(Key.F5, RawInputModifiers.None, PhysicalKey.F5, null)
    pumpUntil 30.0 "a figure" (fun () -> not (isNull pane.Picture))
    pumpUntil 10.0 "the control" (fun () -> window.GetVisualDescendants().OfType<Slider>().Any())
    window.CaptureRenderedFrame().Dispose()
    save window "shell.png"
    let slider = window.GetVisualDescendants().OfType<Slider>().Single()
    Assert.Equal(50.0, slider.Value)

    // Moving the control redraws the figure.
    let before = pane.Picture
    slider.Value <- 80.0
    pumpUntil 30.0 "the figure redrawn" (fun () -> not (obj.ReferenceEquals(pane.Picture, before)))
    Assert.Equal(Some Vector, pane.PaintPath)

    // A column plotted under the wrong unit is underlined and listed as the language service
    // checks the text as typed.
    let editor = window.GetVisualDescendants().OfType<ScriptEditor>().Single()
    let doc = editor.Editor.Document
    let at = editor.Text.IndexOf "U.metre t"
    doc.Replace(at, "U.metre".Length, "U.second")

    pumpUntil 30.0 "the editor's check" (fun () -> editor.Problems.Length > 0)

    pumpUntil 30.0 "the problem listed" (fun () ->
        window.GetVisualDescendants().OfType<TextBlock>().Any(fun t -> not (isNull t.Text) && t.Text.Contains "FS0001"))

    Assert.Contains(editor.Problems, fun d -> d.Code = "FS0001" && d.Message.Contains "float<m>")
    window.CaptureRenderedFrame().Dispose()
    save window "shell-problem.png"

    // A problem is copied with Ctrl+C on it, and every problem from its menu.
    let row =
        window.GetVisualDescendants().OfType<Button>()
        |> Seq.find (fun b -> b.GetVisualDescendants().OfType<TextBlock>().Any(fun t -> not (isNull t.Text) && t.Text.Contains "FS0001"))

    let clipboard () =
        let text = window.Clipboard.TryGetTextAsync()
        pumpUntil 5.0 "the clipboard" (fun () -> text.IsCompleted)
        text.Result

    row.Focus() |> ignore
    window.KeyPress(Key.C, RawInputModifiers.Control, PhysicalKey.C, "c")
    Dispatcher.UIThread.RunJobs()
    let copied = clipboard ()
    Assert.StartsWith($"{editor.Problems.[0].StartLine}:", copied)
    Assert.Contains("FS0001", copied)

    row.ContextMenu.Open row
    Dispatcher.UIThread.RunJobs()
    let all = row.ContextMenu.Items.OfType<MenuItem>() |> Seq.find (fun i -> string i.Header = "Copy all problems")
    all.RaiseEvent(Avalonia.Interactivity.RoutedEventArgs MenuItem.ClickEvent)
    Dispatcher.UIThread.RunJobs()
    let everything = clipboard ()

    for d in editor.Problems do
        Assert.Contains($"{d.StartLine}:{d.StartColumn + 1} {d.Severity} {d.Code}: ", everything)
    row.ContextMenu.Close()
    window.Close()

[<AvaloniaFact>]
let ``an action writes the script, and the figure is run again`` () =
    let script =
        "let t = Array.init 50 (fun i -> float i * 0.1<s>)\nlet fig = Chart.lineIn U.second U.metre t (t |> Array.map (fun t -> 1.0<m> * exp (-t / 2.0<s>))) // decay\n"

    let window = new Shell.MainWindow(script)
    window.Show()
    pumpUntil 60.0 "the workers' start" (fun () -> window.Started.IsCompleted)
    let pane = window.GetVisualDescendants().OfType<FigurePane>().Single()
    window.KeyPress(Key.F5, RawInputModifiers.None, PhysicalKey.F5, null)
    pumpUntil 30.0 "a figure" (fun () -> not (isNull pane.Picture))

    let title () =
        window.GetVisualDescendants().OfType<TextBox>().FirstOrDefault(fun t -> t.PlaceholderText = "title")

    pumpUntil 30.0 "the actions" (fun () -> not (isNull (title ())) && (title ()).IsEnabled)
    let more = window.GetVisualDescendants().OfType<Button>().Single(fun b -> b.Name = "more actions")
    let colourBy = more.ContextMenu.Items.OfType<MenuItem>().Single(fun m -> string m.Header = "_Colour by")
    Assert.False(colourBy.IsEnabled, "colour by is offered for a pipeline")
    Assert.Contains("plot { }", string (ToolTip.GetTip colourBy))

    // A field applies on Enter.
    let title = title ()
    title.Text <- "Decay"
    Dispatcher.UIThread.RunJobs()
    let before = pane.Picture
    title.Focus() |> ignore
    window.KeyPress(Key.Enter, RawInputModifiers.None, PhysicalKey.Enter, null)
    Dispatcher.UIThread.RunJobs()
    let editor = window.GetVisualDescendants().OfType<ScriptEditor>().Single()
    (try
        pumpUntil 30.0 "the script written" (fun () -> editor.Text.Contains "Chart.withTitle \"Decay\"")
     with e ->
        let texts = window.GetVisualDescendants().OfType<TextBlock>() |> Seq.map (fun t -> t.Text) |> Seq.filter (fun t -> not (String.IsNullOrEmpty t)) |> Seq.truncate 12
        let shown = String.Join(" | ", texts)
        failwith $"{e.Message}; title box: {title.Text}; texts: {shown}")
    pumpUntil 30.0 "the figure run again" (fun () -> not (obj.ReferenceEquals(pane.Picture, before)))
    Assert.Contains("// decay", editor.Text)
    Assert.Contains("          |> Chart.withTitle \"Decay\"", editor.Text)
    Assert.True(window.GetVisualDescendants().OfType<TextBlock>().Any(fun t -> t.Text = "added Chart.withTitle"), "the action's note is not shown")
    window.CaptureRenderedFrame().Dispose()
    save window "shell-action.png"

    // Typed text and the action are both the app's history: Ctrl+Z takes back the word, then the
    // action, and runs the figure again; Ctrl+Y does the action again.
    editor.Editor.CaretOffset <- editor.Text.Length
    editor.Editor.TextArea.PerformTextInput "// typed"
    let typed = editor.Text
    let titled = typed.Replace("// typed", "")
    window.KeyPress(Key.Z, RawInputModifiers.Control, PhysicalKey.Z, null)
    pumpUntil 10.0 "the word taken back" (fun () -> editor.Text = titled)
    let shown = pane.Picture
    window.KeyPress(Key.Z, RawInputModifiers.Control, PhysicalKey.Z, null)
    pumpUntil 10.0 "the action taken back" (fun () -> editor.Text = script)
    pumpUntil 30.0 "the figure run again" (fun () -> not (obj.ReferenceEquals(pane.Picture, shown)))
    window.KeyPress(Key.Y, RawInputModifiers.Control, PhysicalKey.Y, null)
    pumpUntil 10.0 "the action done again" (fun () -> editor.Text = titled)
    window.Close()

/// Every leaf of a menu, and its parents' leaves.
let rec private leaves (items: Collections.IEnumerable) : MenuItem list =
    [ for item in items do
          match item with
          | :? MenuItem as m when m.ItemCount > 0 -> yield! leaves m.Items
          | :? MenuItem as m -> m
          | _ -> () ]

/// Gate 14 (b): every command is in its menu with its shortcut, and every key of it takes that
/// command, and no other: the menus and the keys are made from one table, which this walks from
/// the menus' side. A key and its menu entry then do the same, shown for the bottom panel.
[<AvaloniaFact>]
let ``every command is in a menu with its shortcut, and its keys do what the entry does`` () =
    let window = new Shell.MainWindow("let x = 1\n")
    window.Show()
    Dispatcher.UIThread.RunJobs()
    let menu = window.GetVisualDescendants().OfType<Menu>().Single()
    let shown = leaves menu.Items

    for c in Shell.commands do
        match shown |> List.tryFind (fun m -> string m.Header = c.Header) with
        | None -> failwith $"{c.Header} is in no menu"
        | Some item ->
            Assert.Equal(Option.toObj c.Gesture, item.InputGesture)

            for g in Option.toList c.Gesture @ c.Also do
                Assert.Equal(Some c.Header, Shell.commandFor g.Key g.KeyModifiers |> Option.map (fun c -> c.Header))

    let keys = Shell.commands |> List.collect (fun c -> Option.toList c.Gesture @ c.Also) |> List.map string
    Assert.Equal<string list>(List.distinct keys, keys)

    let bottom () =
        window.GetVisualDescendants().OfType<TabControl>().Single(fun t -> t.Items.OfType<TabItem>().Any(fun i -> string i.Header = "Output"))

    Assert.True((bottom ()).IsVisible)
    window.KeyPress(Key.J, RawInputModifiers.Control, PhysicalKey.J, "j")
    Dispatcher.UIThread.RunJobs()
    Assert.False((bottom ()).IsVisible, "Ctrl+J left the bottom panel open")
    let entry = shown |> List.find (fun m -> string m.Header = "_Bottom panel")
    entry.RaiseEvent(Avalonia.Interactivity.RoutedEventArgs MenuItem.ClickEvent)
    Dispatcher.UIThread.RunJobs()
    Assert.True((bottom ()).IsVisible, "its menu entry left the bottom panel closed")
    window.Close()
