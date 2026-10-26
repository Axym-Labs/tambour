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

    let button (label: string) =
        window.GetVisualDescendants().OfType<Button>().FirstOrDefault(fun b -> b.Content = box label)

    pumpUntil 30.0 "the actions" (fun () -> not (isNull (button "Title")) && (button "Title").IsEnabled)
    let colourBy = window.GetVisualDescendants().OfType<ComboBox>().Single(fun c -> c.PlaceholderText = "Colour by")
    Assert.False(colourBy.IsEnabled, "colour by is offered for a pipeline")
    Assert.Contains("plot { }", string (ToolTip.GetTip colourBy))

    let title = window.GetVisualDescendants().OfType<TextBox>().Single(fun t -> t.PlaceholderText = "title")
    title.Text <- "Decay"
    Dispatcher.UIThread.RunJobs()
    let before = pane.Picture
    (button "Title").RaiseEvent(Avalonia.Interactivity.RoutedEventArgs Button.ClickEvent)
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
    window.Close()
