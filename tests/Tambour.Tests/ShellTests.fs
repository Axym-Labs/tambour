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
    window.Close()
