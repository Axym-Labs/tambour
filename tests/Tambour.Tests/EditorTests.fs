/// The script editor over a real language service.
[<Xunit.Collection("workers")>]
module Tambour.Tests.EditorTests

open System
open System.Diagnostics
open System.IO
open System.Threading
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Headless.XUnit
open Avalonia.Threading
open Tambour
open Xunit

let private pumpUntil (seconds: float) (what: string) (ready: unit -> bool) =
    let clock = Stopwatch.StartNew()

    while not (ready ()) do
        if clock.Elapsed.TotalSeconds > seconds then
            failwith $"{what} did not happen within {seconds} s"

        Dispatcher.UIThread.RunJobs()
        Thread.Sleep 10

let private command: Command =
    { File = Path.Combine(AppContext.BaseDirectory, (if OperatingSystem.IsWindows() then "tambour.exe" else "tambour"))
      Arguments = [] }

[<AvaloniaFact>]
let ``a typed dot completes a row's columns, and a binding is renamed wherever it is used`` () =
    use language = new LanguageService(command, { References = [||]; Prelude = Shell.prelude })
    let editor = ScriptEditor(Language = language)
    let window = Window(Width = 800.0, Height = 500.0, Content = editor)
    window.Show()

    let script =
        "type Row = { Time: float<s>; Height: float<m> }\nlet rows = [| { Time = 0.0<s>; Height = 1.0<m> } |]\nlet first = rows.[0]\n"

    editor.Text <- script
    editor.Editor.CaretOffset <- script.Length
    // Typed, as the text area takes a key's text.
    editor.Editor.TextArea.PerformTextInput "first"
    editor.Editor.TextArea.PerformTextInput "."
    pumpUntil 30.0 "the completion list" (fun () -> editor.Completions.Length > 0)
    Assert.Contains("Time", editor.Completions)
    Assert.Contains("Height", editor.Completions)

    // The caret on `rows`, then a new name for it.
    editor.Editor.CaretOffset <- script.IndexOf "rows" + 1
    let renamed = editor.RenameAsync "samples"
    pumpUntil 30.0 "the rename" (fun () -> renamed.IsCompleted)
    Assert.True renamed.Result
    Assert.Contains("let samples = [|", editor.Text)
    Assert.Contains("let first = samples.[0]", editor.Text)
    Assert.DoesNotContain("rows", editor.Text)

    // A library's name is not the script's to rename.
    editor.Editor.CaretOffset <- editor.Text.IndexOf "float<s>" + 2
    let refused = editor.RenameAsync "double"
    pumpUntil 30.0 "the refusal" (fun () -> refused.IsCompleted)
    Assert.False refused.Result
    window.Close()

/// A name typed letter by letter opens the completion; Enter on a list that came by itself ends
/// the line, and Tab takes its choice.
[<AvaloniaFact>]
let ``completion comes as a name is typed, and Enter still ends the line`` () =
    use language = new LanguageService(command, { References = [||]; Prelude = Shell.prelude })
    let editor = ScriptEditor(Language = language)
    let window = Window(Width = 800.0, Height = 500.0, Content = editor)
    window.Show()
    let script = "let pretrained = [| 1.0 |]\nlet a = "
    editor.Text <- script
    editor.Editor.CaretOffset <- script.Length
    editor.Editor.TextArea.Focus() |> ignore

    let typeName () =
        for letter in [ "p"; "r"; "e" ] do
            window.KeyTextInput letter
            Dispatcher.UIThread.RunJobs()

        pumpUntil 30.0 "the completion list" (fun () -> editor.Completions.Length > 0)
        Assert.Contains("pretrained", editor.Completions)

    let press (key: Avalonia.Input.Key) =
        window.KeyPress(key, Avalonia.Input.RawInputModifiers.None, Avalonia.Input.PhysicalKey.None, null)
        window.KeyRelease(key, Avalonia.Input.RawInputModifiers.None, Avalonia.Input.PhysicalKey.None, null)
        Dispatcher.UIThread.RunJobs()

    typeName ()
    press Avalonia.Input.Key.Enter
    Assert.StartsWith(script + "pre\n", editor.Text)
    Assert.Empty editor.Completions

    window.KeyTextInput "let b = "
    Dispatcher.UIThread.RunJobs()
    typeName ()
    press Avalonia.Input.Key.Tab
    Assert.True(editor.Text.TrimEnd().EndsWith "let b = pretrained", $"text: {editor.Text}; list: %A{editor.Completions}")
    window.Close()
