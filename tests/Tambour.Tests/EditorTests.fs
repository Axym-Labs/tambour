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

/// A name typed letter by letter opens the completion, the chosen entry's documentation beside it
/// once the choice rests; Enter and Tab take the choice.
[<AvaloniaFact>]
let ``completion comes as a name is typed, with the chosen entry's documentation`` () =
    use language = new LanguageService(command, { References = [||]; Prelude = Shell.prelude })
    let editor = ScriptEditor(Language = language)
    let window = Window(Width = 800.0, Height = 500.0, Content = editor)
    window.Show()
    let script = "/// Four times the number given.\nlet quadrupled (x: float) = 4.0 * x\nlet a = "
    editor.Text <- script
    editor.Editor.CaretOffset <- script.Length
    editor.Editor.TextArea.Focus() |> ignore

    let typeName () =
        for letter in [ "q"; "u"; "a" ] do
            window.KeyTextInput letter
            Dispatcher.UIThread.RunJobs()

        pumpUntil 30.0 "the completion list" (fun () -> editor.Completions.Length > 0)
        Assert.Contains("quadrupled", editor.Completions)

    let press (key: Avalonia.Input.Key) =
        window.KeyPress(key, Avalonia.Input.RawInputModifiers.None, Avalonia.Input.PhysicalKey.None, null)
        window.KeyRelease(key, Avalonia.Input.RawInputModifiers.None, Avalonia.Input.PhysicalKey.None, null)
        Dispatcher.UIThread.RunJobs()

    typeName ()
    pumpUntil 30.0 "the entry's documentation" (fun () -> editor.CompletionDocs <> "")
    Assert.Contains("quadrupled", editor.CompletionDocs)
    Assert.Contains("Four times the number given.", editor.CompletionDocs)
    window.CaptureRenderedFrame().Dispose()

    match Environment.GetEnvironmentVariable "TAMBOUR_CHECK_OUT" with
    | null
    | "" -> ()
    | dir ->
        use frame = window.CaptureRenderedFrame()
        frame.Save(Path.Combine(dir, "completion-docs.png"), Avalonia.Media.Imaging.PngBitmapEncoderOptions.Default)

    press Avalonia.Input.Key.Enter
    Assert.EndsWith("let a = quadrupled", editor.Text)
    Assert.Empty editor.Completions
    Assert.Equal("", editor.CompletionDocs)

    window.KeyTextInput "\nlet b = "
    Dispatcher.UIThread.RunJobs()
    typeName ()
    press Avalonia.Input.Key.Tab
    Assert.EndsWith("let b = quadrupled", editor.Text.TrimEnd())
    window.Close()

/// A call's overloads, shown on its opening parenthesis, go once the caret leaves the call: the
/// parenthesis taken back, or the caret on to another line.
[<AvaloniaFact>]
let ``a call's overloads go once the caret leaves the call`` () =
    use language = new LanguageService(command, { References = [||]; Prelude = Shell.prelude })
    let editor = ScriptEditor(Language = language)
    let window = Window(Width = 800.0, Height = 500.0, Content = editor)
    window.Show()
    let script = "let xs = [| 3.0; 1.0 |]\n\nlet s = Array.sortBy"
    editor.Text <- script
    editor.Editor.CaretOffset <- script.Length
    editor.Editor.TextArea.Focus() |> ignore

    let press (key: Avalonia.Input.Key) =
        window.KeyPress(key, Avalonia.Input.RawInputModifiers.None, Avalonia.Input.PhysicalKey.None, null)
        window.KeyRelease(key, Avalonia.Input.RawInputModifiers.None, Avalonia.Input.PhysicalKey.None, null)
        Dispatcher.UIThread.RunJobs()

    let opened () =
        window.KeyTextInput "("
        Dispatcher.UIThread.RunJobs()
        pumpUntil 30.0 "the overloads" (fun () -> editor.ShowsOverloads)

    // Taken back: Backspace removes the parenthesis and the closer put in with it.
    opened ()
    press Avalonia.Input.Key.Back
    Assert.Equal(script, editor.Text)
    Assert.False editor.ShowsOverloads

    // Typed on inside the call, they stay; on to another line, outside it, they go.
    opened ()
    window.KeyTextInput "fun x"
    Dispatcher.UIThread.RunJobs()
    Assert.True editor.ShowsOverloads
    editor.Editor.CaretOffset <- script.IndexOf "\n\n" + 1
    Dispatcher.UIThread.RunJobs()
    Assert.False editor.ShowsOverloads
    window.Close()

/// The grammar's colouring is held while the editor is in a window and given back when it leaves:
/// TextMate's tokenizer holds a thread-pool thread while installed, and twenty editors left behind
/// by closed tabs held twenty. Shown again, the editor colours again, the compiler's classes still
/// drawn over the grammar's.
[<AvaloniaFact>]
let ``an editor gives its colouring back when its window closes, and takes it again when shown`` () =
    let editor = ScriptEditor()
    Assert.True editor.Coloured
    let first = Window(Width = 400.0, Height = 300.0, Content = editor)
    first.Show()
    editor.Text <- "let x = 1\n"
    Dispatcher.UIThread.RunJobs()
    first.Close()
    Dispatcher.UIThread.RunJobs()
    Assert.False editor.Coloured

    first.Content <- null
    let second = Window(Width = 400.0, Height = 300.0, Content = editor)
    second.Show()
    Dispatcher.UIThread.RunJobs()
    Assert.True editor.Coloured
    let transformers = editor.Editor.TextArea.TextView.LineTransformers
    Assert.Equal("Classes", (Seq.last transformers).GetType().Name)
    Assert.Equal(1, transformers |> Seq.filter (fun t -> t.GetType().Name = "Classes") |> Seq.length)
    second.Close()
