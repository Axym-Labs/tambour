/// The editor's colours: the compiler's classes drawn over the grammar's, from the check that
/// gives the problems; a problem's message on hover; a glyph and a colour for every completion
/// kind; the light and the dark look.
[<Xunit.Collection("workers")>]
module Tambour.Tests.HighlightTests

open System
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions
open System.Threading
open Avalonia.Controls
open Avalonia.Headless.XUnit
open Avalonia.Media
open Avalonia.Styling
open Avalonia.Threading
open Avalonia.VisualTree
open AvaloniaEdit.Rendering
open Tambour
open Tambour.Protocol
open Xunit

let private log (line: string) = TestContext.Current.TestOutputHelper.WriteLine line

/// A time the app must keep: as asked on a desk, three times it on a shared runner.
let private limit (ms: float) =
    if Environment.GetEnvironmentVariable "GITHUB_ACTIONS" = "true" then 3.0 * ms else ms

let private pumpUntil (seconds: float) (what: string) (ready: unit -> bool) =
    // A shared runner is slower than a desk: three times the wait there.
    let seconds = if Environment.GetEnvironmentVariable "GITHUB_ACTIONS" = "true" then 3.0 * seconds else seconds
    let clock = Stopwatch.StartNew()

    while not (ready ()) do
        if clock.Elapsed.TotalSeconds > seconds then
            failwith $"{what} did not happen within {seconds} s"

        Dispatcher.UIThread.RunJobs()
        Thread.Sleep 10

let private pumpFor (ms: int) =
    let clock = Stopwatch.StartNew()

    while clock.ElapsedMilliseconds < int64 ms do
        Dispatcher.UIThread.RunJobs()
        Thread.Sleep 10

let private command: Command =
    { File = Path.Combine(AppContext.BaseDirectory, (if OperatingSystem.IsWindows() then "tambour.exe" else "tambour"))
      Arguments = [] }

let private init: AnalysisInit = { References = [||]; Prelude = Shell.prelude }

/// Where Marey's examples are: beside this checkout, or where CI puts them.
let private examples () =
    [ Environment.GetEnvironmentVariable "MAREY_EXAMPLES"
      Path.Combine(AppContext.BaseDirectory, "../../../../../../marey/examples")
      Path.Combine(AppContext.BaseDirectory, "../../../../../marey/examples") ]
    |> List.filter (fun d -> not (String.IsNullOrEmpty d) && Directory.Exists d)
    |> List.tryHead
    |> Option.map Path.GetFullPath

/// A Marey example as this checkout compiles it: its `#r "nuget: Marey…"` lines stand for the
/// pinned Marey the app ships with.
let private local (source: string) =
    Regex.Replace(source, @"^#r\s+""nuget:\s*Marey[^""]*""", "// Marey: referenced by the app", RegexOptions.Multiline)

/// The five edit scripts, and Marey's examples where there is a checkout of them.
let private scripts () =
    let edits =
        Directory.GetFiles(Path.Combine(AppContext.BaseDirectory, "scripts", "edits"), "*.fsx")
        |> Array.sort
        |> Array.map (fun f -> Path.GetFileName f, File.ReadAllText f)

    let marey =
        match examples () with
        | Some dir -> Directory.GetFiles(dir, "*.fsx") |> Array.sort |> Array.map (fun f -> Path.GetFileName f, local (File.ReadAllText f))
        | None -> [||]

    Array.append edits marey

/// The foreground of every character on screen, read back from the text view's visual lines.
let private drawn (editor: ScriptEditor) =
    let view = editor.Editor.TextArea.TextView
    let doc = editor.Editor.Document
    let colours = Array.create doc.TextLength Colors.Transparent

    for n in 1 .. doc.LineCount do
        let line = doc.GetLineByNumber n
        let visual = view.GetOrConstructVisualLine line

        for element in visual.Elements do
            match element.TextRunProperties.ForegroundBrush with
            | :? ISolidColorBrush as b ->
                let start = line.Offset + element.RelativeTextOffset

                for o in start .. min (start + element.DocumentLength) line.EndOffset - 1 do
                    colours.[o] <- b.Color
            | _ -> ()

    colours

/// The grammar colours its lines as it gets to them; read until two readings agree.
let private settled (editor: ScriptEditor) =
    let mutable last = drawn editor
    let mutable stable = false
    let clock = Stopwatch.StartNew()

    while not stable && clock.Elapsed.TotalSeconds < 10.0 do
        pumpFor 150
        editor.Editor.TextArea.TextView.Redraw()
        Dispatcher.UIThread.RunJobs()
        let next = drawn editor
        stable <- next = last
        last <- next

    last

/// Gate: on the edit scripts and Marey's examples that compile at the pin, every stretch the
/// compiler classifies is drawn in its class's colour, and every other character as the grammar
/// alone draws it.
[<AvaloniaFact>]
let ``every class the compiler finds is drawn in its colour, and nothing else is`` () =
    use language = new LanguageService(command, init)
    let service = Analysis.Service init
    let editor = ScriptEditor(Language = language)
    let window = Window(Width = 1000.0, Height = 700.0, Content = editor)
    window.Show()
    let failures = ResizeArray<string>()
    let mutable drawnScripts = 0
    let mutable tokens = 0

    for name, script in scripts () do
        let expected = service.Check { Id = 0; Text = script; Path = null }

        if expected.Diagnostics |> Array.exists (fun d -> d.Severity = "error") then
            log $"{name}: does not compile here, skipped"
        else
            let checks = editor.Checks
            let mutable answered = false
            use _ = editor.ProblemsChanged.Subscribe(fun _ -> answered <- true)
            editor.Text <- script
            pumpUntil 30.0 $"{name}'s check" (fun () -> answered && editor.Checks > checks)

            // The editor drew what the check sent.
            let doc = editor.Editor.Document

            let runs =
                expected.Tokens
                |> Array.map (fun t ->
                    let l = doc.GetLineByNumber t.Line
                    l.Offset + t.StartColumn, t.EndColumn - t.StartColumn, t.Kind)

            let held = editor.Classified |> Array.map (fun c -> c.Offset, c.Length, c.Kind)

            if runs <> held then
                failures.Add $"{name}: the editor holds {held.Length} stretches, the check gave {runs.Length}"
            else
                editor.Semantic <- false
                let grammar = settled editor
                editor.Semantic <- true
                let shown = settled editor
                let look = editor.Look
                let wanted = Array.copy grammar

                for o, length, kind in runs do
                    for i in o .. o + length - 1 do
                        wanted.[i] <- Palette.colourOf look kind

                let wrong =
                    [ for i in 0 .. wanted.Length - 1 do
                          if wanted.[i] <> shown.[i] && not (Char.IsWhiteSpace script.[i]) then
                              i ]

                match wrong with
                | [] ->
                    drawnScripts <- drawnScripts + 1
                    tokens <- tokens + runs.Length
                    log $"{name}: {runs.Length} stretches drawn"
                | i :: _ ->
                    let line = doc.GetLineByOffset i

                    failures.Add
                        $"{name}: {wrong.Length} characters wrong, the first at {line.LineNumber}:{i - line.Offset} ({shown.[i]}, wanted {wanted.[i]})"

    window.Close()
    log $"{drawnScripts} scripts, {tokens} stretches"
    Assert.True(drawnScripts >= 5, $"only {drawnScripts} scripts drawn")
    Assert.True(failures.Count = 0, String.Join("\n", failures))

/// Gate: a pause in typing is one check, and its classes are drawn within 500 ms of the last key,
/// on the longest script that compiles, warm.
[<AvaloniaFact>]
let ``a pause in typing is checked once, and classified within half a second`` () =
    use language = new LanguageService(command, init)
    let service = Analysis.Service init

    let name, script =
        scripts ()
        |> Array.filter (fun (_, s) -> not ((service.Check { Id = 0; Text = s; Path = null }).Diagnostics |> Array.exists (fun d -> d.Severity = "error")))
        |> Array.maxBy (fun (_, s) -> s.Length)

    let editor = ScriptEditor(Language = language)
    let window = Window(Width = 1000.0, Height = 700.0, Content = editor)
    window.Show()
    let mutable answers = 0
    use _ = editor.ProblemsChanged.Subscribe(fun _ -> answers <- answers + 1)
    editor.Text <- script + "\n"
    pumpUntil 30.0 "the first check" (fun () -> answers > 0)
    // Warm: the checks of a few pauses in typing, as after a minute's use.
    editor.Editor.CaretOffset <- editor.Text.Length
    editor.Editor.TextArea.Focus() |> ignore

    for i in 1 .. 5 do
        editor.Editor.TextArea.PerformTextInput $"let warm{i} = {i}\n"
        pumpUntil 30.0 "a warming check" (fun () -> answers > i)

    pumpFor 400

    let checks = editor.Checks
    let answered = answers
    let line = "\nlet tambourCheck = 42"

    let clock = Stopwatch()

    for c in line do
        pumpFor 30
        editor.Editor.TextArea.PerformTextInput(string c)
        clock.Restart()

    let offset = editor.Text.IndexOf "tambourCheck"
    let classified () = editor.Classified |> Array.exists (fun c -> c.Offset = offset && c.Kind = "variable")
    pumpUntil 10.0 "the classes of the new line" classified
    let elapsed = clock.Elapsed.TotalMilliseconds
    pumpFor 600
    log $"{name} ({script.Length} characters): classified {elapsed:F0} ms after the last key; {editor.Checks - checks} check(s)"
    window.Close()
    Assert.Equal(1, editor.Checks - checks)
    Assert.Equal(1, answers - answered)
    Assert.True(elapsed < limit 500.0, $"the classes came {elapsed:F0} ms after the last key")

/// Gate: hovering anywhere in a problem's range shows its message, before the symbol's tooltip.
[<AvaloniaFact>]
let ``a problem's message shows on hover anywhere in its range`` () =
    use language = new LanguageService(command, init)
    let script = File.ReadAllText(Path.Combine(AppContext.BaseDirectory, "scripts", "unit-error.fsx"))
    let editor = ScriptEditor(Language = language, Margin = Avalonia.Thickness(13.0, 0.0, 0.0, 0.0))
    // Below a bar and in from the window's edge, as in the app, so that a box taken in the wrong
    // coordinates shows.
    let bar = Border(Height = 47.0)
    DockPanel.SetDock(bar, Dock.Top)
    let panel = DockPanel()
    panel.Children.Add bar
    panel.Children.Add editor
    let window = Window(Width = 1000.0, Height = 700.0, Content = panel)
    window.Show()
    editor.Text <- script
    pumpUntil 30.0 "the unit error" (fun () -> editor.Problems |> Array.exists (fun p -> p.Code = "FS0001"))
    let doc = editor.Editor.Document
    let mutable hovered = 0

    for p in editor.Problems do
        let s = doc.GetLineByNumber(p.StartLine).Offset + p.StartColumn
        let e = doc.GetLineByNumber(p.EndLine).Offset + p.EndColumn

        for o in s .. max s (e - 1) do
            editor.HoverAt o
            Assert.Contains(p.Message, editor.TipText)
            hovered <- hovered + 1

    // The symbol's tooltip follows the problem's message.
    let p = editor.Problems |> Array.find (fun p -> p.Code = "FS0001")
    let o = doc.GetLineByNumber(p.StartLine).Offset + p.StartColumn + 2
    editor.HoverAt o
    pumpUntil 30.0 "the symbol's tooltip" (fun () -> editor.TipText.Contains "Displacement")
    Assert.StartsWith("FS0001", editor.TipText)

    // The tooltip sits on the hovered word: its bottom on the top of the word's line, its left
    // where the word starts.
    let tipOf () = (ToolTip.GetTip editor.Editor :?> Control).FindAncestorOfType<ToolTip>()
    pumpUntil 10.0 "the tooltip open" (fun () -> not (isNull (tipOf ())) && (tipOf ()).Bounds.Height > 0.0)
    let tip = tipOf ()
    let view = editor.Editor.TextArea.TextView
    let mutable start = o

    while start > 0 && Char.IsLetterOrDigit(doc.GetCharAt(start - 1)) do
        start <- start - 1

    let word = view.GetVisualPosition(AvaloniaEdit.TextViewPosition(doc.GetLocation start), VisualYPosition.LineTop) - view.ScrollOffset
    let word = Avalonia.VisualExtensions.PointToScreen(view, word)
    let corner = Avalonia.VisualExtensions.PointToScreen(tip, Avalonia.Point(0.0, tip.Bounds.Height))
    log $"{hovered} places hovered; the tooltip's bottom left at {corner}, the word's top left at {word}"
    Assert.True(abs (corner.Y - word.Y) <= 1 && abs (corner.X - word.X) <= 1, $"the tooltip's bottom left is at {corner}, the word's top left at {word}")
    window.Close()

/// Gate: every kind of completion has a glyph, and a colour in both looks.
[<Fact>]
let ``every completion kind has a glyph and a colour`` () =
    let kinds =
        if typeof<FSharp.Compiler.EditorServices.FSharpGlyph>.IsEnum then
            Enum.GetNames typeof<FSharp.Compiler.EditorServices.FSharpGlyph>
        else
            Microsoft.FSharp.Reflection.FSharpType.GetUnionCases typeof<FSharp.Compiler.EditorServices.FSharpGlyph>
            |> Array.map (fun c -> c.Name)

    Assert.NotEmpty kinds

    for k in kinds do
        match Palette.glyphs.TryFind k with
        | None -> failwith $"{k} has no glyph"
        | Some(mark, kind) ->
            Assert.False(String.IsNullOrWhiteSpace mark, k)

            for look in [ Palette.light; Palette.dark ] do
                Assert.True(kind = "error" || look.Kinds.ContainsKey kind, $"{k}'s kind {kind} has no colour")

/// Every class the language service sends has a colour in both looks.
[<Fact>]
let ``every class the compiler sends has a colour`` () =
    let cases =
        if typeof<FSharp.Compiler.EditorServices.SemanticClassificationType>.IsEnum then
            Enum.GetValues typeof<FSharp.Compiler.EditorServices.SemanticClassificationType>
            |> Seq.cast<FSharp.Compiler.EditorServices.SemanticClassificationType>
            |> Array.ofSeq
        else
            Microsoft.FSharp.Reflection.FSharpType.GetUnionCases typeof<FSharp.Compiler.EditorServices.SemanticClassificationType>
            |> Array.map (fun c -> Microsoft.FSharp.Reflection.FSharpValue.MakeUnion(c, [||]) :?> FSharp.Compiler.EditorServices.SemanticClassificationType)

    for c in cases do
        match Analysis.classOf c with
        | Some(kind, _) ->
            Assert.True(Palette.light.Kinds.ContainsKey kind && Palette.dark.Kinds.ContainsKey kind, $"{c} as {kind}")
        | None -> ()

/// The editor takes the dark look in a dark window, and the light one back.
[<AvaloniaFact>]
let ``the editor follows the app's theme`` () =
    let editor = ScriptEditor()
    let window = Window(Width = 600.0, Height = 400.0, Content = editor, RequestedThemeVariant = ThemeVariant.Dark)
    window.Show()
    Dispatcher.UIThread.RunJobs()
    Assert.Equal(Palette.dark, editor.Look)
    Assert.Equal(Palette.dark.Background, (editor.Editor.Background :?> ISolidColorBrush).Color)
    window.RequestedThemeVariant <- ThemeVariant.Light
    Dispatcher.UIThread.RunJobs()
    Assert.Equal(Palette.light, editor.Look)
    Assert.Equal(Palette.light.Background, (editor.Editor.Background :?> ISolidColorBrush).Color)
    window.Close()


/// A hover's tip goes above the word where it fits, below it where only that fits, and where
/// neither does to the side with more room: never off the window on the side it had no room.
[<Fact>]
let ``a tip goes to the side of the word with room for it`` () =
    let window = Avalonia.Rect(0.0, 0.0, 1000.0, 500.0)
    let word top = Avalonia.Rect(100.0, top, 40.0, 20.0)
    let tip height = Avalonia.Size(400.0, height)
    Assert.Equal(Tip.Above, Tip.side (word 300.0) (tip 200.0) window)
    // Near the top: no room above, room below.
    Assert.Equal(Tip.Below, Tip.side (word 40.0) (tip 200.0) window)
    // Taller than either side: the larger one, below for a word near the top (the reported case)…
    Assert.Equal(Tip.Below, Tip.side (word 40.0) (tip 470.0) window)
    // …and above for one near the bottom.
    Assert.Equal(Tip.Above, Tip.side (word 440.0) (tip 470.0) window)
