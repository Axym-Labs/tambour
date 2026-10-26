namespace Tambour

open System
open System.Threading.Tasks
open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Media
open Avalonia.Threading
open AvaloniaEdit
open AvaloniaEdit.CodeCompletion
open AvaloniaEdit.Document
open AvaloniaEdit.Editing
open AvaloniaEdit.Rendering
open AvaloniaEdit.TextMate
open TextMateSharp.Grammars
open Tambour.Protocol

/// One completion as the completion list shows and inserts it.
type private CompletionItem(item: Completion) =
    interface ICompletionData with
        member _.Image = null
        member _.Text = item.Name
        member _.Content = box $"{item.Name}    {item.Kind}"
        member _.Description = box item.Kind
        member _.Priority = 0.0

        member _.Complete(area: TextArea, segment: ISegment, _: EventArgs) =
            area.Document.Replace(segment, item.Insert)

/// The overloads of a call, for AvaloniaEdit's insight window.
type private Overloads(methods: string[], active: int) =
    let mutable selected = max 0 (min active (methods.Length - 1))
    let changed = Event<ComponentModel.PropertyChangedEventHandler, ComponentModel.PropertyChangedEventArgs>()

    interface IOverloadProvider with
        member _.SelectedIndex
            with get () = selected
            and set v =
                selected <- v
                changed.Trigger(null, ComponentModel.PropertyChangedEventArgs "CurrentHeader")

        member _.Count = methods.Length
        member _.CurrentIndexText = if methods.Length > 1 then $"{selected + 1} of {methods.Length}" else null
        member _.CurrentHeader = box methods.[selected]
        member _.CurrentContent = null

    interface ComponentModel.INotifyPropertyChanged with
        [<CLIEvent>]
        member _.PropertyChanged = changed.Publish

/// The squiggles under the script's problems and the marks on the uses of the symbol at the caret.
type private Marks(document: unit -> TextDocument) =
    let mutable problems: Diagnostic[] = [||]
    let mutable uses: Span[] = [||]

    let segment (doc: TextDocument) (startLine: int) (startColumn: int) (endLine: int) (endColumn: int) =
        let offset line column =
            let line = max 1 (min doc.LineCount line)
            let l = doc.GetLineByNumber line
            l.Offset + max 0 (min l.Length column)

        let s = offset startLine startColumn
        let e = max (offset endLine endColumn) (min doc.TextLength (s + 1))
        TextSegment(StartOffset = s, Length = max 0 (e - s))

    member _.Problems
        with get () = problems
        and set v = problems <- v

    member _.Uses
        with set v = uses <- v

    interface IBackgroundRenderer with
        member _.Layer = KnownLayer.Selection

        member _.Draw(view: TextView, dc: DrawingContext) =
            let doc = document ()

            if not (isNull doc) && view.VisualLinesValid then
                let useBrush = SolidColorBrush(Color.FromArgb(60uy, 120uy, 160uy, 230uy))

                for u in uses do
                    for r in BackgroundGeometryBuilder.GetRectsForSegment(view, segment doc u.StartLine u.StartColumn u.EndLine u.EndColumn, false) do
                        dc.FillRectangle(useBrush, r)

                for p in problems do
                    let colour =
                        match p.Severity with
                        | "error" -> Color.FromRgb(220uy, 40uy, 40uy)
                        | "warning" -> Color.FromRgb(200uy, 150uy, 0uy)
                        | _ -> Color.FromRgb(80uy, 140uy, 200uy)

                    let pen = Pen(SolidColorBrush colour, 1.0)

                    for r in BackgroundGeometryBuilder.GetRectsForSegment(view, segment doc p.StartLine p.StartColumn p.EndLine p.EndColumn, false) do
                        // A wave along the bottom of the text.
                        let geometry = StreamGeometry()

                        use g = geometry.Open()
                        let y = r.Bottom - 1.0
                        let mutable x = r.Left
                        let mutable up = true
                        g.BeginFigure(Point(x, y), false)

                        while x < r.Right do
                            x <- min r.Right (x + 2.0)
                            g.LineTo(Point(x, (if up then y - 2.0 else y)))
                            up <- not up

                        g.EndFigure false
                        dc.DrawGeometry(null, pen, geometry)

/// The script editor: AvaloniaEdit with F# highlighting, and what the language service says about
/// the script. Problems are underlined, the symbol at the caret is marked where it is used, a dot
/// or Ctrl+Space completes, a parenthesis or a comma shows the call's overloads, the pointer
/// resting on a name shows its tooltip, and F2 renames a binding the script makes.
type ScriptEditor() as this =
    inherit UserControl()

    let editor =
        TextEditor(ShowLineNumbers = true, FontFamily = FontFamily "Cascadia Code,Consolas,DejaVu Sans Mono,monospace", FontSize = 13.0)

    let registry = RegistryOptions ThemeName.LightPlus
    let textMate = TextMate.InstallTextMate(editor, registry, true, null)
    let marks = Marks(fun () -> editor.Document)

    let textChanged = Event<string>()
    let problemsChanged = Event<Diagnostic[]>()

    let mutable language: LanguageService = Unchecked.defaultof<_>
    let mutable path: string = null
    /// Counts edits, so an answer about an older text is set aside.
    let mutable version = 0
    let mutable settingText = false
    let mutable completion: CompletionWindow = null
    let mutable overloads: OverloadInsightWindow = null

    /// Counts caret moves, as `version` counts edits.
    let mutable moves = 0

    /// Runs `f` on the UI thread after `ms`, unless `still` no longer holds by then.
    let after (ms: int) (still: unit -> bool) (f: unit -> unit) =
        Task.Delay(ms).ContinueWith(fun (_: Task) -> Dispatcher.UIThread.Post(fun () -> if still () then f ())) |> ignore

    let redraw () = editor.TextArea.TextView.InvalidateLayer KnownLayer.Selection

    /// The caret as the language service counts it: line from 1, column from 0.
    let caret () =
        let c = editor.TextArea.Caret
        c.Line, c.Column - 1

    /// Asks the language service, and hands the answer to `use` on the UI thread if the text is
    /// still the one asked about.
    let ask (question: LanguageService -> Task<'a>) (answer: 'a -> unit) =
        if not (obj.ReferenceEquals(language, null)) then
            let asked = version

            (question language)
                .ContinueWith(fun (t: Task<'a>) ->
                    if t.IsCompletedSuccessfully then
                        Dispatcher.UIThread.Post(fun () -> if asked = version then answer t.Result))
            |> ignore

    let check () =
        let text = editor.Text

        ask (fun l -> l.CheckAsync(text, path)) (fun c ->
            marks.Problems <- c.Diagnostics
            redraw ()
            problemsChanged.Trigger c.Diagnostics)

    let markUses () =
        let line, column = caret ()
        let text = editor.Text

        ask (fun l -> l.UsesAsync(text, path, line, column)) (fun u ->
            marks.Uses <- (if u.Spans.Length > 1 then u.Spans else [||])
            redraw ())

    /// Where the identifier the caret is in, or just after, begins.
    let wordStart () =
        let doc = editor.Document
        let mutable s = editor.CaretOffset

        while s > 0 && (Char.IsLetterOrDigit(doc.GetCharAt(s - 1)) || doc.GetCharAt(s - 1) = '_' || doc.GetCharAt(s - 1) = '\'') do
            s <- s - 1

        s

    let complete () =
        let line, column = caret ()
        let text = editor.Text

        ask (fun l -> l.CompleteAsync(text, path, line, column)) (fun c ->
            if c.Items.Length > 0 then
                if not (isNull completion) then completion.Close()
                let window = CompletionWindow(editor.TextArea)
                window.StartOffset <- wordStart ()

                for item in c.Items do
                    window.CompletionList.CompletionData.Add(CompletionItem item)

                window.Closed.Add(fun _ -> completion <- null)
                completion <- window
                window.Show())

    let signature () =
        let line, column = caret ()
        let text = editor.Text

        ask (fun l -> l.SignatureAsync(text, path, line, column)) (fun s ->
            if not (isNull overloads) then overloads.Close()

            if s.Methods.Length > 0 then
                let window = OverloadInsightWindow(editor.TextArea, Provider = Overloads(s.Methods, s.Active))
                window.Closed.Add(fun _ -> overloads <- null)
                overloads <- window
                window.Show())

    /// Renames the binding at the caret everywhere the script uses it, as one edit. False when
    /// the symbol there is not one the script defines.
    let renameTo (name: string) : Task<bool> =
        let result = TaskCompletionSource<bool>()
        let line, column = caret ()
        let text = editor.Text

        if obj.ReferenceEquals(language, null) then
            result.SetResult false
        else
            let asked = version

            language
                .UsesAsync(text, path, line, column)
                .ContinueWith(fun (t: Task<Used>) ->
                    Dispatcher.UIThread.Post(fun () ->
                        if not t.IsCompletedSuccessfully || asked <> version || not t.Result.Renamable || t.Result.Spans.Length = 0 then
                            result.TrySetResult false |> ignore
                        else
                            let doc = editor.Document
                            doc.BeginUpdate()

                            try
                                // From the end, so earlier offsets stay where they are.
                                for s in t.Result.Spans |> Array.sortByDescending (fun s -> s.StartLine, s.StartColumn) do
                                    let l = doc.GetLineByNumber s.StartLine
                                    doc.Replace(l.Offset + s.StartColumn, s.EndColumn - s.StartColumn, name)
                            finally
                                doc.EndUpdate()

                            result.TrySetResult true |> ignore))
            |> ignore

        result.Task

    let rename () =
        let line, column = caret ()
        let text = editor.Text

        ask (fun l -> l.UsesAsync(text, path, line, column)) (fun u ->
            if u.Renamable && u.Spans.Length > 0 then
                let box = TextBox(Text = u.Name, MinWidth = 160.0)
                let flyout = Flyout(Content = box)

                box.KeyDown.Add(fun e ->
                    match e.Key with
                    | Key.Enter when not (String.IsNullOrWhiteSpace box.Text) && box.Text.Trim() <> u.Name ->
                        e.Handled <- true
                        flyout.Hide()
                        renameTo (box.Text.Trim()) |> ignore
                    | Key.Escape -> flyout.Hide()
                    | _ -> ())

                flyout.ShowAt editor
                box.SelectAll()
                box.Focus() |> ignore)

    do
        textMate.SetGrammar(registry.GetScopeByLanguageId "fsharp")
        editor.TextArea.TextView.BackgroundRenderers.Add marks
        editor.Options.ConvertTabsToSpaces <- true
        editor.Options.IndentationSize <- 4
        this.Content <- editor

        editor.TextChanged.Add(fun _ ->
            version <- version + 1
            marks.Uses <- [||]
            let edit = version
            after 250 (fun () -> edit = version) check

            if not settingText then
                textChanged.Trigger editor.Text)

        editor.TextArea.Caret.PositionChanged.Add(fun _ ->
            moves <- moves + 1
            let move = moves
            after 300 (fun () -> move = moves) markUses)

        editor.TextArea.TextEntered.Add(fun e ->
            match e.Text with
            | "." -> complete ()
            | "("
            | "," -> signature ()
            | ")" -> if not (isNull overloads) then overloads.Close()
            | _ -> ())

        editor.TextArea.KeyDown.Add(fun e ->
            match e.Key, e.KeyModifiers with
            | Key.Space, KeyModifiers.Control ->
                e.Handled <- true
                complete ()
            | Key.F2, _ ->
                e.Handled <- true
                rename ()
            | _ -> ())

        editor.PointerHover.Add(fun e ->
            match editor.GetPositionFromPoint(e.GetPosition editor) |> Option.ofNullable with
            | Some p ->
                let text = editor.Text

                ask (fun l -> l.HoverAsync(text, path, p.Line, p.Column - 1)) (fun h ->
                    if not (String.IsNullOrWhiteSpace h.Text) then
                        ToolTip.SetTip(editor, h.Text)
                        ToolTip.SetIsOpen(editor, true))
            | None -> ())

        editor.PointerHoverStopped.Add(fun _ -> ToolTip.SetIsOpen(editor, false))

    /// The script's text. Setting it replaces the document; the edit is not reported as typed.
    member _.Text
        with get () = editor.Text
        and set (value: string) =
            if value <> editor.Text then
                settingText <- true

                try
                    editor.Text <- value
                finally
                    settingText <- false

    /// The language service the editor asks; the script is checked as soon as there is one.
    member _.Language
        with get () = language
        and set (value: LanguageService) =
            if not (obj.ReferenceEquals(value, language)) then
                language <- value
                check ()

    /// Where the script is on disk, or null.
    member _.Path
        with get () = path
        and set (value: string) = path <- value

    /// The problems the language service last found.
    member _.Problems = marks.Problems

    /// The editor underneath, for what this control does not wrap.
    member _.Editor = editor

    /// What the open completion list offers, or nothing when none is open.
    member _.Completions =
        if isNull completion then [||] else completion.CompletionList.CompletionData |> Seq.map (fun d -> d.Text) |> Array.ofSeq

    /// Renames the binding at the caret, as F2 does once a name is typed.
    member _.RenameAsync(name: string) : Task<bool> = renameTo name

    /// Moves the caret to a line and column (from 1 and 0) and brings it into view.
    member _.GoTo(line: int, column: int) =
        let doc = editor.Document
        let l = doc.GetLineByNumber(max 1 (min doc.LineCount line))
        editor.CaretOffset <- l.Offset + max 0 (min l.Length column)
        editor.ScrollTo(l.LineNumber, column + 1)
        editor.Focus() |> ignore

    /// The text as typed, after each edit.
    [<CLIEvent>]
    member _.TextChanged = textChanged.Publish

    /// The language service's problems, after each check.
    [<CLIEvent>]
    member _.ProblemsChanged = problemsChanged.Publish
