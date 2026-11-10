namespace Tambour

open System
open System.Threading.Tasks
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Documents
open Avalonia.Input
open Avalonia.Interactivity
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Styling
open Avalonia.Threading
open AvaloniaEdit
open AvaloniaEdit.CodeCompletion
open AvaloniaEdit.Document
open AvaloniaEdit.Editing
open AvaloniaEdit.Rendering
open AvaloniaEdit.TextMate
open TextMateSharp.Grammars
open Tambour.Protocol

/// What has been typed of the name being completed, which the list's entries show in bold.
type private Typed() =
    let changed = Event<string>()
    let mutable text = ""

    member _.Text
        with get () = text
        and set v =
            if v <> text then
                text <- v
                changed.Trigger v

    member _.Changed = changed.Publish

/// How the editor shows things outside its text: a completion's name, a signature in a tooltip.
module Shown =

    /// The editor's font, and the code in its tooltips.
    let code = Palette.code

    /// Which characters of a name the typed text matches, in order and ignoring case; none where it
    /// does not match them all.
    let matched (name: string) (typed: string) =
        let marks = Array.zeroCreate name.Length
        let mutable j = 0

        for i in 0 .. name.Length - 1 do
            if j < typed.Length && Char.ToLowerInvariant name.[i] = Char.ToLowerInvariant typed.[j] then
                marks.[i] <- true
                j <- j + 1

        if j = typed.Length then marks else Array.zeroCreate name.Length

    /// A name, its typed characters in bold.
    let showMatched (block: TextBlock) (name: string) (typed: string) =
        let marks = matched name typed
        let inlines = InlineCollection()
        let mutable i = 0

        while i < name.Length do
            let s = i

            while i < name.Length && marks.[i] = marks.[s] do
                i <- i + 1

            inlines.Add(Run(name.Substring(s, i - s), FontWeight = (if marks.[s] then FontWeight.Bold else FontWeight.Normal)))

        block.Inlines <- inlines

    /// What a keyword makes the name after it.
    let private naming =
        Map.ofList
            [ "module", "namespace"
              "namespace", "namespace"
              "from", "namespace"
              "type", "type"
              "class", "type"
              "struct", "struct"
              "interface", "interface"
              "union", "type"
              "exception", "type"
              "enum", "enum"
              "delegate", "type" ]

    let private keywords =
        set [ "val"; "member"; "property"; "type"; "module"; "namespace"; "new"; "static"; "abstract"; "override"
              "union"; "case"; "active"; "pattern"; "field"; "event"; "mutable"; "inherit"; "interface"; "exception"
              "struct"; "class"; "of"; "with"; "get"; "set"; "and"; "inline"; "private"; "internal" ]

    let private pieces = Text.RegularExpressions.Regex(@"[A-Za-z_][A-Za-z0-9_']*|\s+|.")

    /// A signature as code: its keywords, its name, its parameters and its types in their colours.
    let signature (look: Palette.Look) (text: string) =
        let inlines = InlineCollection()
        let brush (kind: string) = SolidColorBrush(Palette.colourOf look kind)
        let plain = SolidColorBrush look.Foreground
        let mutable named = false
        let mutable expectType = false
        let mutable depth = 0
        let mutable previous = ""
        let mutable lineStart = true
        // The kind the next name is, after `module`, `type` and the like.
        let mutable nameKind = "function"
        // The colour of the name before a dot, which the name after it continues.
        let mutable last: IBrush = plain
        let mutable dotted: IBrush option = None
        // `custom operation: name (Type)`, a computation expression's operation, its name drawn as
        // the editor draws the operation, a keyword, and its argument a type.
        let mutable custom = false
        // `Calls Type.Method` under it: the method the operation calls.
        let mutable calls = false

        for m in pieces.Matches text do
            let p = m.Value

            let coloured: IBrush =
                if Char.IsWhiteSpace p.[0] then
                    if p.Contains '\n' then
                        named <- false
                        expectType <- false
                        depth <- 0
                        lineStart <- true
                        nameKind <- "function"
                        custom <- false
                        calls <- false

                    plain
                elif Char.IsLetter p.[0] || p.[0] = '_' then
                    // `from Marey` under a module's or type's name: where it is from.
                    let isFrom = p = "from" && lineStart
                    let first = lineStart
                    lineStart <- false

                    if first && p = "Calls" then
                        calls <- true
                        plain
                    elif calls then
                        // A type before each dot, the method after the last.
                        let next = m.Index + m.Length
                        brush (if next < text.Length && text.[next] = '.' then "type" else "method")
                    elif first && p = "custom" then
                        brush "keyword"
                    elif p = "operation" && previous = " " && not named && not custom then
                        custom <- true
                        brush "keyword"
                    elif custom && not named then
                        named <- true
                        brush "keyword"
                    elif custom then
                        brush "type"
                    elif depth = 0 && not expectType && (keywords.Contains p || isFrom) then
                        match naming.TryFind p with
                        | Some kind when not named -> nameKind <- kind
                        | _ -> ()

                        brush "keyword"
                    elif depth > 0 || expectType then
                        brush "type"
                    elif not named then
                        named <- true
                        brush nameKind
                    else
                        brush "parameter"
                else
                    lineStart <- false

                    match p with
                    // The name follows the colon of `custom operation:`.
                    | ":" when custom && not named -> ()
                    | "<" -> depth <- depth + 1
                    | ">" when previous = "-" -> expectType <- true
                    | ">" -> depth <- max 0 (depth - 1)
                    | ":"
                    | "*" -> expectType <- true
                    | "("
                    | ")"
                    | ","
                    | "=" when depth = 0 -> expectType <- false
                    | _ -> ()

                    plain

            let colour =
                match dotted with
                | Some qualified when (Char.IsLetter p.[0] || p.[0] = '_') && not calls -> qualified
                | _ -> coloured

            dotted <- if p = "."&& (Char.IsLetter previous.[0] || previous.[0] = '_') then Some last else None
            last <- colour
            previous <- p
            inlines.Add(Run(p, Foreground = colour))

        inlines

/// One completion as the completion list shows and inserts it: its kind's glyph, and its name in
/// the kind's colour with what has been typed of it in bold. The kind's name is its description.
/// Where a hover's tip goes beside the hovered word.
module Tip =

    type Side =
        | Above
        | Below

    /// Above the word where the tip fits, as an editor's hover is; below where only that fits; and
    /// where neither does, the side with more room, the tip slid into the window from there.
    let side (word: Rect) (tip: Size) (bounds: Rect) =
        let above, below = word.Top - bounds.Top, bounds.Bottom - word.Bottom

        if tip.Height <= above then Above
        elif tip.Height <= below then Below
        elif above >= below then Above
        else Below

type private CompletionItem(item: Completion, look: Palette.Look, typed: Typed) =
    let content =
        lazy
            (let mark, kind = Palette.glyphs.TryFind item.Glyph |> Option.defaultValue ("·", "")
             let colour = SolidColorBrush(Palette.colourOf look kind)

             let glyph =
                 Border(
                     Width = Tokens.Height.icon,
                     Height = Tokens.Height.icon,
                     CornerRadius = CornerRadius Tokens.Radius.sm,
                     BorderBrush = colour,
                     BorderThickness = Thickness Tokens.border,
                     Margin = Thickness(0.0, 0.0, Tokens.Space.s2, 0.0),
                     Child =
                         TextBlock(
                             Text = mark,
                             Foreground = colour,
                             FontSize = Tokens.Type.mark,
                             FontWeight = FontWeight.Bold,
                             HorizontalAlignment = HorizontalAlignment.Center,
                             VerticalAlignment = VerticalAlignment.Center
                         )
                 )

             let name = TextBlock(Foreground = colour, FontFamily = Shown.code, VerticalAlignment = VerticalAlignment.Center)
             Shown.showMatched name item.Name typed.Text
             typed.Changed.Add(Shown.showMatched name item.Name)
             let row = StackPanel(Orientation = Orientation.Horizontal)
             row.Children.Add glyph
             row.Children.Add name
             row)

    member _.Item = item

    interface ICompletionData with
        member _.Image = null
        member _.Text = item.Name
        member _.Content = box content.Value
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

module private Segments =

    /// A stretch of the script, counted as the language service counts (lines from 1, columns
    /// from 0), as offsets: at least one character, so that an empty range can be seen.
    let of' (doc: TextDocument) (startLine: int) (startColumn: int) (endLine: int) (endColumn: int) =
        let offset line column =
            let line = max 1 (min doc.LineCount line)
            let l = doc.GetLineByNumber line
            l.Offset + max 0 (min l.Length column)

        let s = offset startLine startColumn
        let e = max (offset endLine endColumn) (min doc.TextLength (s + 1))
        TextSegment(StartOffset = s, Length = max 0 (e - s))

    let ofProblem (doc: TextDocument) (p: Diagnostic) = of' doc p.StartLine p.StartColumn p.EndLine p.EndColumn

    let ofFinding (doc: TextDocument) (f: Finding) = of' doc f.StartLine f.StartColumn f.EndLine f.EndColumn

/// The squiggles under the script's problems, the marks on the uses of the symbol at the caret,
/// and on the bracket at the caret and its match.
type private Marks(document: unit -> TextDocument) =
    let mutable problems: Diagnostic[] = [||]
    let mutable uses: Span[] = [||]
    let mutable brackets: (int * int) option = None
    let mutable link: (int * int) option = None

    member val Look = Palette.light with get, set

    member _.Problems
        with get () = problems
        and set v = problems <- v

    member _.Uses
        with set v = uses <- v

    member _.Brackets
        with get () = brackets
        and set v = brackets <- v

    /// The name under the pointer while Ctrl is held, from an offset to another: underlined, as
    /// a click goes to its definition.
    member _.Link
        with get () = link
        and set v = link <- v

    interface IBackgroundRenderer with
        member _.Layer = KnownLayer.Selection

        member this.Draw(view: TextView, dc: DrawingContext) =
            let doc = document ()

            if not (isNull doc) && view.VisualLinesValid then
                let look = this.Look
                let useBrush = SolidColorBrush look.Uses

                for u in uses do
                    for r in BackgroundGeometryBuilder.GetRectsForSegment(view, Segments.of' doc u.StartLine u.StartColumn u.EndLine u.EndColumn, false) do
                        dc.FillRectangle(useBrush, r)

                match brackets with
                | Some(a, b) when max a b < doc.TextLength ->
                    let bracketBrush = SolidColorBrush look.Bracket

                    for o in [ a; b ] do
                        for r in BackgroundGeometryBuilder.GetRectsForSegment(view, TextSegment(StartOffset = o, Length = 1), false) do
                            dc.FillRectangle(bracketBrush, r)
                | _ -> ()

                match link with
                | Some(a, b) when b <= doc.TextLength && b > a ->
                    let pen = Pen(SolidColorBrush look.Foreground, 1.0)

                    for r in BackgroundGeometryBuilder.GetRectsForSegment(view, TextSegment(StartOffset = a, Length = b - a), false) do
                        dc.DrawLine(pen, Point(r.Left, r.Bottom - 0.5), Point(r.Right, r.Bottom - 0.5))
                | _ -> ()

                for p in problems do
                    let colour =
                        match p.Severity with
                        | "error" -> look.Error
                        | "warning" -> look.Warning
                        | _ -> look.Info

                    let pen = Pen(SolidColorBrush colour, 1.0)

                    for r in BackgroundGeometryBuilder.GetRectsForSegment(view, Segments.ofProblem doc p, false) do
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

/// One stretch of the document the compiler classified: where, what kind, and whether mutable.
[<Struct>]
type Classified =
    { Offset: int
      Length: int
      Kind: string
      Mutable: bool }

/// Semantic highlighting: each stretch the compiler classified, drawn in its kind's colour over the
/// grammar's colours, a mutable one underlined. The stretches move with each edit until the next
/// check replaces them, so nothing flickers while typing.
type private Classes() =
    inherit DocumentColorizingTransformer()

    let mutable runs: Classified[] = [||]
    let mutable look = Palette.light
    let mutable brushes: Map<string, IBrush> = Map.empty

    let brushOf (kind: string) =
        match brushes.TryFind kind with
        | Some b -> b
        | None ->
            let b = SolidColorBrush(Palette.colourOf look kind) :> IBrush
            brushes <- brushes.Add(kind, b)
            b

    member val Enabled = true with get, set

    member _.Look
        with get () = look
        and set v =
            look <- v
            brushes <- Map.empty

    /// In order, none overlapping.
    member _.Runs
        with get () = runs
        and set v = runs <- v

    member _.Move(e: DocumentChangeEventArgs) =
        runs <-
            runs
            |> Array.choose (fun r ->
                let s = e.GetNewOffset(r.Offset, AnchorMovementType.AfterInsertion)
                let t = e.GetNewOffset(r.Offset + r.Length, AnchorMovementType.BeforeInsertion)
                if t > s then Some { r with Offset = s; Length = t - s } else None)

    override this.ColorizeLine(line: DocumentLine) =
        if this.Enabled && runs.Length > 0 then
            let a = line.Offset
            let b = line.EndOffset

            // The first run that ends past the line's start.
            let mutable lo = 0
            let mutable hi = runs.Length

            while lo < hi do
                let mid = (lo + hi) / 2
                if runs.[mid].Offset + runs.[mid].Length <= a then lo <- mid + 1 else hi <- mid

            let mutable i = lo

            while i < runs.Length && runs.[i].Offset < b do
                let r = runs.[i]
                let s = max r.Offset a
                let t = min (r.Offset + r.Length) b

                if t > s then
                    let brush = brushOf r.Kind
                    let underline = r.Mutable

                    this.ChangeLinePart(
                        s,
                        t,
                        fun element ->
                            element.TextRunProperties.SetForegroundBrush brush

                            if underline then
                                element.TextRunProperties.SetTextDecorations TextDecorations.Underline
                    )

                i <- i + 1

/// What the editor does at its caret when the window's commands ask: their menu entries and keys,
/// which the preferences can change.
[<RequireQualifiedAccess>]
type EditorCommand =
    /// The completion list, as a dot opens it.
    | Complete
    /// A new name for the binding at the caret, wherever it is used.
    | Rename
    /// The menu of Marey's fixes for the call at the caret.
    | ShowFixes
    /// Where the name at the caret is defined, as a Ctrl+click on it goes.
    | GoToDefinition

/// The script editor: AvaloniaEdit with F# highlighting, the grammar's and the compiler's, in the
/// colours of the app's light or dark theme, and what the language service says about the script.
/// Problems are underlined and their messages shown on hover, the symbol at the caret is marked
/// where it is used and the bracket at it with its match, a name typed or a dot completes, a
/// parenthesis or a comma shows the call's overloads, the pointer resting on a name shows its
/// tooltip, and the right button on a Marey call short of its unit witnesses offers the fixes for
/// it. Completion, renaming and the fixes are also the window's commands, with keys of their own
/// (`Run`). Enter, Tab, Shift+Tab and Backspace indent
/// as F# is indented, and brackets and quotes are closed as they are opened.
type ScriptEditor() as this =
    inherit UserControl()

    let editor =
        TextEditor(ShowLineNumbers = true, FontFamily = Palette.codeFor Preferences.Live.Current.Ligatures, FontSize = Preferences.Live.Current.EditorSize)

    let registry = RegistryOptions ThemeName.LightPlus
    let marks = Marks(fun () -> editor.Document)
    let classes = Classes()

    /// The grammar's colouring, while the editor is in a window: TextMate's tokenizer holds a
    /// thread-pool thread for as long as it is installed, so a closed tab's editor gives it back.
    let mutable textMate: TextMate.Installation = null
    let mutable sized: IDisposable = { new IDisposable with member _.Dispose() = () }

    /// The UI thread's dispatcher when the editor was made. A reply posted after it is gone (a
    /// headless test's application torn down) goes nowhere, rather than reading
    /// `Dispatcher.UIThread`, which off the UI thread would make a new one on the calling thread.
    let ui = Dispatcher.UIThread

    let edited = Event<History.TextEdit * string>()
    let problemsChanged = Event<Diagnostic[]>()
    let caretMoved = Event<int * int>()
    let fixedBy = Event<string * string>()
    let defined = Event<Defined>()

    let mutable language: LanguageService = Unchecked.defaultof<_>
    let mutable path: string = null
    /// Counts edits, so an answer about an older text is set aside.
    let mutable version = 0
    let mutable settingText = false
    let mutable completion: CompletionWindow = null
    let mutable typed = Typed()
    /// Whether the completion open came by itself, as a name was typed.
    let mutable unasked = false
    /// Where the name begins whose list, come by itself, was closed: it does not come again in it.
    let mutable quietAt = -1
    /// Counts the choices made in a completion list, so a description of an earlier one is set aside.
    let mutable described = 0
    /// The description shown beside the completion list, as text; empty where none is.
    let mutable describedText = ""
    let mutable overloads: OverloadInsightWindow = null
    /// The opening parenthesis of the call whose overloads are shown; they go once the caret is
    /// in another call or none.
    let mutable overloadsCall: int option = None
    let mutable look = Palette.light
    let mutable dark = false

    /// Counts caret moves, as `version` counts edits.
    let mutable moves = 0
    /// Counts the checks asked for.
    let mutable checks = 0
    /// Counts hovers, so a tooltip arriving after the pointer moved on is set aside.
    let mutable hovers = 0
    let mutable tipText = ""

    /// Marey's findings in the last check, and the edit the check was of: their fixes are offered
    /// only while the text is the one checked.
    let mutable findings: Finding[] = [||]
    let mutable foundAt = -1
    /// The menu of fixes, while it is open.
    let mutable fixMenu: ContextMenu = null

    /// The closers the editor put in, while the caret stays on their line.
    let closers = ResizeArray<TextAnchor>()

    /// Runs `f` on the UI thread after `ms`, unless `still` no longer holds by then.
    let after (ms: int) (still: unit -> bool) (f: unit -> unit) =
        Task.Delay(ms).ContinueWith(fun (_: Task) -> ui.Post(fun () -> if still () then f ())) |> ignore

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
                        ui.Post(fun () -> if asked = version then answer t.Result))
            |> ignore

    /// The tokens of a check as stretches of the document, in order.
    let classify (tokens: Token[]) =
        let doc = editor.Document

        tokens
        |> Array.choose (fun t ->
            if t.Line >= 1 && t.Line <= doc.LineCount then
                let l = doc.GetLineByNumber t.Line
                let s = min t.StartColumn l.Length
                let e = min t.EndColumn l.Length

                if e > s then
                    Some
                        { Offset = l.Offset + s
                          Length = e - s
                          Kind = t.Kind
                          Mutable = t.Mutable }
                else
                    None
            else
                None)

    let check () =
        let text = editor.Text
        checks <- checks + 1

        ask (fun l -> l.CheckAsync(text, path)) (fun c ->
            marks.Problems <- c.Diagnostics
            findings <- c.Findings
            foundAt <- version
            classes.Runs <- classify c.Tokens
            editor.TextArea.TextView.Redraw()
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

    /// A name's signature in its colours, and its documentation below it.
    let describedView (h: Hovered) : Control =
        let panel = StackPanel(Spacing = Tokens.Space.s2)

        panel.Children.Add(
            TextBlock(Inlines = Shown.signature look h.Text, FontFamily = Shown.code, TextWrapping = TextWrapping.Wrap)
        )

        if not (String.IsNullOrWhiteSpace h.Doc) then
            panel.Children.Add(Markdown.view look h.Doc)

        panel

    /// Beside a completion list, the chosen entry's signature and documentation, once the choice
    /// has rested on it: the list and this panel side by side in the list's popup.
    let withDocs (window: CompletionWindow) =
        let docs =
            Border(
                Background = SolidColorBrush look.Background,
                BorderBrush = SolidColorBrush look.Line,
                BorderThickness = Thickness(Tokens.border),
                Padding = Thickness(Tokens.Space.s3, Tokens.Space.s2),
                Width = 400.0,
                MaxHeight = 320.0,
                IsVisible = false
            )

        let list = window.CompletionList
        // Wide enough for a long name, as AvaloniaEdit's own width cut them short, and the popup
        // past its own cap of 550 to hold the panel too.
        list.Width <- 280.0
        window.MaxWidth <- list.Width + docs.Width
        window.Child <- null
        let side = DockPanel()
        DockPanel.SetDock(docs, Dock.Right)
        side.Children.Add docs
        side.Children.Add list
        window.Child <- side

        list.SelectionChanged.Add(fun _ ->
            described <- described + 1
            let mine = described
            let current () = mine = described && obj.ReferenceEquals(completion, window)

            match list.SelectedItem with
            | null ->
                docs.IsVisible <- false
                describedText <- ""
            | item ->
                after 350 current (fun () ->
                    let line, column = caret ()
                    let text = editor.Text

                    ask (fun l -> l.DescribeAsync(text, path, line, column, item.Text)) (fun h ->
                        if current () then
                            if String.IsNullOrWhiteSpace h.Text then
                                docs.IsVisible <- false
                                describedText <- ""
                            else
                                docs.Child <- ScrollViewer(Content = describedView h)
                                docs.IsVisible <- true
                                describedText <- h.Text + (if String.IsNullOrWhiteSpace h.Doc then "" else "\n" + h.Doc))))

    /// Whether a completion's name takes what was typed of it.
    let takes (typedText: string) (name: string) = typedText = "" || Array.exists id (Shown.matched name typedText)

    /// Opens the completion list at the caret: asked for (a dot, Ctrl+Space), or come by itself
    /// as a name is typed, and then only where something completes what was typed.
    let complete (byItself: bool) =
        let line, column = caret ()
        let text = editor.Text

        ask (fun l -> l.CompleteAsync(text, path, line, column)) (fun c ->
            let start = wordStart ()
            let sofar = editor.Document.GetText(start, editor.CaretOffset - start)

            let wanted =
                not byItself
                || (isNull completion && start <> quietAt && Typing.completesAsTyped editor.Text editor.CaretOffset
                    && c.Items |> Array.exists (fun i -> takes sofar i.Name))

            if c.Items.Length > 0 && wanted then
                if not (isNull completion) then completion.Close()
                let window = CompletionWindow(editor.TextArea)
                withDocs window
                window.StartOffset <- start
                typed <- Typed()
                typed.Text <- sofar

                for item in c.Items do
                    window.CompletionList.CompletionData.Add(CompletionItem(item, look, typed))

                window.Closed.Add(fun _ ->
                    if obj.ReferenceEquals(completion, window) then
                        if unasked then quietAt <- window.StartOffset
                        describedText <- ""
                        completion <- null)

                completion <- window
                unasked <- byItself
                window.Show()
                if sofar <> "" then window.CompletionList.SelectItem sofar)

    let signature () =
        let line, column = caret ()
        let text = editor.Text

        ask (fun l -> l.SignatureAsync(text, path, line, column)) (fun s ->
            if not (isNull overloads) then overloads.Close()

            let call = Typing.enclosingCall editor.Text editor.CaretOffset

            if s.Methods.Length > 0 && call.IsSome then
                let window = OverloadInsightWindow(editor.TextArea, Provider = Overloads(s.Methods, s.Active))

                window.Closed.Add(fun _ ->
                    if obj.ReferenceEquals(overloads, window) then
                        overloads <- null)

                overloads <- window
                overloadsCall <- call
                window.Show())

    /// What typing a character sets off: completion after a dot, the overloads after an opening
    /// parenthesis or a comma.
    let reactTo (input: string) =
        match input with
        | "." -> complete false
        | letter when
            letter.Length = 1
            && isNull completion
            && (Char.IsLetterOrDigit letter.[0] || letter.[0] = '_')
            && wordStart () <> quietAt
            && Typing.completesAsTyped editor.Text editor.CaretOffset
            ->
            complete true
        | "("
        | "," -> signature ()
        | ")" -> if not (isNull overloads) then overloads.Close()
        | _ -> ()

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
                    ui.Post(fun () ->
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

    // ---- Typing --------------------------------------------------------------------------------

    let isInserted (offset: int) =
        closers |> Seq.exists (fun a -> not a.IsDeleted && a.Offset = offset)

    let selection () =
        let area = editor.TextArea

        if area.Selection.IsEmpty then
            editor.CaretOffset, editor.CaretOffset
        else
            let s = area.Selection.SurroundingSegment
            s.Offset, s.EndOffset

    /// Makes a change as one edit, and puts the selection where it says.
    let apply (c: Typing.Change) =
        let doc = editor.Document

        if c.Length > 0 || c.Text.Length > 0 then
            doc.Replace(c.Offset, c.Length, c.Text)

        if c.SelectionStart = c.SelectionEnd then
            editor.TextArea.ClearSelection()
            editor.CaretOffset <- c.SelectionEnd
        else
            editor.Select(c.SelectionStart, c.SelectionEnd - c.SelectionStart)

        if c.Closer >= 0 then
            closers.Add(doc.CreateAnchor c.Closer)

        editor.TextArea.Caret.BringCaretToView()

    let onKey (e: KeyEventArgs) =
        let text () = editor.Text

        match e.Key, e.KeyModifiers with
        | Key.Tab, KeyModifiers.None when not (isNull completion) ->
            // Tab takes the list's choice, as Enter does; past the list it would move the focus.
            e.Handled <- true
            completion.CompletionList.RequestInsertion e
        | Key.Tab, KeyModifiers.None when isNull completion ->
            e.Handled <- true
            let s, t = selection ()
            apply (Typing.indent (text ()) s t)
        | Key.Tab, KeyModifiers.Shift when isNull completion ->
            e.Handled <- true
            let s, t = selection ()
            apply (Typing.outdent (text ()) s t)
        | Key.Back, KeyModifiers.None ->
            let s, t = selection ()

            match Typing.backspace (text ()) s t isInserted with
            | Some c ->
                e.Handled <- true
                apply c
            | None -> ()
        | _ -> ()

    let onTextEntering (e: TextInputEventArgs) =
        let s, t = selection ()

        match e.Text with
        | "\n"
        | "\r"
        | "\r\n" ->
            e.Handled <- true
            apply (Typing.enter editor.Text s t)
        // Closers put in as an opener is typed, unless the user turned that off.
        | input when not (isNull input) && Preferences.Live.Current.AutoClose ->
            match Typing.typed editor.Text s t input isInserted with
            | Some c ->
                e.Handled <- true
                apply c
                reactTo input
            | None -> ()
        | _ -> ()

    // ---- Hover -----------------------------------------------------------------------------------

    let problemsAt (offset: int) =
        let doc = editor.Document

        marks.Problems
        |> Array.filter (fun p ->
            let s = Segments.ofProblem doc p
            s.StartOffset <= offset && offset < s.EndOffset)

    /// Marey's findings whose call holds an offset, or ends at it, while the text is the one
    /// checked.
    let findingsAt (offset: int) =
        if foundAt <> version then
            [||]
        else
            let doc = editor.Document

            findings
            |> Array.filter (fun f ->
                let s = Segments.ofFinding doc f
                s.StartOffset <= offset && offset <= s.EndOffset)

    let tipView (problems: Diagnostic[]) (found: Finding[]) (hovered: Hovered option) : Control =
        let panel = StackPanel(Spacing = Tokens.Space.s2, MaxWidth = 640.0)
        let texts = ResizeArray<string>()

        for p in problems do
            let colour =
                match p.Severity with
                | "error" -> look.Error
                | "warning" -> look.Warning
                | _ -> look.Info

            let message = $"{p.Code}: {p.Message}"
            texts.Add message
            panel.Children.Add(TextBlock(Text = message, Foreground = SolidColorBrush colour, TextWrapping = TextWrapping.Wrap))

        // What Marey says the call wants, and the key that offers its fixes.
        for f in found do
            let message = $"{f.Code}: {f.Message}"
            texts.Add message
            panel.Children.Add(TextBlock(Text = message, Foreground = SolidColorBrush look.Info, TextWrapping = TextWrapping.Wrap))

            for fix in f.Fixes do
                let offer =
                    match ScriptEditor.FixKey() with
                    | "" -> $"Right-click to fix: {fix.Title}"
                    | key -> $"{key} to fix: {fix.Title}"

                texts.Add offer
                panel.Children.Add(TextBlock(Text = offer, Foreground = SolidColorBrush look.Info, FontStyle = FontStyle.Italic, TextWrapping = TextWrapping.Wrap))

        match hovered with
        | Some h when not (String.IsNullOrWhiteSpace h.Text) ->
            texts.Add h.Text
            if not (String.IsNullOrWhiteSpace h.Doc) then texts.Add h.Doc
            panel.Children.Add(describedView h)
        | _ -> ()

        tipText <- String.Join("\n", texts)
        panel

    /// The hovered word's box in the editor, which the tooltip sits on.
    let mutable hovered = Rect()

    /// The word at an offset, or the one character there, as a box in the editor: from the top of
    /// its line to the bottom.
    let wordBox (offset: int) =
        let doc = editor.Document
        let view = editor.TextArea.TextView
        let inWord i = i >= 0 && i < doc.TextLength && (let c = doc.GetCharAt i in Char.IsLetterOrDigit c || c = '_' || c = '\'')
        let mutable first = offset
        let mutable last = offset

        while inWord (first - 1) do
            first <- first - 1

        while inWord last do
            last <- last + 1

        if last = first then last <- min doc.TextLength (first + 1)

        let at (o: int) (y: VisualYPosition) =
            let p = view.GetVisualPosition(TextViewPosition(doc.GetLocation o), y) - view.ScrollOffset
            view.TranslatePoint(p, editor) |> Option.ofNullable |> Option.defaultValue p

        let top = at first VisualYPosition.LineTop
        let bottom = at last VisualYPosition.LineBottom
        Rect(top, Point(max bottom.X (top.X + 1.0), bottom.Y))

    let showTip (content: Control) =
        ToolTip.SetTip(editor, content)
        ToolTip.SetIsOpen(editor, true)

    /// What the pointer resting at an offset shows: the problems there at once, then the symbol's
    /// tooltip below them when the language service answers.
    let hoverAt (offset: int) =
        hovers <- hovers + 1
        let hover = hovers
        let problems = problemsAt offset
        let found = findingsAt offset
        let location = editor.Document.GetLocation offset
        let text = editor.Text
        tipText <- ""
        hovered <- wordBox offset

        if problems.Length > 0 || found.Length > 0 then
            showTip (tipView problems found None)

        ask (fun l -> l.HoverAsync(text, path, location.Line, location.Column - 1)) (fun h ->
            if hover = hovers && (problems.Length > 0 || found.Length > 0 || not (String.IsNullOrWhiteSpace h.Text)) then
                showTip (tipView problems found (Some h)))

    // ---- Definitions -----------------------------------------------------------------------------

    /// The name at an offset, or just before it, as the offsets it starts and ends at.
    let nameAt (offset: int) =
        let doc = editor.Document
        let inWord i = i >= 0 && i < doc.TextLength && (let c = doc.GetCharAt i in Char.IsLetterOrDigit c || c = '_' || c = '\'')
        let mutable first = offset
        let mutable last = offset

        while inWord (first - 1) do
            first <- first - 1

        while inWord last do
            last <- last + 1

        if last > first && not (Char.IsDigit(doc.GetCharAt first)) then Some(first, last) else None

    /// Asks where the name at an offset is defined, and says so through `Defined`.
    let definitionAt (offset: int) =
        let location = editor.Document.GetLocation offset
        let text = editor.Text
        ask (fun l -> l.DefinitionAsync(text, path, location.Line, location.Column - 1)) defined.Trigger

    /// The document offset under the pointer, if it is over text.
    let pointedAt (e: PointerEventArgs) =
        let view = editor.TextArea.TextView

        view.GetPositionFloor(e.GetPosition view + view.ScrollOffset)
        |> Option.ofNullable
        |> Option.map (fun p -> editor.Document.GetOffset p.Location)

    let hand = lazy (new Cursor(StandardCursorType.Hand))

    /// Underlines the name under the pointer while Ctrl is held, and takes the underline away
    /// otherwise; only where the language service can say where it is defined.
    let showLink (target: (int * int) option) =
        if marks.Link <> target then
            marks.Link <- target
            editor.TextArea.TextView.Cursor <- (if target.IsSome then hand.Value else null)
            redraw ()

    // ---- Fixes -----------------------------------------------------------------------------------

    /// Replaces only what differs from `value`, as one edit, not reported as typed.
    let replaceText (value: string) =
        let current = editor.Text

        if value <> current then
            let edit = History.TextEdit.between current value
            settingText <- true

            try
                editor.Document.Replace(edit.Offset, edit.Removed.Length, edit.Inserted)
            finally
                settingText <- false

    /// Makes a fix's edits as one change, which the app's history takes as one entry.
    let applyFix (fix: Fix) =
        let before = editor.Text
        let after = Analysis.applyFix before fix

        if after <> before then
            replaceText after
            fixedBy.Trigger(fix.Title, after)

    /// The fixes for the findings at an offset, as a menu: at the pointer, or under the word at
    /// the offset. False where there are none.
    let openFixes (offset: int) (atPointer: bool) =
        let fixes = findingsAt offset |> Array.collect (fun f -> f.Fixes)

        if fixes.Length = 0 then
            false
        else
            let items =
                [ for fix in fixes do
                      let item = MenuItem(Header = fix.Title)
                      item.Click.Add(fun _ -> applyFix fix)
                      item ]

            let menu = ContextMenu(ItemsSource = items, PlacementTarget = editor)

            if not atPointer then
                menu.Placement <- PlacementMode.AnchorAndGravity
                menu.PlacementRect <- Nullable(wordBox offset)
                menu.PlacementAnchor <- Primitives.PopupPositioning.PopupAnchor.BottomLeft
                menu.PlacementGravity <- Primitives.PopupPositioning.PopupGravity.BottomRight

            menu.Closed.Add(fun _ -> if obj.ReferenceEquals(fixMenu, menu) then fixMenu <- null)
            ToolTip.SetIsOpen(editor, false)
            fixMenu <- menu
            menu.Open editor
            true

    // ---- The look ------------------------------------------------------------------------------

    let applyLook () =
        dark <- this.ActualThemeVariant = ThemeVariant.Dark
        look <- Palette.forVariant this.ActualThemeVariant
        if not (isNull textMate) then textMate.SetTheme(Palette.grammarTheme look)
        editor.Background <- SolidColorBrush look.Background
        editor.Foreground <- SolidColorBrush look.Foreground
        editor.LineNumbersForeground <- SolidColorBrush look.LineNumbers
        editor.TextArea.SelectionBrush <- SolidColorBrush look.Selection
        editor.TextArea.TextView.CurrentLineBackground <- SolidColorBrush look.CurrentLine
        editor.TextArea.TextView.CurrentLineBorder <- Pen(Brushes.Transparent, 0.0)
        // The tips on the editor's page, so code in them is drawn on what its colours are for.
        this.Resources.["ToolTipBackground"] <- SolidColorBrush look.Background
        this.Resources.["ToolTipForeground"] <- SolidColorBrush look.Foreground
        this.Resources.["ToolTipBorderBrush"] <- SolidColorBrush look.Line
        classes.Look <- look
        marks.Look <- look
        editor.TextArea.TextView.Redraw()

    let install () =
        if isNull textMate then
            textMate <- TextMate.InstallTextMate(editor, registry, true, null)
            textMate.SetGrammar(registry.GetScopeByLanguageId "fsharp")
            textMate.SetTheme(Palette.grammarTheme look)
            // After the grammar's colouriser, so the compiler's classes are drawn over it.
            editor.TextArea.TextView.LineTransformers.Remove classes |> ignore
            editor.TextArea.TextView.LineTransformers.Add classes

    let uninstall () =
        if not (isNull textMate) then
            textMate.Dispose()
            textMate <- null

    do
        install ()
        editor.TextArea.TextView.BackgroundRenderers.Add marks
        editor.Options.ConvertTabsToSpaces <- true
        editor.Options.IndentationSize <- Typing.Width
        editor.Options.HighlightCurrentLine <- true
        this.Content <- editor
        applyLook ()

        // On the hovered word, as an editor's hover is: above its line and starting where it
        // starts, or below the line where there is room only there (`Tip.side`). Avalonia's own
        // flip keeps a tip that fits on neither side where it was, off the window's edge; the side
        // is chosen here against the window, where Linux draws popups.
        ToolTip.SetPlacement(editor, PlacementMode.Custom)

        ToolTip.SetCustomPopupPlacementCallback(
            editor,
            Primitives.PopupPositioning.CustomPopupPlacementCallback(fun p ->
                // The rectangle Avalonia offers is the editor's own bounds in the window's
                // coordinates; the word's box is in the editor's.
                let word = hovered.Translate(Vector(p.AnchorRectangle.X, p.AnchorRectangle.Y))
                p.AnchorRectangle <- word

                let window =
                    match TopLevel.GetTopLevel editor with
                    | null -> Rect(p.PopupSize)
                    | top -> Rect(top.ClientSize)

                match Tip.side word p.PopupSize window with
                | Tip.Above ->
                    p.Anchor <- Primitives.PopupPositioning.PopupAnchor.TopLeft
                    p.Gravity <- Primitives.PopupPositioning.PopupGravity.TopRight
                | Tip.Below ->
                    p.Anchor <- Primitives.PopupPositioning.PopupAnchor.BottomLeft
                    p.Gravity <- Primitives.PopupPositioning.PopupGravity.BottomRight
                // Not the tooltip's own 20 px below the pointer.
                p.Offset <- Point()
                p.ConstraintAdjustment <-
                    Primitives.PopupPositioning.PopupPositionerConstraintAdjustment.SlideY
                    ||| Primitives.PopupPositioning.PopupPositionerConstraintAdjustment.SlideX)
        )
        this.ActualThemeVariantChanged.Add(fun _ -> applyLook ())

        // The app keeps the history, so AvaloniaEdit keeps none.
        editor.Document.UndoStack.SizeLimit <- 0

        editor.TextArea.Caret.PositionChanged.Add(fun _ ->
            caretMoved.Trigger(editor.TextArea.Caret.Line, editor.TextArea.Caret.Column))

        editor.TextChanged.Add(fun _ ->
            version <- version + 1
            marks.Uses <- [||]
            let edit = version
            after 250 (fun () -> edit = version) check)

        editor.Document.Changed.Add(fun e ->
            classes.Move e

            if not settingText then
                let edit: History.TextEdit =
                    { Offset = e.Offset
                      Removed = e.RemovedText.Text
                      Inserted = e.InsertedText.Text }

                edited.Trigger(edit, editor.Document.Text))

        editor.TextArea.Caret.PositionChanged.Add(fun _ ->
            moves <- moves + 1
            let move = moves
            let line = editor.TextArea.Caret.Line
            closers.RemoveAll(fun a -> a.IsDeleted || a.Line <> line) |> ignore
            marks.Brackets <- Typing.matching editor.Text editor.CaretOffset

            // The overloads go once the caret has left their call: its parenthesis deleted, the
            // caret past its closer or on to another line outside it.
            if not (isNull overloads) && Typing.enclosingCall editor.Text editor.CaretOffset <> overloadsCall then
                overloads.Close()
            redraw ()

            if not (isNull completion) && editor.CaretOffset >= completion.StartOffset then
                typed.Text <- editor.Document.GetText(completion.StartOffset, editor.CaretOffset - completion.StartOffset)

                // A list that came by itself goes once nothing in it takes what was typed.
                if unasked
                   && not (completion.CompletionList.CompletionData |> Seq.exists (fun d -> takes typed.Text (string d.Text))) then
                    completion.Close()

            after 300 (fun () -> move = moves) markUses)

        editor.TextArea.TextEntering.Add onTextEntering
        editor.TextArea.TextEntered.Add(fun e -> reactTo e.Text)
        editor.TextArea.AddHandler(InputElement.KeyDownEvent, EventHandler<KeyEventArgs>(fun _ e -> onKey e), RoutingStrategies.Tunnel)

        // The right button on a call Marey has fixes for offers them; elsewhere it does nothing,
        // as before.
        editor.ContextRequested.Add(fun e ->
            let view = editor.TextArea.TextView

            let offset =
                match e.TryGetPosition view with
                | true, p ->
                    view.GetPositionFloor(p + view.ScrollOffset)
                    |> Option.ofNullable
                    |> Option.map (fun p -> editor.Document.GetOffset p.Location)
                | _ -> Some editor.CaretOffset

            match offset with
            | Some o when openFixes o true -> e.Handled <- true
            | _ -> ())

        editor.PointerHover.Add(fun e ->
            let view = editor.TextArea.TextView

            match view.GetPositionFloor(e.GetPosition view + view.ScrollOffset) |> Option.ofNullable with
            | Some p -> hoverAt (editor.Document.GetOffset(p.Location))
            | None -> ())

        // Ctrl+click on a name goes to its definition; with Ctrl held the name under the pointer
        // is underlined, as a link is.
        let linking (m: KeyModifiers) = m.HasFlag KeyModifiers.Control && not (obj.ReferenceEquals(language, null))

        editor.TextArea.AddHandler(
            InputElement.PointerMovedEvent,
            EventHandler<PointerEventArgs>(fun _ e ->
                showLink (if linking e.KeyModifiers then pointedAt e |> Option.bind nameAt else None)),
            RoutingStrategies.Tunnel,
            true
        )

        editor.TextArea.AddHandler(
            InputElement.PointerPressedEvent,
            EventHandler<PointerPressedEventArgs>(fun _ e ->
                if linking e.KeyModifiers && e.GetCurrentPoint(editor.TextArea).Properties.IsLeftButtonPressed then
                    match pointedAt e |> Option.bind (fun o -> nameAt o |> Option.map (fun _ -> o)) with
                    | Some offset ->
                        e.Handled <- true
                        showLink None
                        editor.CaretOffset <- offset
                        definitionAt offset
                    | None -> ()),
            RoutingStrategies.Tunnel
        )

        editor.TextArea.AddHandler(InputElement.KeyUpEvent, EventHandler<KeyEventArgs>(fun _ e -> if not (linking e.KeyModifiers) then showLink None), RoutingStrategies.Tunnel, true)
        editor.TextArea.PointerExited.Add(fun _ -> showLink None)

        editor.PointerHoverStopped.Add(fun _ ->
            hovers <- hovers + 1
            ToolTip.SetIsOpen(editor, false))

    /// The script's text. Setting it replaces only what differs, as one edit, so the caret stays
    /// where it was outside it; the edit is not reported as typed.
    member _.Text
        with get () = editor.Text
        and set (value: string) = replaceText value

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

    /// The calls Marey's witnesses found short in the last check, with their fixes.
    member _.Findings = findings

    /// Opens the menu of fixes for the call at an offset, as Show fixes at the caret does; false
    /// where it has none, or the text has changed since the check.
    member _.OfferFixes(offset: int) = openFixes offset false

    /// Does a command of the window's at the caret.
    member _.Run(command: EditorCommand) =
        editor.TextArea.Focus() |> ignore

        match command with
        | EditorCommand.Complete -> complete false
        | EditorCommand.Rename -> rename ()
        | EditorCommand.ShowFixes -> openFixes editor.CaretOffset false |> ignore
        | EditorCommand.GoToDefinition -> definitionAt editor.CaretOffset

    /// The key that shows the fixes, as a tooltip names it; empty where none does. The window
    /// sets it from its commands, whose keys the preferences change.
    static member val FixKey: unit -> string = (fun () -> "Ctrl+.") with get, set

    /// The menu of fixes, while it is open; null otherwise.
    member _.FixMenu = fixMenu

    /// The stretches the last check classified, moved with the edits since.
    member _.Classified = classes.Runs

    /// Whether the compiler's classes are drawn; the grammar's colours alone where not.
    member _.Semantic
        with get () = classes.Enabled
        and set (value: bool) =
            classes.Enabled <- value
            editor.TextArea.TextView.Redraw()

    /// The colours the editor is drawn in, which follow the app's theme.
    member _.Look = look

    /// How many checks the editor has asked for.
    member _.Checks = checks

    /// The editor underneath, for what this control does not wrap.
    override _.OnAttachedToVisualTree(e: VisualTreeAttachmentEventArgs) =
        base.OnAttachedToVisualTree e
        install ()
        // The type size and ligatures as the preferences have them, now and as they change while
        // shown.
        let follow (p: Preferences.Preferences) =
            editor.FontSize <- p.EditorSize
            let family = Palette.codeFor p.Ligatures
            if editor.FontFamily <> family then editor.FontFamily <- family

        follow Preferences.Live.Current
        sized <- Preferences.Live.Changed.Subscribe follow

    override _.OnDetachedFromVisualTree(e: VisualTreeAttachmentEventArgs) =
        base.OnDetachedFromVisualTree e
        uninstall ()
        sized.Dispose()

    member _.Editor = editor

    /// Whether the grammar's colouring is installed: while the editor is in a window.
    member _.Coloured = not (isNull textMate)

    /// What the open completion list offers, or nothing when none is open.
    /// The signature and documentation shown beside the completion list; empty where none is.
    member _.CompletionDocs = describedText

    /// Whether a call's overloads are shown.
    member _.ShowsOverloads = not (isNull overloads)

    member _.Completions =
        if isNull completion then [||] else completion.CompletionList.CompletionData |> Seq.map (fun d -> d.Text) |> Array.ofSeq

    /// Renames the binding at the caret, as F2 does once a name is typed.
    member _.RenameAsync(name: string) : Task<bool> = renameTo name

    /// Shows what the pointer resting on the character at an offset shows.
    member _.HoverAt(offset: int) = hoverAt offset

    /// The text of the tooltip last shown: the problems' messages, the signature, its documentation.
    member _.TipText = tipText

    /// Asks where the name at an offset is defined, as Ctrl+click on it does.
    member _.DefinitionAt(offset: int) = definitionAt offset

    /// The name underlined for Ctrl+click, as offsets; none while none is.
    member _.Link = marks.Link

    /// Whether the text can only be read, as a library's source is shown.
    member _.ReadOnly
        with get () = editor.IsReadOnly
        and set (value: bool) = editor.IsReadOnly <- value

    /// Moves the caret to a line and column (from 1 and 0) and brings it into view.
    member _.GoTo(line: int, column: int) =
        let doc = editor.Document
        let l = doc.GetLineByNumber(max 1 (min doc.LineCount line))
        editor.CaretOffset <- l.Offset + max 0 (min l.Length column)

        // A source opened at a definition shows its line a third of the way down; the script
        // scrolls only as far as the line needs.
        let place () =
            if editor.IsReadOnly then
                let view = editor.TextArea.TextView
                let top = view.GetVisualTopByDocumentLine l.LineNumber
                // TextEditor.ScrollToVerticalOffset leaves the offset where it is; its scroll
                // viewer's own takes it.
                match Avalonia.VisualTree.VisualExtensions.GetVisualDescendants editor |> Seq.tryPick (function :? Avalonia.Controls.ScrollViewer as v -> Some v | _ -> None) with
                | Some v -> v.Offset <- Vector(v.Offset.X, max 0.0 (top - view.Bounds.Height / 3.0))
                | None -> editor.ScrollTo(l.LineNumber, column + 1)
            else
                editor.ScrollTo(l.LineNumber, column + 1)

        place ()

        // A view just shown, or given its text just now, scrolls once it is laid out: until then
        // its extent is the old text's, and the offset is held to it.
        if editor.IsReadOnly then
            let mutable tries = 0
            let mutable handler: EventHandler = null

            handler <-
                EventHandler(fun _ _ ->
                    tries <- tries + 1
                    place ()

                    if editor.VerticalOffset > 0.0 || tries > 10 then
                        editor.LayoutUpdated.RemoveHandler handler)

            editor.LayoutUpdated.AddHandler handler
        editor.Focus() |> ignore

    /// Each change typed into the document, and the text after it. A change made through `Text`
    /// is not one.
    [<CLIEvent>]
    member _.Edited = edited.Publish

    /// The language service's problems, after each check.
    [<CLIEvent>]
    member _.ProblemsChanged = problemsChanged.Publish

    /// A fix made, by its title, and the text after it. Its change is not one `Edited` reports.
    [<CLIEvent>]
    member _.Fixed = fixedBy.Publish

    /// Where the caret is, as a line and a column from 1, each time it moves.
    [<CLIEvent>]
    member _.CaretMoved = caretMoved.Publish

    /// Where a name is defined, as go to definition found it.
    [<CLIEvent>]
    member _.Defined = defined.Publish
