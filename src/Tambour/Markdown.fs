/// The Markdown a library's documentation is written in, drawn as a tooltip draws text: headings,
/// paragraphs, bulleted items and fenced code, with bold, italic and code within a line. Anything
/// else is shown as it was written.
module Tambour.Markdown

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Documents
open Avalonia.Layout
open Avalonia.Media

type Block =
    | Heading of level: int * text: string
    | Paragraph of string
    | Item of string
    | Code of string

/// The blocks of a text: a paragraph's lines joined, an item's own continuation lines with it.
let parse (text: string) : Block list =
    let blocks = ResizeArray<Block>()
    let lines = (if isNull text then "" else text).Replace("\r", "").Split('\n')
    let mutable i = 0
    let pending = Text.StringBuilder()
    let mutable item = false

    let flush () =
        if pending.Length > 0 then
            let t = pending.ToString()
            blocks.Add(if item then Item t else Paragraph t)
            pending.Clear() |> ignore

        item <- false

    while i < lines.Length do
        let line = lines.[i]
        let trimmed = line.Trim()

        if trimmed.StartsWith "```" then
            flush ()
            let code = ResizeArray<string>()
            i <- i + 1

            while i < lines.Length && not (lines.[i].Trim().StartsWith "```") do
                code.Add lines.[i]
                i <- i + 1

            blocks.Add(Code(String.Join("\n", code)))
        elif trimmed = "" then
            flush ()
        elif trimmed.StartsWith "#" && trimmed.TrimStart('#').StartsWith " " then
            flush ()
            let level = trimmed.Length - trimmed.TrimStart('#').Length
            blocks.Add(Heading(level, trimmed.TrimStart('#').Trim()))
        elif trimmed.StartsWith "- " || trimmed.StartsWith "* " then
            flush ()
            item <- true
            pending.Append(trimmed.Substring 2) |> ignore
        else
            if pending.Length > 0 then pending.Append ' ' |> ignore
            pending.Append trimmed |> ignore

        i <- i + 1

    flush ()
    List.ofSeq blocks

/// The pieces of a line: plain text, `code`, **bold** and *italic*, each marker taken only where its
/// closer follows.
type Span =
    | Plain of string
    | InlineCode of string
    | Bold of string
    | Italic of string

let spans (text: string) : Span list =
    let out = ResizeArray<Span>()
    let plain = Text.StringBuilder()
    let mutable i = 0

    let emit (span: Span) =
        if plain.Length > 0 then
            out.Add(Plain(plain.ToString()))
            plain.Clear() |> ignore

        out.Add span

    /// The closer of a marker opened at `i`, past the text it encloses, if there is one.
    let closer (marker: string) =
        let from = i + marker.Length

        if from < text.Length && not (Char.IsWhiteSpace text.[from]) then
            match text.IndexOf(marker, from) with
            | j when j > from -> Some j
            | _ -> None
        else
            None

    while i < text.Length do
        let rest = text.AsSpan i

        let taken =
            if rest.StartsWith "`" then
                match text.IndexOf('`', i + 1) with
                | j when j > i + 1 ->
                    emit (InlineCode(text.Substring(i + 1, j - i - 1)))
                    i <- j + 1
                    true
                | _ -> false
            elif rest.StartsWith "**" then
                match closer "**" with
                | Some j ->
                    emit (Bold(text.Substring(i + 2, j - i - 2)))
                    i <- j + 2
                    true
                | None -> false
            elif rest.StartsWith "*" then
                match closer "*" with
                | Some j ->
                    emit (Italic(text.Substring(i + 1, j - i - 1)))
                    i <- j + 1
                    true
                | None -> false
            else
                false

        if not taken then
            plain.Append text.[i] |> ignore
            i <- i + 1

    if plain.Length > 0 then out.Add(Plain(plain.ToString()))
    List.ofSeq out

let private inlines (look: Palette.Look) (text: string) =
    let collection = InlineCollection()

    for span in spans text do
        collection.Add(
            match span with
            | Plain t -> Run t
            | InlineCode t -> Run(t, FontFamily = Palette.code, Foreground = SolidColorBrush(Palette.colourOf look "type"))
            | Bold t -> Run(t, FontWeight = FontWeight.Bold)
            | Italic t -> Run(t, FontStyle = FontStyle.Italic)
        )

    collection

/// A text's blocks as controls, one under another, in a look's colours.
let view (look: Palette.Look) (text: string) : Control =
    let panel = StackPanel(Spacing = Tokens.Space.s2)
    let ink = SolidColorBrush look.Foreground

    for block in parse text do
        panel.Children.Add(
            match block with
            | Heading(level, t) ->
                TextBlock(
                    Inlines = inlines look t,
                    Foreground = ink,
                    FontWeight = FontWeight.SemiBold,
                    FontSize = (if level <= 2 then Tokens.Type.title else Tokens.Type.ui),
                    TextWrapping = TextWrapping.Wrap,
                    Margin = Thickness(0.0, Tokens.Space.s1, 0.0, 0.0)
                )
                :> Control
            | Paragraph t -> TextBlock(Inlines = inlines look t, Foreground = ink, TextWrapping = TextWrapping.Wrap)
            | Item t ->
                let row = DockPanel()
                let bullet = TextBlock(Text = "•", Foreground = ink, Margin = Thickness(Tokens.Space.s1, 0.0, Tokens.Space.s2, 0.0))
                DockPanel.SetDock(bullet, Dock.Left)
                row.Children.Add bullet
                row.Children.Add(TextBlock(Inlines = inlines look t, Foreground = ink, TextWrapping = TextWrapping.Wrap))
                row
            | Code t ->
                Border(
                    Background = SolidColorBrush look.Raised,
                    CornerRadius = CornerRadius Tokens.Radius.sm,
                    Padding = Thickness(Tokens.Space.s2, Tokens.Space.s1),
                    Child = TextBlock(Text = t, FontFamily = Palette.code, Foreground = ink, TextWrapping = TextWrapping.NoWrap)
                )
        )

    panel
