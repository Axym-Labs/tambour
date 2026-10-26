/// What typing does in the script editor beyond putting in what was typed: indentation, F# aware,
/// on Enter, Tab and Shift+Tab; Backspace in indentation; brackets and quotes closed as they are
/// opened. Each is a function of the text and the selection, giving the one change to make.
module Tambour.Typing

open System

/// One replacement, and the selection after it, the caret at its end. `Closer` is where a closer
/// this change put in begins, so that typing it steps over it; -1 where there is none.
type Change =
    { Offset: int
      Length: int
      Text: string
      SelectionStart: int
      SelectionEnd: int
      Closer: int }

/// Columns an indentation level takes. F# has no use for anything between.
[<Literal>]
let Width = 4

let private spaces (n: int) = String(' ', n)

let private isIdentifier (c: char) = Char.IsLetterOrDigit c || c = '_' || c = '\''

/// A string literal, a character literal or a comment: where it begins and ends (past its last
/// character), and whether it ends there or runs on (a line comment, a literal never closed).
type private Literal =
    { Start: int
      End: int
      Open: bool
      Comment: bool }

/// The strings, characters and comments of a script, in order.
let private literals (text: string) =
    let found = ResizeArray<Literal>()
    let n = text.Length
    let at (i: int) = if i < n then text.[i] else '\000'
    let mutable i = 0

    let add start stop isOpen =
        found.Add
            { Start = start
              End = stop
              Open = isOpen
              Comment = text.[start] = '/' || text.[start] = '(' }
        i <- stop

    while i < n do
        let c = text.[i]

        if c = '/' && at (i + 1) = '/' then
            let e = text.IndexOf('\n', i)
            add i (if e < 0 then n else e) true
        elif c = '(' && at (i + 1) = '*' && at (i + 2) <> ')' then
            // Nested, as F# nests them.
            let mutable depth = 1
            let mutable j = i + 2

            while depth > 0 && j < n do
                if text.[j] = '(' && at (j + 1) = '*' then
                    depth <- depth + 1
                    j <- j + 2
                elif text.[j] = '*' && at (j + 1) = ')' then
                    depth <- depth - 1
                    j <- j + 2
                else
                    j <- j + 1

            add i j (depth > 0)
        elif c = '"' && at (i + 1) = '"' && at (i + 2) = '"' then
            let e = text.IndexOf("\"\"\"", i + 3, StringComparison.Ordinal)
            if e < 0 then add i n true else add i (e + 3) false
        elif c = '"' then
            let verbatim = i > 0 && (text.[i - 1] = '@' || (text.[i - 1] = '$' && i > 1 && text.[i - 2] = '@'))
            let mutable j = i + 1
            let mutable closed = false

            while not closed && j < n do
                match text.[j] with
                | '\\' when not verbatim -> j <- j + 2
                | '"' when verbatim && at (j + 1) = '"' -> j <- j + 2
                | '"' ->
                    closed <- true
                    j <- j + 1
                | _ -> j <- j + 1

            add i (min j n) (not closed)
        elif c = '\'' && not (i > 0 && isIdentifier text.[i - 1]) then
            // A character, 'a' or '\n' or 'A'; otherwise a type parameter's mark.
            if at (i + 1) <> '\\' && at (i + 1) <> '\n' && at (i + 2) = '\'' then
                add i (i + 3) false
            elif at (i + 1) = '\\' then
                let e = text.IndexOf('\'', i + 2)

                if e > 0 && e - i <= 10 then add i (e + 1) false else i <- i + 1
            else
                i <- i + 1
        else
            i <- i + 1

    found.ToArray()

/// Whether a place in the text, between two characters, is in code rather than in a string, a
/// character or a comment.
let private within (offset: int) (l: Literal) =
    l.Start < offset && (offset < l.End || (l.Open && offset <= l.End))

let inCode (text: string) (offset: int) =
    literals text |> Array.forall (fun l -> not (within offset l))

/// Which characters are code, one flag each.
let codeMask (text: string) =
    let mask = Array.create text.Length true

    for l in literals text do
        for i in l.Start .. min (l.End - 1) (text.Length - 1) do
            mask.[i] <- false

    mask

/// The line a place is on: where it begins, and where it ends before its line break.
let private lineAround (text: string) (offset: int) =
    let start = if offset = 0 then 0 else text.LastIndexOf('\n', offset - 1) + 1
    let next = text.IndexOf('\n', offset)
    let stop = if next < 0 then text.Length else next
    let stop = if stop > start && text.[stop - 1] = '\r' then stop - 1 else stop
    start, stop

let private newlineOf (text: string) = if text.Contains "\r\n" then "\r\n" else "\n"

let private leading (line: string) =
    let mutable i = 0

    while i < line.Length && line.[i] = ' ' do
        i <- i + 1

    i

/// What a line ending in these opens, so that the next line goes one level deeper: the end of
/// a binding or a lambda's arrow, an open bracket, or a keyword whose body follows.
let private openers = [ "="; "->"; "<-"; "("; "["; "[|"; "{"; "{|" ]

let private keywords = [ "do"; "then"; "else"; "with"; "try"; "finally" ]

let private closerOf =
    dict [ "(", ")"; "[", "]"; "{", "}"; "[|", "|]"; "{|", "|}" ]

let private endsWithWord (code: string) (word: string) =
    code.EndsWith(word, StringComparison.Ordinal)
    && (code.Length = word.Length || not (isIdentifier code.[code.Length - word.Length - 1]))

/// Whether a line, its code up to where Enter is pressed, opens a deeper level. A `match … with`
/// and a `try`'s lone `with` do not: their cases begin at the `match`'s or the `try`'s column.
let private opens (code: string) =
    let trimmed = code.TrimStart()

    if endsWithWord code "with" then
        not (trimmed.StartsWith("match ", StringComparison.Ordinal) || trimmed = "with")
    else
        openers |> List.exists (fun o -> code.EndsWith(o, StringComparison.Ordinal))
        || keywords |> List.exists (endsWithWord code)

/// Enter: a line break, the new line indented as the one it breaks, one level deeper after a line
/// that opens one; between a bracket and its closer, the closer on a line of its own at the
/// opener's indentation. Spaces either side of the break are not kept. In a string, a break alone.
let enter (text: string) (selectionStart: int) (selectionEnd: int) : Change =
    let s, e = selectionStart, selectionEnd
    let newline = newlineOf text

    if literals text |> Array.exists (fun l -> not l.Comment && within s l) then
        { Offset = s
          Length = e - s
          Text = newline
          SelectionStart = s + newline.Length
          SelectionEnd = s + newline.Length
          Closer = -1 }
    else
        let lineStart, _ = lineAround text s
        let _, lineEnd = lineAround text e
        let before = text.Substring(lineStart, s - lineStart)
        let after = text.Substring(e, lineEnd - e)
        let kept = before.TrimEnd(' ')
        let from = lineStart + kept.Length
        let till = e + (after.Length - after.TrimStart(' ').Length)
        let rest = text.Substring(till, lineEnd - till)
        let indent = if kept.Length = 0 then before.Length else leading before

        // The line's code before the break: a comment after it does not count.
        let mask = codeMask text
        let mutable codeEnd = from

        while codeEnd > lineStart && (not mask.[codeEnd - 1] || text.[codeEnd - 1] = ' ') do
            codeEnd <- codeEnd - 1

        let code = text.Substring(lineStart, codeEnd - lineStart)
        // A line comment's line is judged by its code; in a block comment, the break is indented
        // as the line it breaks and no deeper.
        let inComment = literals text |> Array.exists (fun l -> l.Comment && text.[l.Start] = '(' && within s l)

        let opener =
            if inComment || codeEnd <> from then
                None
            else
                [ "[|"; "{|"; "("; "["; "{" ]
                |> List.tryFind (fun o -> code.EndsWith(o, StringComparison.Ordinal) && rest.StartsWith(closerOf.[o], StringComparison.Ordinal))

        let inserted, caret =
            match opener with
            | Some _ ->
                let first = newline + spaces (indent + Width)
                first + newline + spaces indent, first.Length
            | None ->
                let deeper = not inComment && opens code
                let line = newline + spaces (if deeper then indent + Width else indent)
                line, line.Length

        { Offset = from
          Length = till - from
          Text = inserted
          SelectionStart = from + caret
          SelectionEnd = from + caret
          Closer = -1 }

/// The whole lines a selection touches: where the first begins and the last ends. A selection
/// ending at a line's start does not touch that line.
let private linesOf (text: string) (s: int) (e: int) =
    let first, _ = lineAround text s
    let e = if e > s && e > 0 && text.[e - 1] = '\n' then e - 1 else e
    let _, last = lineAround text e
    first, last

let private eachLine (text: string) (first: int) (last: int) (f: string -> string) =
    let block = text.Substring(first, last - first)
    let lines = block.Split('\n')
    let changed = lines |> Array.map (fun l -> let body = l.TrimEnd('\r') in f body + l.Substring body.Length) |> String.concat "\n"
    changed

/// Tab: with nothing selected, spaces to the next level; with a selection, every line it touches a
/// level deeper (blank lines left blank), and those lines selected.
let indent (text: string) (selectionStart: int) (selectionEnd: int) : Change =
    let s, e = selectionStart, selectionEnd

    if s = e then
        let lineStart, _ = lineAround text s
        let n = Width - (s - lineStart) % Width

        { Offset = s
          Length = 0
          Text = spaces n
          SelectionStart = s + n
          SelectionEnd = s + n
          Closer = -1 }
    else
        let first, last = linesOf text s e
        let changed = eachLine text first last (fun l -> if l.Trim() = "" then l else spaces Width + l)

        { Offset = first
          Length = last - first
          Text = changed
          SelectionStart = first
          SelectionEnd = first + changed.Length
          Closer = -1 }

/// How many spaces taking one level off leaves out of `n`: back to the level before.
let private outOf (n: int) = if n = 0 then 0 elif n % Width = 0 then Width else n % Width

/// Shift+Tab: every line the selection touches, or the caret's line, a level shallower. The caret
/// stays on the text it was on; a selection selects the lines.
let outdent (text: string) (selectionStart: int) (selectionEnd: int) : Change =
    let s, e = selectionStart, selectionEnd
    let first, last = linesOf text s e
    let changed = eachLine text first last (fun l -> l.Substring(outOf (leading l)))

    if s = e then
        let removed = outOf (leading (text.Substring(first, last - first)))
        let caret = max first (s - removed)

        { Offset = first
          Length = last - first
          Text = changed
          SelectionStart = caret
          SelectionEnd = caret
          Closer = -1 }
    else
        { Offset = first
          Length = last - first
          Text = changed
          SelectionStart = first
          SelectionEnd = first + changed.Length
          Closer = -1 }

/// Backspace, where it does more than take one character: in a line's indentation, back to the
/// level before; right after an opener whose closer the editor put in, the closer with it (and
/// inside `[| |]` or `{| |}`, the bars). Elsewhere nothing, and the editor takes one character.
let backspace (text: string) (selectionStart: int) (selectionEnd: int) (inserted: int -> bool) : Change option =
    let s = selectionStart

    if selectionStart <> selectionEnd || s = 0 then
        None
    else
        let lineStart, _ = lineAround text s
        let before = text.Substring(lineStart, s - lineStart)
        let has (at: int) (what: string) = at >= 0 && at + what.Length <= text.Length && String.CompareOrdinal(text, at, what, 0, what.Length) = 0

        let remove offset length =
            Some
                { Offset = offset
                  Length = length
                  Text = ""
                  SelectionStart = offset
                  SelectionEnd = offset
                  Closer = -1 }

        if before.Length > 0 && before.Trim(' ').Length = 0 then
            let n = outOf before.Length
            remove (s - n) n
        elif not (inserted s) then
            None
        elif (has (s - 2) "[|" && has s "|]") || (has (s - 2) "{|" && has s "|}") then
            remove (s - 1) 2
        elif [ "()"; "[]"; "{}"; "\"\"" ] |> List.exists (fun p -> has (s - 1) p) then
            remove (s - 1) 2
        else
            None

/// A character typed, where the editor does more than put it in: a closer it put in is stepped
/// over; an opener (`(`, `[`, `{`, `"`) wraps a selection, or, in code and not before an
/// identifier, gets its closer; `|` inside a fresh `[]` or `{}` makes `[||]` or `{||}`. `<` and
/// `'` are never closed: in F# they are also less-than, a type parameter and a character.
let typed (text: string) (selectionStart: int) (selectionEnd: int) (input: string) (inserted: int -> bool) : Change option =
    let s, e = selectionStart, selectionEnd
    let next = if e < text.Length then text.[e] else '\000'
    let previous = if s > 0 then text.[s - 1] else '\000'

    let change offset length (put: string) caret closer =
        Some
            { Offset = offset
              Length = length
              Text = put
              SelectionStart = caret
              SelectionEnd = caret
              Closer = closer }

    let stepOver = s = e && s < text.Length && inserted s

    match input with
    | ")"
    | "]"
    | "}"
    | "\"" when stepOver && string next = input -> change s 0 "" (s + 1) -1
    | "|" when stepOver && (next = '|') && s + 1 < text.Length && (text.[s + 1] = ']' || text.[s + 1] = '}') -> change s 0 "" (s + 1) -1
    | "|" when s = e && inserted s && ((previous = '[' && next = ']') || (previous = '{' && next = '}')) && inCode text s ->
        change s 0 "||" (s + 1) (s + 1)
    | "("
    | "["
    | "{"
    | "\"" when s <> e && inCode text s ->
        let closer = if input = "\"" then "\"" else closerOf.[input]
        let selected = text.Substring(s, e - s)

        Some
            { Offset = s
              Length = e - s
              Text = input + selected + closer
              SelectionStart = s + 1
              SelectionEnd = s + 1 + selected.Length
              Closer = -1 }
    | "("
    | "["
    | "{" when s = e && inCode text s && not (isIdentifier next) -> change s 0 (input + closerOf.[input]) (s + 1) (s + 1)
    | "\"" when s = e && inCode text s && not (isIdentifier next) && not (isIdentifier previous) && previous <> '"' ->
        change s 0 "\"\"" (s + 1) (s + 1)
    | _ -> None

/// The bracket at the caret, just before it or else just after, and the one that matches it, as
/// offsets; strings, characters and comments are passed over.
let matching (text: string) (caret: int) : (int * int) option =
    let mask = codeMask text
    let opening = "([{"
    let closing = ")]}"
    let isBracket i = i >= 0 && i < text.Length && mask.[i] && (opening.IndexOf text.[i] >= 0 || closing.IndexOf text.[i] >= 0)

    let at =
        if isBracket (caret - 1) then Some(caret - 1)
        elif isBracket caret then Some caret
        else None

    at
    |> Option.bind (fun i ->
        let c = text.[i]
        let o = opening.IndexOf c

        let target, step, self =
            if o >= 0 then closing.[o], 1, c
            else opening.[closing.IndexOf c], -1, c

        let mutable depth = 0
        let mutable j = i + step
        let mutable found = None

        while found.IsNone && j >= 0 && j < text.Length do
            if mask.[j] then
                if text.[j] = self then depth <- depth + 1
                elif text.[j] = target then
                    if depth = 0 then found <- Some j else depth <- depth - 1

            j <- j + step

        found |> Option.map (fun j -> i, j))
