namespace Tambour

open System
open System.Globalization
open System.Text
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Input
open Avalonia.Input.Platform
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Media.TextFormatting
open Tambour.Data

/// A cell's text: as the grid shows it, and as typed text is read into a column of a type.
module CellText =

    let private invariant = CultureInfo.InvariantCulture

    /// The text the grid shows for a cell.
    let shown (values: Values) (row: int) =
        match values with
        | Floats a -> if Double.IsNaN a.[row] then "" else a.[row].ToString("G10", invariant)
        | Ints a -> a.[row].ToString invariant
        | Texts a -> a.[row]
        | Bools a -> if a.[row] then "true" else "false"
        | Dates a -> a.[row].ToString("yyyy-MM-dd HH:mm:ss.FFFFFF", invariant)

    /// The text a cell is edited and copied as: every digit a float holds.
    let exact (values: Values) (row: int) =
        match values with
        | Floats a -> if Double.IsNaN a.[row] then "" else a.[row].ToString("R", invariant)
        | Dates a -> a.[row].ToString("o", invariant)
        | v -> shown v row

    /// Typed text as one value of a column's type, or why it is not one.
    let parse (kind: string) (text: string) : Result<Values, string> =
        let t = if isNull text then "" else text.Trim()

        match kind with
        | "float" when t = "" -> Ok(Floats [| nan |])
        | "float" ->
            match Double.TryParse(t, NumberStyles.Float, invariant) with
            | true, v -> Ok(Floats [| v |])
            | _ -> Error $"{t} is not a number"
        | "int" ->
            match Int32.TryParse(t, NumberStyles.Integer, invariant) with
            | true, v -> Ok(Ints [| v |])
            | _ -> Error $"{t} is not a whole number, and an int column has no missing values"
        | "string" -> Ok(Texts [| (if isNull text then "" else text) |])
        | "bool" ->
            match t.ToLowerInvariant() with
            | "true" | "1" | "yes" -> Ok(Bools [| true |])
            | "false" | "0" | "no" -> Ok(Bools [| false |])
            | _ -> Error $"{t} is not true or false"
        | _ ->
            match DateTime.TryParse(t, invariant, DateTimeStyles.RoundtripKind) with
            | true, v ->
                let utc =
                    match v.Kind with
                    | DateTimeKind.Utc -> v
                    | DateTimeKind.Local -> v.ToUniversalTime()
                    | _ -> DateTime.SpecifyKind(v, DateTimeKind.Utc)

                Ok(Dates [| DateTime(utc.Ticks / 10L * 10L, DateTimeKind.Utc) |])
            | _ -> Error $"{t} is not a date and time"

    /// A block of cells as a spreadsheet copies it: rows on lines, cells split by tabs, a cell
    /// holding a tab, a line break or a quote quoted. A row of one empty cell is written `""`, so a
    /// last such row is not taken for the text's end.
    let toTsv (rows: string[][]) =
        let cell (c: string) =
            if c.IndexOfAny [| '\t'; '\n'; '\r'; '"' |] >= 0 then "\"" + c.Replace("\"", "\"\"") + "\"" else c

        let row (r: string[]) =
            if r.Length = 1 && r.[0] = "" then "\"\"" else String.Join("\t", r |> Array.map cell)

        String.Join("\n", rows |> Array.map row)

    /// A block of cells from a spreadsheet's copy.
    let ofTsv (text: string) : string[][] =
        let rows = ResizeArray<string[]>()
        let row = ResizeArray<string>()
        let cell = StringBuilder()
        let mutable quoted = false
        // Whether the text's last line holds anything, if only a quoted empty cell.
        let mutable begun = false
        let mutable i = 0

        while i < text.Length do
            let c = text.[i]

            begun <- c <> '\n' && c <> '\r'

            if quoted then
                if c = '"' && i + 1 < text.Length && text.[i + 1] = '"' then
                    cell.Append '"' |> ignore
                    i <- i + 1
                elif c = '"' then
                    quoted <- false
                else
                    cell.Append c |> ignore
            else
                match c with
                | '"' when cell.Length = 0 -> quoted <- true
                | '\t' ->
                    row.Add(cell.ToString())
                    cell.Clear() |> ignore
                | '\r' -> ()
                | '\n' ->
                    row.Add(cell.ToString())
                    cell.Clear() |> ignore
                    rows.Add(row.ToArray())
                    row.Clear()
                | c -> cell.Append c |> ignore

            i <- i + 1

        if cell.Length > 0 || row.Count > 0 || begun then
            row.Add(cell.ToString())
            rows.Add(row.ToArray())

        rows.ToArray()

/// The grid's commands on a selection, as table edits that hold what they replace, each one entry
/// on the history. The grid's context menu, the Table menu and the grid's keys all take them.
module GridCommand =

    /// Columns `C0`..`C1` and rows `R0`..`R1`, both inclusive.
    type Selection = { C0: int; C1: int; R0: int; R1: int }

    type Kind =
        | Copy
        | Cut
        | Paste
        | CopyRows
        | PasteRows
        | DuplicateRows
        | InsertRowAbove
        | InsertRowBelow
        | DeleteRows
        | FillDown
        | InsertColumn
        | RenameColumn
        | DeleteColumns

    /// Each command's name in the menus and its key while the grid has the focus, in the menus'
    /// order: the cells, the rows, the columns.
    let all: (Kind * string * string) list =
        [ Copy, "_Copy", "Ctrl+C"
          Cut, "Cu_t", "Ctrl+X"
          Paste, "_Paste", "Ctrl+V"
          FillDown, "_Fill down", "Ctrl+D"
          CopyRows, "Copy _rows", "Ctrl+Shift+C"
          PasteRows, "Paste ro_ws", "Ctrl+Shift+V"
          DuplicateRows, "_Duplicate rows", "Ctrl+Shift+D"
          InsertRowAbove, "Insert row _above", "Ctrl+OemPlus"
          InsertRowBelow, "Insert row _below", "Ctrl+Shift+OemPlus"
          DeleteRows, "D_elete rows", "Ctrl+OemMinus"
          InsertColumn, "_Insert column", ""
          RenameColumn, "Re_name column", ""
          DeleteColumns, "Delete co_lumns", "" ]

    /// Where the menus draw a line: before the rows' commands and before the columns'.
    let startsGroup (kind: Kind) = kind = CopyRows || kind = InsertColumn

    let private count (s: Selection) = s.R1 - s.R0 + 1

    /// Values one after another, as one run.
    let private join (kind: string) (runs: Values seq) =
        runs |> Seq.fold (fun (acc: Values) v -> Values.insert acc.Length v acc) (Values.blank kind 0)

    /// The selected cells as a block of exact text.
    let copyCells (t: Table) (s: Selection) =
        CellText.toTsv [| for r in s.R0 .. s.R1 -> [| for c in s.C0 .. s.C1 -> CellText.exact t.Columns.[c].Values r |] |]

    /// The selected rows, every column of them, as a block of exact text.
    let copyRows (t: Table) (s: Selection) =
        CellText.toTsv [| for r in s.R0 .. s.R1 -> [| for c in t.Columns -> CellText.exact c.Values r |] |]

    /// The selected cells emptied: a float's missing, a text's empty, an int's 0.
    let clear (t: Table) (s: Selection) =
        [ for c in s.C0 .. s.C1 -> TableEdit.setCells t c s.R0 (Values.blank t.Columns.[c].Values.Kind (count s)) ]

    /// A column's run read from its cells' texts (a cell there is none of, blank), or why one is
    /// not of its type.
    let private read (column: Column) (cells: string option[]) : Result<Values, string> =
        let kind = column.Values.Kind
        let parsed = cells |> Array.map (function Some text -> CellText.parse kind text | None -> Ok(Values.blank kind 1))

        match parsed |> Array.tryPick (function Error e -> Some e | Ok _ -> None) with
        | Some why -> Error $"{column.Name}: {why}"
        | None -> Ok(parsed |> Array.choose (function Ok v -> Some v | Error _ -> None) |> join kind)

    /// A block pasted at the selection's corner: rows added where it runs past the end, each
    /// column's cells read as its type, and refused whole if one is not.
    let paste (t: Table) (s: Selection) (block: string[][]) : Result<TableEdit list, string> =
        if block.Length = 0 || t.Columns.IsEmpty then
            Ok []
        else
            let width = min (block |> Array.map Array.length |> Array.max) (t.Columns.Length - s.C0)
            let added = max 0 (s.R0 + block.Length - t.Rows)
            let grow = if added > 0 then [ TableEdit.addRows t t.Rows added ] else []
            let t' = (grow |> List.fold TableEdit.apply (Map.ofList [ t.Name, t ])).[t.Name]

            let cells =
                [ for j in 0 .. width - 1 ->
                      read t'.Columns.[s.C0 + j] (block |> Array.map (fun r -> Some(if j < r.Length then r.[j] else "")))
                      |> Result.map (TableEdit.setCells t' (s.C0 + j) s.R0) ]

            match cells |> List.tryPick (function Error e -> Some e | Ok _ -> None) with
            | Some why -> Error why
            | None -> Ok(grow @ (cells |> List.choose (function Ok e -> Some e | Error _ -> None)))

    /// Whole rows pasted, inserted before the selection's first row: each line of the block a row,
    /// its cells in the columns' order.
    let pasteRows (t: Table) (s: Selection) (block: string[][]) : Result<TableEdit list, string> =
        if block.Length = 0 || t.Columns.IsEmpty then
            Ok []
        else
            let runs = t.Columns |> List.mapi (fun j c -> read c (block |> Array.map (fun r -> if j < r.Length then Some r.[j] else None)))

            match runs |> List.tryPick (function Error e -> Some e | Ok _ -> None) with
            | Some why -> Error why
            | None -> Ok [ AddRows(t.Name, min s.R0 t.Rows, runs |> List.choose (function Ok v -> Some v | Error _ -> None)) ]

    /// The selected rows again, below them.
    let duplicateRows (t: Table) (s: Selection) =
        [ AddRows(t.Name, s.R1 + 1, t.Columns |> List.map (fun c -> Values.slice s.R0 (count s) c.Values)) ]

    let insertRowAbove (t: Table) (s: Selection) = [ TableEdit.addRows t (min s.R0 t.Rows) 1 ]
    let insertRowBelow (t: Table) (s: Selection) = [ TableEdit.addRows t (min (s.R1 + 1) t.Rows) 1 ]
    let deleteRows (t: Table) (s: Selection) = [ TableEdit.removeRows t s.R0 (count s) ]

    /// Each selected column's first selected cell copied into the cells below it.
    let fillDown (t: Table) (s: Selection) =
        [ if s.R1 > s.R0 then
              for c in s.C0 .. s.C1 ->
                  let values = t.Columns.[c].Values
                  TableEdit.setCells t c (s.R0 + 1) (Seq.replicate (s.R1 - s.R0) (Values.slice s.R0 1 values) |> join values.Kind) ]

    /// The selected columns removed, right to left so each index holds; none where that would
    /// leave the table no column.
    let deleteColumns (t: Table) (s: Selection) =
        if s.C1 - s.C0 + 1 >= t.Columns.Length then [] else [ for c in s.C1 .. -1 .. s.C0 -> TableEdit.removeColumn t c ]

    /// The edits a command makes, for those that need no clipboard and no form.
    let edits (kind: Kind) (t: Table) (s: Selection) : TableEdit list =
        match kind with
        | Cut -> clear t s
        | DuplicateRows -> duplicateRows t s
        | InsertRowAbove -> insertRowAbove t s
        | InsertRowBelow -> insertRowBelow t s
        | DeleteRows -> deleteRows t s
        | FillDown -> fillDown t s
        | DeleteColumns -> deleteColumns t s
        | Copy
        | Paste
        | CopyRows
        | PasteRows
        | InsertColumn
        | RenameColumn -> []

/// A cell of the grid, by column and row.
[<Struct>]
type CellRef = { Column: int; Row: int }

/// What a point of the grid is over.
type private Hit =
    | OnCell of CellRef
    | OnColumnHeader of int
    | OnRowHeader of int

/// The table grid: a table drawn from its column arrays, only the cells in view, so a million rows
/// scroll as ten do. Cells are selected with the pointer and the keys, edited in place (typed into,
/// or Enter or F2), cleared with Delete, copied and pasted as a spreadsheet does. Every change is
/// reported as table edits, holding what they replace; the grid changes its table only when it is
/// given a new one.
type TableGrid() as this =
    inherit Control()

    static let rowHeight = 22.0
    static let headerHeight = 40.0
    static let columnWidth = 120.0

    let edited = Event<TableEdit list>()
    let selectionChanged = Event<int list>()
    /// Rows marked from elsewhere, as picked in a figure.
    let mutable marked: Set<int> = Set.empty
    let refused = Event<string>()
    let header = Event<int>()
    let insertColumn = Event<int>()

    let mutable table: Table option = None
    let mutable top = 0
    let mutable left = 0
    let mutable anchor = { Column = 0; Row = 0 }
    let mutable focus = { Column = 0; Row = 0 }
    let mutable editing: CellRef option = None

    let vertical = ScrollBar(Orientation = Orientation.Vertical, Minimum = 0.0, SmallChange = 1.0, LargeChange = 20.0)
    let horizontal = ScrollBar(Orientation = Orientation.Horizontal, Minimum = 0.0, SmallChange = 1.0, LargeChange = 1.0)
    let editor = TextBox(IsVisible = false, BorderThickness = Thickness 1.0, Padding = Thickness(3.0, 1.0), MinHeight = 0.0)

    // The UI's font, Inter, where the app carries it.
    let font = FontFamily "fonts:Inter#Inter, $Default"
    let typeface = Typeface font
    let bold = Typeface(font, FontStyle.Normal, FontWeight.SemiBold)

    let text (s: string) (face: Typeface) (size: float) (brush: IBrush) =
        FormattedText(s, CultureInfo.InvariantCulture, FlowDirection.LeftToRight, face, size, brush)

    /// A column's type as its header shows it, read once per unit text rather than every frame.
    let kinds = Collections.Generic.Dictionary<string, string>()

    let kindOf (column: Column) =
        match column.Values with
        | Floats _ ->
            let unit = if isNull column.Unit then "" else column.Unit

            match kinds.TryGetValue unit with
            | true, k -> k
            | _ ->
                let k =
                    match Units.typing unit with
                    | NoUnit -> "float"
                    | Typed _ -> unit
                    | Untyped _ -> unit + " (untyped)"

                kinds.[unit] <- k
                k
        | v -> v.Kind

    let mutable lastRender = 0.0

    /// The glyphs a number is written with, each looked up and measured once, so a number is drawn
    /// as a run of them and never shaped. A cell holding anything else is shaped, once per text.
    let numerals (size: float) =
        lazy
            (let glyphs = typeface.GlyphTypeface

             dict
                 [ for c in "0123456789.-+E" ->
                       let advance = (FormattedText(string c, CultureInfo.InvariantCulture, FlowDirection.LeftToRight, typeface, size, Brushes.Black)).WidthIncludingTrailingWhitespace
                       c, (glyphs.CharacterToGlyphMap.[int c], advance) ])

    let cellNumerals = numerals 12.5
    let rowNumerals = numerals 11.0
    let shaped = Collections.Generic.Dictionary<string, FormattedText>()

    /// A cell's text at a size, at its left edge and the top of its line: as a run of numerals where
    /// it is a number, as text shaped once otherwise. Its width comes back, to align by.
    let cellText (s: string) (size: float) (glyphs: Lazy<Collections.Generic.IDictionary<char, uint16 * float>>) (brush: IBrush) =
        let map = glyphs.Value

        if s.Length > 0 && s |> Seq.forall map.ContainsKey then
            let infos = s |> Seq.mapi (fun i c -> let g, a = map.[c] in GlyphInfo(g, i, a)) |> Array.ofSeq
            let width = infos |> Array.sumBy (fun g -> g.GlyphAdvance)
            let metrics = typeface.GlyphTypeface.Metrics
            let ascent = -float metrics.Ascent * size / float metrics.DesignEmHeight

            let draw (dc: DrawingContext) (at: Point) =
                use run = new GlyphRun(typeface.GlyphTypeface, size, s.AsMemory(), infos, Nullable(Point(at.X, at.Y + ascent)))
                dc.DrawGlyphRun(brush, run)

            width, draw
        else
            let key = $"{size}|{s}"

            let formatted =
                match shaped.TryGetValue key with
                | true, f -> f
                | _ ->
                    if shaped.Count > 20_000 then shaped.Clear()
                    let f = FormattedText(s, CultureInfo.InvariantCulture, FlowDirection.LeftToRight, typeface, size, brush)
                    shaped.[key] <- f
                    f

            formatted.Width, (fun (dc: DrawingContext) (at: Point) -> dc.DrawText(formatted, at))

    let rows () = match table with Some t -> t.Rows | None -> 0
    let columns () = match table with Some t -> t.Columns.Length | None -> 0

    /// The width of the row numbers, by the digits of the last.
    let numberWidth () = 14.0 + 8.0 * float (string (max 1 (rows ()))).Length

    let visibleRows () =
        max 1 (int ((this.Bounds.Height - headerHeight - horizontal.Bounds.Height) / rowHeight))

    let visibleColumns () =
        max 1 (int ((this.Bounds.Width - numberWidth () - vertical.Bounds.Width) / columnWidth))

    let clampScroll () =
        top <- max 0 (min top (rows () - visibleRows ()))
        left <- max 0 (min left (columns () - visibleColumns ()))
        vertical.Maximum <- float (max 0 (rows () - visibleRows ()))
        vertical.ViewportSize <- float (visibleRows ())
        vertical.Value <- float top
        horizontal.Maximum <- float (max 0 (columns () - 1))
        horizontal.ViewportSize <- float (visibleColumns ())
        horizontal.Value <- float left

    /// Where a cell is drawn, when it is in view.
    let cellRect (c: CellRef) =
        let x = numberWidth () + float (c.Column - left) * columnWidth
        let y = headerHeight + float (c.Row - top) * rowHeight
        Rect(x, y, columnWidth, rowHeight)

    let bringIntoView (c: CellRef) =
        // Not before the grid has a size: what fits is not known, and a hidden tab's grid has none.
        if this.Bounds.Width > 0.0 && this.Bounds.Height > 0.0 then
            if c.Row < top then top <- c.Row
            elif c.Row >= top + visibleRows () then top <- c.Row - visibleRows () + 1

            if c.Column < left then left <- c.Column
            elif c.Column >= left + visibleColumns () then left <- c.Column - visibleColumns () + 1

            clampScroll ()

        this.InvalidateVisual()

    let selectionBounds () =
        min anchor.Column focus.Column, max anchor.Column focus.Column, min anchor.Row focus.Row, max anchor.Row focus.Row

    let moveTo (c: CellRef) (extend: bool) =
        let c = { Column = max 0 (min (columns () - 1) c.Column); Row = max 0 (min (rows () - 1) c.Row) }
        focus <- c
        if not extend then anchor <- c
        bringIntoView c
        let c0, c1 = min anchor.Column focus.Column, max anchor.Column focus.Column
        selectionChanged.Trigger [ c0..c1 ]

    /// The cell under a point, or the column or row whose header it is over.
    let hit (p: Point) =
        let column = left + int (floor ((p.X - numberWidth ()) / columnWidth))
        let row = top + int (floor ((p.Y - headerHeight) / rowHeight))

        if p.X < numberWidth () then
            if p.Y >= headerHeight && row < rows () then Some(OnRowHeader row) else None
        elif column >= columns () then None
        elif p.Y < headerHeight then Some(OnColumnHeader column)
        elif row < rows () then Some(OnCell { Column = column; Row = row })
        else None

    let endEdit (commit: bool) =
        match editing, table with
        | Some c, Some t when commit ->
            let column = t.Columns.[c.Column]

            match CellText.parse column.Values.Kind editor.Text with
            | Ok value ->
                editing <- None
                editor.IsVisible <- false
                if not (value.Equals(Values.slice c.Row 1 column.Values)) then
                    edited.Trigger [ TableEdit.setCells t c.Column c.Row value ]
                this.Focus() |> ignore
                moveTo { c with Row = c.Row + 1 } false
            | Error why ->
                editor.BorderBrush <- SolidColorBrush (Palette.forVariant this.ActualThemeVariant).Error
                ToolTip.SetTip(editor, why)
                refused.Trigger why
        | _ ->
            editing <- None
            editor.IsVisible <- false
            this.Focus() |> ignore

        this.InvalidateArrange()
        this.InvalidateVisual()

    let beginEdit (initial: string option) =
        match table with
        | Some t when columns () > 0 && rows () > 0 ->
            bringIntoView focus
            editing <- Some focus
            editor.Text <- (match initial with Some s -> s | None -> CellText.exact t.Columns.[focus.Column].Values focus.Row)
            editor.BorderBrush <- SolidColorBrush (Palette.forVariant this.ActualThemeVariant).Accent
            ToolTip.SetTip(editor, null)
            editor.IsVisible <- true
            this.InvalidateArrange()
            editor.Focus() |> ignore
            editor.CaretIndex <- editor.Text.Length
        | _ -> ()

    let selection () : GridCommand.Selection =
        let c0, c1, r0, r1 = selectionBounds ()
        { C0 = c0; C1 = c1; R0 = r0; R1 = r1 }

    /// The selection's cells as a block of exact text.
    let copied () =
        match table with
        | Some t when columns () > 0 && rows () > 0 -> GridCommand.copyCells t (selection ())
        | _ -> ""

    let report (result: Result<TableEdit list, string>) =
        match result with
        | Ok [] -> ()
        | Ok edits -> edited.Trigger edits
        | Error why -> refused.Trigger why

    /// A block pasted at the selection's corner.
    let paste (block: string[][]) =
        match table with
        | Some t when columns () > 0 -> report (GridCommand.paste t (selection ()) block)
        | _ -> ()

    let clear () =
        match table with
        | Some t when columns () > 0 && rows () > 0 -> report (Ok(GridCommand.clear t (selection ())))
        | _ -> ()

    let clipboard () = TopLevel.GetTopLevel(this) |> Option.ofObj |> Option.map (fun top -> top.Clipboard)

    /// The clipboard's text, read and handed on on the UI thread.
    let withPasted (f: string[][] -> unit) =
        match clipboard () with
        | Some board ->
            board.TryGetTextAsync().ContinueWith(fun (t: Threading.Tasks.Task<string>) ->
                if t.IsCompletedSuccessfully && not (isNull t.Result) then
                    Threading.Dispatcher.UIThread.Post(fun () -> f (CellText.ofTsv t.Result)))
            |> ignore
        | None -> ()

    /// Takes a command on the selection.
    let run (kind: GridCommand.Kind) =
        match table with
        | Some t when columns () > 0 ->
            let s = selection ()
            let any = rows () > 0

            match kind with
            | GridCommand.Copy when any -> clipboard () |> Option.iter (fun b -> b.SetTextAsync(copied ()) |> ignore)
            | GridCommand.Cut when any ->
                clipboard () |> Option.iter (fun b -> b.SetTextAsync(copied ()) |> ignore)
                clear ()
            | GridCommand.CopyRows when any -> clipboard () |> Option.iter (fun b -> b.SetTextAsync(GridCommand.copyRows t s) |> ignore)
            | GridCommand.Paste -> withPasted paste
            | GridCommand.PasteRows -> withPasted (fun block -> report (GridCommand.pasteRows t (selection ()) block))
            | GridCommand.InsertColumn -> insertColumn.Trigger s.C0
            | GridCommand.RenameColumn -> header.Trigger s.C0
            | GridCommand.InsertRowAbove
            | GridCommand.InsertRowBelow -> report (Ok(GridCommand.edits kind t s))
            | _ when any -> report (Ok(GridCommand.edits kind t s))
            | _ -> ()
        | _ -> ()


    do
        this.Focusable <- true
        this.ClipToBounds <- true

        // The shaped texts carry their colour.
        this.ActualThemeVariantChanged.Add(fun _ ->
            shaped.Clear()
            this.InvalidateVisual())

        for child in [ vertical :> Control; horizontal; editor ] do
            this.VisualChildren.Add child
            this.LogicalChildren.Add child

        vertical.Scroll.Add(fun e ->
            top <- int (Math.Round e.NewValue)
            clampScroll ()
            this.InvalidateVisual())

        horizontal.Scroll.Add(fun e ->
            left <- int (Math.Round e.NewValue)
            clampScroll ()
            this.InvalidateVisual())

        editor.KeyDown.Add(fun e ->
            match e.Key with
            | Key.Enter ->
                e.Handled <- true
                endEdit true
            | Key.Escape ->
                e.Handled <- true
                endEdit false
            | _ -> ())

        editor.LostFocus.Add(fun _ -> if editing.IsSome then endEdit true)

        // The commands on cells, rows and columns, as the Table menu has them.
        let items =
            [ for kind, name, keys in GridCommand.all do
                  if GridCommand.startsGroup kind then Separator() :> Control
                  let item = MenuItem(Header = name)
                  if keys <> "" then item.InputGesture <- KeyGesture.Parse keys
                  item.Click.Add(fun _ -> run kind)
                  item ]

        this.ContextMenu <- ContextMenu(ItemsSource = items)

    /// Changes the grid makes to its table, as edits that hold what they replace.
    [<CLIEvent>]
    member _.Edited = edited.Publish

    /// An edit the grid would not make, and why.
    [<CLIEvent>]
    member _.Refused = refused.Publish

    /// The columns the selection covers, left to right, after each change to it.
    [<CLIEvent>]
    member _.SelectionChanged = selectionChanged.Publish

    /// Rows marked from elsewhere, drawn highlighted; the first is brought into view.
    member _.Marked
        with get () = marked
        and set (rows: Set<int>) =
            if rows <> marked then
                marked <- rows

                if not rows.IsEmpty then
                    let first = Set.minElement rows
                    if first < top || first >= top + visibleRows () then top <- max 0 (first - 2)
                    clampScroll ()

                this.InvalidateVisual()

    /// A column's header double-clicked, or Rename column taken, to edit the column.
    [<CLIEvent>]
    member _.HeaderActivated = header.Publish

    /// Insert column taken: where the new column goes.
    [<CLIEvent>]
    member _.InsertColumnAt = insertColumn.Publish

    /// Takes a command on the selection, as its menu entry or its key does.
    member _.Run(kind: GridCommand.Kind) = run kind

    /// The table shown.
    member _.Table
        with get () = table
        and set (value: Table option) =
            if not (obj.ReferenceEquals(value, table)) then
                table <- value
                if focus.Row >= rows () || focus.Column >= columns () then
                    anchor <- { Column = 0; Row = 0 }
                    focus <- anchor
                clampScroll ()
                this.InvalidateVisual()

    /// The first row in view.
    member _.Top
        with get () = top
        and set (value: int) =
            top <- value
            clampScroll ()
            this.InvalidateVisual()

    /// The selected cells: the corner the selection began at, and the one it reaches.
    member _.Selection = anchor, focus

    member _.Select(a: CellRef, b: CellRef) =
        anchor <- a
        moveTo b true

    /// The selection as a spreadsheet would copy it.
    member _.Copied = copied ()

    /// Pastes a block of text at the selection.
    member _.Paste(text: string) = paste (CellText.ofTsv text)

    /// Types into the selected cell, as keys would.
    member _.TypeInto(text: string) =
        beginEdit (Some text)
        endEdit true

    override _.MeasureOverride(available: Size) =
        for c in [ vertical :> Control; horizontal; editor ] do
            c.Measure available

        Size(0.0, 0.0)

    override _.ArrangeOverride(size: Size) =
        let bar = 14.0
        vertical.Arrange(Rect(size.Width - bar, headerHeight, bar, max 0.0 (size.Height - headerHeight - bar)))
        horizontal.Arrange(Rect(numberWidth (), size.Height - bar, max 0.0 (size.Width - numberWidth () - bar), bar))

        match editing with
        | Some c -> editor.Arrange(cellRect c)
        | None -> editor.Arrange(Rect())

        clampScroll ()
        size

    /// How long the last `Render` took, in milliseconds: recording the frame, not rasterising it.
    member _.LastRender = lastRender

    override _.Render(dc: DrawingContext) =
        let clock = Diagnostics.Stopwatch.StartNew()
        let bounds = Rect(this.Bounds.Size)
        let look = Palette.forVariant this.ActualThemeVariant
        let ink = SolidColorBrush look.Foreground
        let dim = SolidColorBrush look.Dim
        dc.FillRectangle(SolidColorBrush look.Background, bounds)

        match table with
        | None -> ()
        | Some t ->
            let line = Pen(SolidColorBrush look.Line, 1.0)
            let headerBrush = SolidColorBrush look.Raised
            let numbers = numberWidth ()
            let shownRows = min (visibleRows () + 1) (t.Rows - top)
            let shownColumns = min (visibleColumns () + 1) (t.Columns.Length - left)

            dc.FillRectangle(headerBrush, Rect(0.0, 0.0, bounds.Width, headerHeight))
            dc.FillRectangle(headerBrush, Rect(0.0, 0.0, numbers, bounds.Height))

            // Marked rows, then the selection, under the text.
            for i in 0 .. shownRows - 1 do
                if marked.Contains(top + i) then
                    dc.FillRectangle(SolidColorBrush look.Marked, Rect(0.0, headerHeight + float i * rowHeight, bounds.Width, rowHeight))

            let c0, c1, r0, r1 = selectionBounds ()
            let a = cellRect { Column = max c0 left; Row = max r0 top }
            let b = cellRect { Column = c1; Row = r1 }

            if c1 >= left && r1 >= top then
                dc.FillRectangle(SolidColorBrush look.Selected, Rect(a.TopLeft, b.BottomRight))
                dc.DrawRectangle(Pen(SolidColorBrush look.Accent, 1.5), cellRect focus)

            for j in 0 .. shownColumns - 1 do
                let column = t.Columns.[left + j]
                let x = numbers + float j * columnWidth
                dc.DrawText(text column.Name bold 12.5 (SolidColorBrush look.Text), Point(x + 6.0, 4.0))

                dc.DrawText(text (kindOf column) typeface 11.0 dim, Point(x + 6.0, 21.0))

                // The column's cells in view, clipped to it.
                use _ = dc.PushClip(Rect(x + 2.0, headerHeight, columnWidth - 4.0, bounds.Height - headerHeight))

                let numeric =
                    match column.Values with
                    | Floats _
                    | Ints _ -> true
                    | _ -> false

                for i in 0 .. shownRows - 1 do
                    let shown = (CellText.shown column.Values (top + i)).Replace('\n', '⏎').Replace('\r', '⏎')

                    if shown <> "" then
                        let width, draw = cellText shown 12.5 cellNumerals ink
                        let y = headerHeight + float i * rowHeight + 3.0
                        draw dc (Point((if numeric then x + columnWidth - 5.0 - width else x + 5.0), y))

                dc.DrawLine(line, Point(x + columnWidth, 0.0), Point(x + columnWidth, bounds.Height))

            for i in 0 .. shownRows - 1 do
                let y = headerHeight + float i * rowHeight
                let width, draw = cellText (string (top + i + 1)) 11.0 rowNumerals dim
                draw dc (Point(numbers - 6.0 - width, y + 4.0))
                dc.DrawLine(line, Point(0.0, y + rowHeight), Point(bounds.Width, y + rowHeight))

            dc.DrawLine(line, Point(0.0, headerHeight), Point(bounds.Width, headerHeight))
            dc.DrawLine(line, Point(numbers, 0.0), Point(numbers, bounds.Height))

        lastRender <- clock.Elapsed.TotalMilliseconds

    override _.OnPointerPressed(e: PointerPressedEventArgs) =
        base.OnPointerPressed e
        this.Focus() |> ignore
        let extend = e.KeyModifiers.HasFlag KeyModifiers.Shift || e.KeyModifiers.HasFlag KeyModifiers.Control
        let c0, c1, r0, r1 = selectionBounds ()

        // A right click on what is selected keeps the selection for the menu it opens; elsewhere it
        // selects what it is on first.
        let keep =
            e.GetCurrentPoint(this).Properties.IsRightButtonPressed
            && (match hit (e.GetPosition this) with
                | Some(OnCell c) -> c.Column >= c0 && c.Column <= c1 && c.Row >= r0 && c.Row <= r1
                | Some(OnRowHeader r) -> r >= r0 && r <= r1 && c0 = 0 && c1 = columns () - 1
                | Some(OnColumnHeader c) -> c >= c0 && c <= c1 && r0 = 0 && r1 = rows () - 1
                | None -> true)

        if not keep then
            match hit (e.GetPosition this) with
            | Some(OnCell c) ->
                if editing.IsSome then endEdit true
                moveTo c (e.KeyModifiers.HasFlag KeyModifiers.Shift)
                if e.ClickCount = 2 then beginEdit None
            | Some(OnColumnHeader column) when e.ClickCount = 2 -> header.Trigger column
            | Some(OnColumnHeader column) ->
                if not extend then anchor <- { Column = column; Row = 0 }
                anchor <- { anchor with Row = 0 }
                moveTo { Column = column; Row = rows () - 1 } true
            | Some(OnRowHeader row) ->
                // A whole row; Shift or Ctrl extends the rows from the one the selection began at.
                if not extend then anchor <- { Column = 0; Row = row }
                anchor <- { anchor with Column = 0 }
                moveTo { Column = columns () - 1; Row = row } true
            | None -> ()

    override _.OnPointerWheelChanged(e: PointerWheelEventArgs) =
        base.OnPointerWheelChanged e
        top <- top - int (e.Delta.Y * 3.0)
        left <- left - int e.Delta.X
        clampScroll ()
        this.InvalidateVisual()
        e.Handled <- true

    override _.OnKeyDown(e: KeyEventArgs) =
        base.OnKeyDown e
        let shift = e.KeyModifiers.HasFlag KeyModifiers.Shift
        let control = e.KeyModifiers.HasFlag KeyModifiers.Control

        if editing.IsNone then
            match e.Key with
            | Key.Up -> moveTo { focus with Row = focus.Row - 1 } shift; e.Handled <- true
            | Key.Down -> moveTo { focus with Row = focus.Row + 1 } shift; e.Handled <- true
            | Key.Left -> moveTo { focus with Column = focus.Column - 1 } shift; e.Handled <- true
            | Key.Right
            | Key.Tab -> moveTo { focus with Column = focus.Column + 1 } shift; e.Handled <- true
            | Key.PageUp -> moveTo { focus with Row = focus.Row - visibleRows () } shift; e.Handled <- true
            | Key.PageDown -> moveTo { focus with Row = focus.Row + visibleRows () } shift; e.Handled <- true
            | Key.Home when control -> moveTo { Column = focus.Column; Row = 0 } shift; e.Handled <- true
            | Key.End when control -> moveTo { Column = focus.Column; Row = rows () - 1 } shift; e.Handled <- true
            | Key.Enter
            | Key.F2 -> beginEdit None; e.Handled <- true
            | Key.Delete -> clear (); e.Handled <- true
            | _ when control ->
                match GridCommand.all |> List.tryFind (fun (_, _, keys) -> keys <> "" && (let g = KeyGesture.Parse keys in g.Key = e.Key && g.KeyModifiers = e.KeyModifiers)) with
                | Some(kind, _, _) ->
                    run kind
                    e.Handled <- true
                | None -> ()
            | _ -> ()

    override _.OnTextInput(e: TextInputEventArgs) =
        base.OnTextInput e

        if editing.IsNone && not (String.IsNullOrEmpty e.Text) && not (Char.IsControl e.Text.[0]) then
            beginEdit (Some e.Text)
            e.Handled <- true
