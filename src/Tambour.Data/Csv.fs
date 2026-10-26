/// A table as CSV (RFC 4180), with a unit row below the header: a float column's unit there, or
/// nothing, and the other types marked `#int`, `#string`, `#bool` or `#date`, since no unit holds
/// a `#`. Numbers in the invariant culture, written to round-trip; a missing float an empty cell;
/// an instant in ISO 8601, in UTC.
module Tambour.Data.Csv

open System
open System.Globalization
open System.IO
open System.Text

let private invariant = CultureInfo.InvariantCulture

// ---- Writing ------------------------------------------------------------------------------------

let private quoted (cell: string) =
    "\"" + cell.Replace("\"", "\"\"") + "\""

/// A cell as written: quoted when it holds a separator, a quote or a line break, or begins or ends
/// with a space, which another reader would trim.
let private field (cell: string) =
    if cell.IndexOfAny [| ','; '"'; '\n'; '\r' |] >= 0 || (cell.Length > 0 && (Char.IsWhiteSpace cell.[0] || Char.IsWhiteSpace cell.[cell.Length - 1])) then
        quoted cell
    else
        cell

let private marker (values: Values) =
    match values with
    | Floats _ -> ""
    | v -> "#" + v.Kind

let private cell (values: Values) (row: int) =
    match values with
    | Floats a -> if Double.IsNaN a.[row] then "" else a.[row].ToString("R", invariant)
    | Ints a -> a.[row].ToString invariant
    // An empty string is written quoted, so a row of one empty string is not a blank line.
    | Texts a -> if String.IsNullOrEmpty a.[row] then "\"\"" else field a.[row]
    | Bools a -> if a.[row] then "true" else "false"
    | Dates a -> a.[row].ToString("o", invariant)

let write (table: Table) : string =
    let text = StringBuilder()
    let line (cells: string seq) = text.Append(String.Join(",", cells)).Append('\n') |> ignore
    line (table.Columns |> List.map (fun c -> field c.Name))

    line (
        table.Columns
        |> List.map (fun c ->
            match c.Values with
            | Floats _ -> field (if isNull c.Unit then "" else c.Unit)
            | v -> marker v)
    )

    for row in 0 .. table.Rows - 1 do
        line (table.Columns |> List.map (fun c -> cell c.Values row))

    text.ToString()

// ---- Reading ------------------------------------------------------------------------------------

/// The records of a CSV text, each its cells. A line break ends a record; the break after the
/// last record makes none.
let private records (text: string) : ResizeArray<ResizeArray<string>> =
    let all = ResizeArray()
    let mutable current = ResizeArray<string>()
    let cell = StringBuilder()
    let mutable i = 0
    let mutable quotedCell = false
    let mutable inQuotes = false
    let mutable dirty = false

    let endCell () =
        current.Add(cell.ToString())
        cell.Clear() |> ignore
        quotedCell <- false

    let endRecord () =
        endCell ()
        all.Add current
        current <- ResizeArray()
        dirty <- false

    while i < text.Length do
        let c = text.[i]

        if inQuotes then
            if c = '"' then
                if i + 1 < text.Length && text.[i + 1] = '"' then
                    cell.Append '"' |> ignore
                    i <- i + 1
                else
                    inQuotes <- false
            else
                cell.Append c |> ignore
        else
            match c with
            | '"' when cell.Length = 0 && not quotedCell ->
                inQuotes <- true
                quotedCell <- true
                dirty <- true
            | ',' ->
                endCell ()
                dirty <- true
            | '\r' when i + 1 < text.Length && text.[i + 1] = '\n' -> ()
            | '\n'
            | '\r' -> endRecord ()
            | c ->
                cell.Append c |> ignore
                dirty <- true

        i <- i + 1

    if inQuotes then failwith "the file ends inside a quoted cell"
    if dirty || cell.Length > 0 || current.Count > 0 then endRecord ()
    all

let private parseFloat (s: string) =
    match Double.TryParse(s, NumberStyles.Float, invariant) with
    | true, v -> Some v
    | _ -> None

let private parseInt (s: string) =
    match Int32.TryParse(s, NumberStyles.Integer, invariant) with
    | true, v -> Some v
    | _ -> None

let private parseBool (s: string) =
    match s.ToLowerInvariant() with
    | "true" -> Some true
    | "false" -> Some false
    | _ -> None

/// An instant: with its offset, converted to UTC; without one, taken as UTC. A finer one than a
/// microsecond is not one a table holds, so the column is read as text.
let private parseDate (s: string) =
    match DateTime.TryParse(s, invariant, DateTimeStyles.RoundtripKind) with
    | true, v when v.Ticks % 10L <> 0L -> None
    | true, v when v.Kind = DateTimeKind.Utc -> Some v
    | true, v when v.Kind = DateTimeKind.Local -> Some(v.ToUniversalTime())
    | true, v -> Some(DateTime.SpecifyKind(v, DateTimeKind.Utc))
    | _ -> None

let private all (parse: string -> 'a option) (cells: string[]) =
    let parsed = cells |> Array.map parse
    if parsed |> Array.forall Option.isSome then Some(parsed |> Array.map Option.get) else None

/// A float column's cells: an empty one is a missing value.
let private floats (cells: string[]) =
    all (fun c -> if c = "" then Some nan else parseFloat c) cells

/// A column's values as its marker says, or as its cells show when there is none. A column
/// typed int, bool or date with an empty cell becomes the type that holds one: float for an int,
/// string otherwise.
let private column (name: string) (mark: string option) (cells: string[]) : Result<Column, string> =
    let filled = cells |> Array.filter (fun c -> c <> "")
    let hasEmpty = filled.Length < cells.Length

    let asText () = Ok { Name = name; Unit = null; Values = Texts cells }

    let asFloats (unit: string) =
        match floats cells with
        | Some v -> Ok { Name = name; Unit = unit; Values = Floats v }
        | None -> Error $"column {name} is marked as numbers, and holds a cell that is not one"

    match mark with
    | Some "#string" -> asText ()
    | Some "#int" when hasEmpty -> asFloats null
    | Some "#int" ->
        match all parseInt cells with
        | Some v -> Ok { Name = name; Unit = null; Values = Ints v }
        | None -> asFloats null
    | Some "#bool" ->
        match (if hasEmpty then None else all parseBool cells) with
        | Some v -> Ok { Name = name; Unit = null; Values = Bools v }
        | None -> asText ()
    | Some "#date" ->
        match (if hasEmpty then None else all parseDate cells) with
        | Some v -> Ok { Name = name; Unit = null; Values = Dates v }
        | None -> asText ()
    | Some m when m.StartsWith "#" -> Error $"column {name} is marked {m}, which is no type"
    | Some unit -> asFloats (if unit = "" then null else unit)
    | None ->
        // No unit row: the type the cells show.
        if filled.Length > 0 && not hasEmpty && (all parseInt filled).IsSome then
            Ok { Name = name; Unit = null; Values = Ints(cells |> Array.map (parseInt >> Option.get)) }
        elif (floats cells).IsSome && filled.Length > 0 then
            asFloats null
        elif filled.Length > 0 && not hasEmpty && (all parseBool filled).IsSome then
            Ok { Name = name; Unit = null; Values = Bools(cells |> Array.map (parseBool >> Option.get)) }
        elif filled.Length > 0 && not hasEmpty && (all parseDate filled).IsSome then
            Ok { Name = name; Unit = null; Values = Dates(cells |> Array.map (parseDate >> Option.get)) }
        else
            asText ()

/// Whether the row under the header is a unit row, read as one: each cell empty, a type's marker
/// or something that is not a number, and every column it would make a float column holding only
/// numbers and empty cells below it. Any unit text survives, read by Marey or not, and a first
/// row of text in a file from elsewhere is not taken for units.
let private isUnitRow (rows: ResizeArray<ResizeArray<string>>) (width: int) =
    rows.Count > 1
    && rows.[1].Count = width
    && Seq.forall
        (fun j ->
            let c = rows.[1].[j]

            c.StartsWith "#"
            || ((c = "" || (parseFloat c).IsNone)
                && Seq.forall (fun r -> r >= rows.Count || rows.[r].Count <> width || rows.[r].[j] = "" || (parseFloat rows.[r].[j]).IsSome) (seq { 2 .. rows.Count - 1 })))
        (seq { 0 .. width - 1 })

let read (name: string) (text: string) : Result<Table, string> =
    try
        let rows = records text

        if rows.Count = 0 then
            Error "the file is empty"
        else
            let names = rows.[0].ToArray()
            let width = names.Length
            let marked = isUnitRow rows width
            let first = if marked then 2 else 1

            match rows |> Seq.skip first |> Seq.tryFindIndex (fun r -> r.Count <> width) with
            | Some i -> Error $"row {first + i + 1} has {rows.[first + i].Count} cells, and the header {width}"
            | None ->
                let cells j = [| for r in first .. rows.Count - 1 -> rows.[r].[j] |]

                let columns =
                    [ for j in 0 .. width - 1 ->
                          column names.[j] (if marked then Some rows.[1].[j] else None) (cells j) ]

                match columns |> List.tryPick (function Error e -> Some e | Ok _ -> None) with
                | Some e -> Error e
                | None -> Table.create name (columns |> List.map (function Ok c -> c | Error _ -> failwith "unreachable"))
    with e ->
        Error e.Message

let readFile (path: string) : Result<Table, string> =
    read (Path.GetFileNameWithoutExtension path) (File.ReadAllText path)

let writeFile (path: string) (table: Table) = File.WriteAllText(path, write table)
