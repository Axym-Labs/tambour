/// The prelude Tambour writes beside a project's scripts: a record type and a binding per table.
/// In Tambour (`--define:TAMBOUR`) the binding is the columns the app sent; elsewhere it reads the
/// table's file, so a script runs in plain `dotnet fsi` from the project's files alone.
module Tambour.Data.Prelude

open System
open System.Text
open System.Text.RegularExpressions

[<Literal>]
let FileName = "prelude.fsx"

let private keywords =
    set
        [ "abstract"; "and"; "as"; "assert"; "base"; "begin"; "class"; "const"; "default"; "delegate"; "do"; "done"
          "downcast"; "downto"; "elif"; "else"; "end"; "exception"; "extern"; "false"; "finally"; "fixed"; "for"
          "fun"; "function"; "global"; "if"; "in"; "inherit"; "inline"; "interface"; "internal"; "lazy"; "let"
          "match"; "member"; "module"; "mutable"; "namespace"; "new"; "not"; "null"; "of"; "open"; "or"; "override"
          "private"; "public"; "rec"; "return"; "select"; "sig"; "static"; "struct"; "then"; "to"; "true"; "try"
          "type"; "upcast"; "use"; "val"; "void"; "when"; "while"; "with"; "yield"; "break"; "checked"; "component"
          "constraint"; "continue"; "event"; "external"; "include"; "mixin"; "parallel"; "process"; "protected"
          "pure"; "sealed"; "tailcall"; "trait"; "virtual"; "params" ]

/// A name as an F# identifier: as it is where it is one, in double backticks where it is not.
let identifier (name: string) =
    if Regex.IsMatch(name, @"^[A-Za-z_][A-Za-z0-9_']*$") && not (keywords.Contains name) then
        name
    else
        "``" + Regex.Replace(name.Replace("`", "'"), @"[\r\n\t]", " ") + "``"

/// The record type's name for a table: its words run together, capitalised, a plural `s` dropped
/// (`trials` gives `Trial`).
let typeName (table: string) =
    let words = Regex.Split(table, @"[^A-Za-z0-9]+") |> Array.filter (fun w -> w <> "")
    let joined = words |> Array.map (fun w -> string (Char.ToUpperInvariant w.[0]) + w.Substring 1) |> String.concat ""

    let singular =
        if joined.Length > 3 && joined.EndsWith "s" && not (joined.EndsWith "ss") && not (joined.EndsWith "us") then
            joined.Substring(0, joined.Length - 1)
        else
            joined

    if singular = "" || Char.IsDigit singular.[0] then "Row" + singular else singular

/// A string as an F# literal.
let private literal (s: string) = "\"" + s.Replace("\\", "\\\\").Replace("\"", "\\\"") + "\""

let private invariant = Globalization.CultureInfo.InvariantCulture

/// A column's F# type in the record.
let fieldType (c: Column) =
    match c.Values with
    | Floats _ ->
        match Units.typing c.Unit with
        | Typed(measure, _) -> $"float<{measure}>"
        | _ -> "float"
    | Ints _ -> "int"
    | Texts _ -> "string"
    | Bools _ -> "bool"
    | Dates _ -> "DateTime"

/// A table the prelude binds: the table, and the file it is kept in, by name.
type Source = { Table: Table; File: string }

/// The reader of the tables' files, for a script run outside Tambour: CSV as Tambour writes it.
let private csvReader =
    """
/// Reading the tables' files, for a script run outside Tambour.
module private Read =
    let private invariant = Globalization.CultureInfo.InvariantCulture

    /// A CSV file's rows below its header and unit row, each its cells.
    let rows (path: string) : string[][] =
        let text = IO.File.ReadAllText path
        let all = ResizeArray<string[]>()
        let row = ResizeArray<string>()
        let cell = Text.StringBuilder()
        let mutable quoted = false
        let mutable i = 0
        let mutable dirty = false

        let endRow () =
            row.Add(cell.ToString())
            cell.Clear() |> ignore
            all.Add(row.ToArray())
            row.Clear()
            dirty <- false

        while i < text.Length do
            let c = text.[i]

            if quoted then
                if c = '"' then
                    if i + 1 < text.Length && text.[i + 1] = '"' then
                        cell.Append '"' |> ignore
                        i <- i + 1
                    else
                        quoted <- false
                else
                    cell.Append c |> ignore
            else
                match c with
                | '"' -> quoted <- true; dirty <- true
                | ',' -> row.Add(cell.ToString()); cell.Clear() |> ignore; dirty <- true
                | '\r' when i + 1 < text.Length && text.[i + 1] = '\n' -> ()
                | '\n' | '\r' -> endRow ()
                | c -> cell.Append c |> ignore; dirty <- true

            i <- i + 1

        if dirty || cell.Length > 0 || row.Count > 0 then endRow ()
        all.ToArray() |> Array.skip 2

    let float (s: string) = if s = "" then nan else Double.Parse(s, Globalization.NumberStyles.Float, invariant)
    let int (s: string) = Int32.Parse(s, invariant)
    let bool (s: string) = s.ToLowerInvariant() = "true"
    let date (s: string) = DateTime.Parse(s, invariant, Globalization.DateTimeStyles.RoundtripKind).ToUniversalTime()
"""

/// The reader of a table kept as Arrow, for a script run outside Tambour.
let private arrowReader =
    """
/// Reading the tables kept as Arrow, for a script run outside Tambour.
module private ReadArrow =
    open Apache.Arrow

    let batches (path: string) =
        use stream = IO.File.OpenRead path
        use reader = new Ipc.ArrowFileReader(stream)
        let all = ResizeArray<RecordBatch>()
        let mutable b = reader.ReadNextRecordBatch()

        while not (isNull b) do
            all.Add b
            b <- reader.ReadNextRecordBatch()

        all.ToArray()

    let column (batches: RecordBatch[]) (j: int) (get: IArrowArray -> int -> 'a) =
        [| for b in batches do
               let a = b.Column j
               for i in 0 .. a.Length - 1 -> get a i |]

    let float (a: IArrowArray) i = let d = a :?> DoubleArray in if d.IsNull i then nan else d.GetValue(i).Value
    let int (a: IArrowArray) i = (a :?> Int32Array).GetValue(i).Value
    let text (a: IArrowArray) i = let d = a :?> StringArray in if d.IsNull i then "" else d.GetString i
    let bool (a: IArrowArray) i = (a :?> BooleanArray).GetValue(i).Value
    let date (a: IArrowArray) i = (a :?> TimestampArray).GetTimestamp(i).Value.UtcDateTime
"""

/// The prelude for a project's tables.
let generate (sources: Source list) : string =
    let text = StringBuilder()
    let line (s: string) = text.Append(s).Append('\n') |> ignore
    let arrow = sources |> List.exists (fun s -> s.File.EndsWith ".arrow")

    line "// Written by Tambour from the project's tables whenever their columns change; an edit here is"
    line "// lost. A script uses it with #load \"prelude.fsx\" and open Prelude."

    if arrow then
        line "#if !TAMBOUR"
        line "#r \"nuget: Apache.Arrow, 23.0.0\""
        line "#endif"

    line "module Prelude"
    line ""
    line "open System"
    line "open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols"

    for s in sources do
        let t = s.Table
        line ""
        line $"/// One row of the table {t.Name}, from {s.File}."
        line $"type {typeName t.Name} ="

        t.Columns
        |> List.iteri (fun i c ->
            let note =
                match c.Values, Units.typing c.Unit with
                | Floats _, Typed _ -> $"      /// {c.Unit}\n"
                | Floats _, Untyped why -> $"      /// {c.Unit}: a plain float, since {why}\n"
                | _ -> ""

            let opening = if i = 0 then "    { " else "      "
            let closing = if i = t.Columns.Length - 1 then " }" else ""
            text.Append(if i = 0 then note.Replace("      ///", "    ///") else note) |> ignore
            line $"{opening}{identifier c.Name}: {fieldType c}{closing}")

    line ""
    line "#if TAMBOUR"

    for s in sources do
        let t = s.Table
        let name = typeName t.Name
        line ("let mutable " + identifier t.Name + ": " + name + "[] = Tambour.Data.Bound.rows<" + name + "> " + literal t.Name)

    line "#else"
    text.Append(csvReader) |> ignore
    if arrow then text.Append(arrowReader) |> ignore

    for s in sources do
        let t = s.Table
        let path = "IO.Path.Combine(__SOURCE_DIRECTORY__, " + literal s.File + ")"
        line ""

        let value (j: int) (c: Column) =
            let wrap (raw: string) =
                match c.Values, Units.typing c.Unit with
                | Floats _, Typed(measure, factor) when factor = 1.0 -> "LanguagePrimitives.FloatWithMeasure<" + measure + "> (" + raw + ")"
                | Floats _, Typed(measure, factor) ->
                    "LanguagePrimitives.FloatWithMeasure<" + measure + "> (" + raw + " * " + factor.ToString("R", invariant) + ")"
                | _ -> raw

            if s.File.EndsWith ".arrow" then
                let reader =
                    match c.Values with
                    | Floats _ -> "ReadArrow.float"
                    | Ints _ -> "ReadArrow.int"
                    | Texts _ -> "ReadArrow.text"
                    | Bools _ -> "ReadArrow.bool"
                    | Dates _ -> "ReadArrow.date"

                identifier c.Name + " = " + wrap ("columns.[" + string j + "].[i] :?> _"),
                "ReadArrow.column batches " + string j + " (fun a i -> box (" + reader + " a i))"
            else
                let reader =
                    match c.Values with
                    | Floats _ -> "Read.float"
                    | Ints _ -> "Read.int"
                    | Texts _ -> "id"
                    | Bools _ -> "Read.bool"
                    | Dates _ -> "Read.date"

                identifier c.Name + " = " + wrap (reader + " r.[" + string j + "]"), ""

        if s.File.EndsWith ".arrow" then
            line $"let {identifier t.Name}: {typeName t.Name}[] ="
            line $"    let batches = ReadArrow.batches ({path})"
            line "    let columns ="
            line "        [|"
            t.Columns |> List.iteri (fun j c -> line $"            {snd (value j c)}")
            line "        |]"
            line $"    Array.init (if columns.Length = 0 then 0 else columns.[0].Length) (fun i ->"
            line "        {"
            t.Columns |> List.iteri (fun j c -> line $"          {fst (value j c)}")
            line "        })"
        else
            line $"let {identifier t.Name}: {typeName t.Name}[] ="
            line $"    Read.rows ({path})"
            line "    |> Array.map (fun r ->"
            line "        {"
            t.Columns |> List.iteri (fun j c -> line $"          {fst (value j c)}")
            line "        })"

    line "#endif"
    text.ToString()
