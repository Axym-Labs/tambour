/// The prelude Tambour writes beside a project's scripts: a record type and a binding per table.
/// In Tambour (`--define:TAMBOUR`) the binding is the columns the app sent; elsewhere it reads the
/// table's file, so a script runs in plain `dotnet fsi` from the project's files alone.
module Tambour.Data.Prelude

open System
open System.Text
open System.Text.RegularExpressions
open Marey

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

// ---- Witnesses ------------------------------------------------------------------------------------

/// Marey's witness for each SI symbol, by its symbol: what a typed column is plotted under.
let private witnesses =
    dict
        [ "m", "U.metre"; "g", "U.kilogram"; "s", "U.second"; "A", "U.ampere"; "K", "U.kelvin"; "mol", "U.mole"; "cd", "U.candela"
          "Hz", "U.hertz"; "N", "U.newton"; "Pa", "U.pascal"; "J", "U.joule"; "W", "U.watt"; "C", "U.coulomb"; "V", "U.volt"
          "F", "U.farad"; "Ω", "U.ohm"; "S", "U.siemens"; "Wb", "U.weber"; "T", "U.tesla"; "H", "U.henry"; "lm", "U.lumen"
          "lx", "U.lux"; "Bq", "U.becquerel"; "Gy", "U.gray"; "Sv", "U.sievert"; "kat", "U.katal" ]

let private prefixes =
    [ "da"; "Q"; "R"; "Y"; "Z"; "E"; "P"; "T"; "G"; "M"; "k"; "h"; "d"; "c"; "m"; "µ"; "μ"; "u"; "n"; "p"; "f"; "a"; "z"; "y"; "r"; "q" ]

/// A symbol's witness with its prefix taken off, since a typed column holds the coherent SI unit.
let private witnessOf (symbol: string) =
    match witnesses.TryGetValue symbol with
    | true, w -> Some w
    | _ ->
        prefixes
        |> List.tryPick (fun p ->
            if symbol.Length > p.Length && symbol.StartsWith p then
                match witnesses.TryGetValue(symbol.Substring p.Length) with
                | true, w -> Some w
                | _ -> None
            else
                None)

/// The F# expression of Marey's witness a typed column is plotted under, composed as the unit is
/// written (`V/m` is `U.div U.volt U.metre`), in the coherent SI unit; None for a column the
/// script sees as a plain float.
let witness (unitText: string) : string option =
    match Units.typing unitText with
    | Typed _ ->
        match U.tryParse unitText with
        | Ok u ->
            let factors = if List.isEmpty u.Factors then [ u.Symbol, 1 ] else u.Factors

            let power (w: string) (n: int) =
                match n with
                | 1 -> w
                | 2 -> $"(U.square {w})"
                | 3 -> $"(U.cube {w})"
                | n -> List.replicate (n - 1) w |> List.fold (fun acc x -> $"(U.mul {acc} {x})") w

            let parts sign =
                factors
                |> List.filter (fun (_, e) -> sign * e > 0)
                |> List.map (fun (s, e) -> witnessOf s |> Option.map (fun w -> power w (abs e)))

            let product (ws: string option list) =
                if ws |> List.exists Option.isNone then None
                else
                    match ws |> List.choose id with
                    | [] -> Some ""
                    | first :: rest -> Some(rest |> List.fold (fun acc w -> $"(U.mul {acc} {w})") first)

            match product (parts 1), product (parts -1) with
            | Some "", Some "" -> None
            | Some n, Some "" -> Some n
            | Some "", Some d -> Some $"(U.recip {d})"
            | Some n, Some d -> Some $"(U.div {n} {d})"
            | _ -> None
        | Error _ -> None
    | _ -> None
