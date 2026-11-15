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

/// Whether a script loads the prelude itself, with a `#load "prelude.fsx"` line of its own, as a
/// script written before the prelude was loaded for it does.
let loaded (script: string) =
    not (isNull script)
    && Regex.IsMatch(script, @"^[ \t]*#load[ \t]+""prelude\.fsx""", RegexOptions.Multiline)

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

/// A number as F# reads it back exactly.
let private roundTrip (x: float) =
    let text = x.ToString("R", invariant)
    if text |> Seq.exists (fun c -> c = '.' || c = 'E' || c = 'e' || Char.IsLetter c) then text else text + ".0"

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

/// Whether a column reaches a script as uncertain values: a float column with a declaration.
let isUncertain (c: Column) =
    match c.Values, c.Uncertainty with
    | Floats _, Some _ -> true
    | _ -> false

/// The columns a table's records have: each but a companion `u(name)` whose column's
/// declaration reads it, since that column's values carry it.
let fieldsOf (t: Table) =
    let read =
        t.Columns |> List.filter (fun c -> isUncertain c && Uncertainty.usesCompanion c) |> List.map (fun c -> Uncertainty.companion c.Name) |> Set.ofList

    t.Columns |> List.filter (fun c -> not (read.Contains c.Name))

/// Whether a table's rows are read raw and then measured: it has an uncertain column.
let isMeasured (t: Table) = t.Columns |> List.exists isUncertain

/// The measure of a column's values as a script sees them: `m`, or `1` for a plain float.
let private measureOf (c: Column) =
    match Units.typing c.Unit with
    | Typed(measure, _) -> measure
    | _ -> "1"

/// A column's F# type in the record: uncertain values where it has a declaration.
let recordType (c: Column) =
    if isUncertain c then $"Uncertain<{measureOf c}>" else fieldType c

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

// ---- Witnesses ------------------------------------------------------------------------------------

/// Marey's witness for each SI symbol, by its symbol: what a typed column is plotted under.
let private witnesses =
    dict
        [ "m", "U.metre"; "g", "U.kilogram"; "s", "U.second"; "A", "U.ampere"; "K", "U.kelvin"; "mol", "U.mole"; "cd", "U.candela"
          "Hz", "U.hertz"; "N", "U.newton"; "Pa", "U.pascal"; "J", "U.joule"; "W", "U.watt"; "C", "U.coulomb"; "V", "U.volt"
          "F", "U.farad"; "Ω", "U.ohm"; "S", "U.siemens"; "Wb", "U.weber"; "T", "U.tesla"; "H", "U.henry"; "lm", "U.lumen"
          "lx", "U.lux"; "Bq", "U.becquerel"; "Gy", "U.gray"; "Sv", "U.sievert"; "kat", "U.katal"
          "L", "(U.cube U.metre)" ]

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

/// The prelude for a project's tables.
let generate (sources: Source list) : string =
    let text = StringBuilder()
    let line (s: string) = text.Append(s).Append('\n') |> ignore
    let arrow = sources |> List.exists (fun s -> s.File.EndsWith ".arrow")

    line "// Written by Tambour from the project's tables whenever their columns change; an edit here is"
    line "// lost. Tambour loads and opens it for every script beside it; a script run elsewhere says"
    line "// #load \"prelude.fsx\" and open Prelude."

    if arrow then
        line "#if !TAMBOUR"
        line "#r \"nuget: Apache.Arrow, 23.0.0\""
        line "#endif"

    line "module Prelude"
    line ""
    line "open System"
    line "open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols"

    let measured = sources |> List.filter (fun s -> isMeasured s.Table)

    if not measured.IsEmpty then
        line "open Marey"

    /// A record type, a field per column, each with its unit and its uncertainty noted.
    let record (indent: string) (doc: string) (name: string) (columns: Column list) (typeOf: Column -> string) =
        line ""
        line $"{indent}/// {doc}"
        line $"{indent}type {name} ="

        columns
        |> List.iteri (fun i c ->
            let unit =
                match c.Values, Units.typing c.Unit with
                | Floats _, Typed _ -> Some c.Unit
                | Floats _, Untyped why -> Some $"{c.Unit}: a plain float, since {why}"
                | _ -> None

            let uncertainty =
                match c.Uncertainty with
                | Some u when typeOf c <> fieldType c ->
                    let part (kind: string) (p: Part) =
                        let value = if Uncertainty.fromColumn p then $"each row's in {Uncertainty.companion c.Name}" else p.Value.ToString("R", invariant)
                        let source = if String.IsNullOrEmpty p.Source then "" else $" ({p.Source})"
                        $"{kind}: {Uncertainty.describe p.Spread}, {value}{source}"

                    [ u.PerReading |> Option.map (part "each reading's own")
                      u.Shared |> Option.map (part "shared by every reading") ]
                    |> List.choose id
                    |> function
                        | [] -> None
                        | parts -> Some(String.Join("; ", parts))
                | _ -> None

            let note = [ unit; uncertainty ] |> List.choose id

            let opening = if i = 0 then $"{indent}    {{ " else $"{indent}      "
            let closing = if i = columns.Length - 1 then " }" else ""

            if not note.IsEmpty then
                line $"""{(if i = 0 then indent + "    " else indent + "      ")}/// {String.Join(". ", note)}"""

            line $"{opening}{identifier c.Name}: {typeOf c}{closing}")

    if not measured.IsEmpty then
        line ""
        line "/// The rows of the tables with uncertain columns as their files hold them, before the"
        line "/// uncertainties are put on."
        line "module Raw ="

        for s in measured do
            record "    " $"One row of the table {s.Table.Name} as {s.File} holds it." (typeName s.Table.Name) s.Table.Columns fieldType

    for s in sources do
        let t = s.Table
        record "" $"One row of the table {t.Name}, from {s.File}." (typeName t.Name) (fieldsOf t) recordType

    if not measured.IsEmpty then
        line ""
        line "/// Each table's rows with their columns' uncertainties put on: each reading its own input,"
        line "/// and a shared part one input for the whole column, so it cancels in a difference."
        line "module Measured ="
        line "    /// A reading with its own uncertainty, as stated; exact where the row states none."
        line "    let private reading (make: string -> U<'u> -> float<'u> -> float<'u> -> Uncertain<'u>) (label: string) (unit: U<'u>) (value: float<'u>) (u: float<'u>) ="
        line "        if Double.IsNaN(float u) || Double.IsNaN(float value) then Uncertain.exact unit value else make label unit value u"

        for s in measured do
            let t = s.Table
            let name = typeName t.Name
            line ""
            line $"    let {identifier t.Name} (rows: Raw.{name}[]) : {name}[] ="

            let witnessOf (c: Column) = witness c.Unit |> Option.defaultValue "U.one"
            let scaled (c: Column) (v: float) =
                match Units.typing c.Unit with
                | Typed(measure, factor) -> $"{roundTrip (v * factor)}<{measure}>"
                | _ -> roundTrip v

            let make (c: Column) (p: Part) =
                let dof =
                    match c.Uncertainty with
                    | Some u when not (Double.IsPositiveInfinity u.Dof) -> $" |> Uncertain.withDof {roundTrip u.Dof}"
                    | _ -> ""

                match p.Spread with
                | Spread.Standard when dof = "" -> "Uncertain.standard"
                | Spread.Standard -> $"(fun l w v u -> Uncertain.standard l w v u{dof})"
                | Spread.Rectangular -> "Uncertain.rectangular"
                | Spread.Triangular -> "Uncertain.triangular"
                | Spread.Resolution -> "Uncertain.resolution"
                | Spread.Expanded k -> $"(fun l w v u -> Uncertain.certificate l w v u {roundTrip k})"

            let label (c: Column) (p: Part) (shared: bool) =
                let source = if String.IsNullOrEmpty p.Source then "" else ": " + p.Source
                if shared then literal (c.Name + (if source = "" then ": shared" else source))
                else literal (c.Name + ", row ") + " + string (i + 1)" + (if source = "" then "" else " + " + literal source)

            // A shared part is made once, with an estimate of zero, and added to every reading.
            for c in t.Columns do
                match c.Uncertainty with
                | Some { Shared = Some p } when isUncertain c ->
                    let zero = match Units.typing c.Unit with Typed(m, _) -> $"0.0<{m}>" | _ -> "0.0"
                    line $"        let ``{c.Name} shared`` = {make c p} {label c p true} {witnessOf c} {zero} ({scaled c p.Value})"
                | _ -> ()

            line "        rows"
            line "        |> Array.mapi (fun i r ->"
            line "            {"

            for c in fieldsOf t do
                let field = $"r.{identifier c.Name}"

                let value =
                    match c.Uncertainty with
                    | Some u when isUncertain c ->
                        let own =
                            match u.PerReading with
                            | Some p ->
                                let stated = if Uncertainty.fromColumn p then $"r.{identifier (Uncertainty.companion c.Name)}" else $"({scaled c p.Value})"
                                $"reading {make c p} ({label c p false}) {witnessOf c} {field} {stated}"
                            | None -> $"Uncertain.exact {witnessOf c} {field}"

                        match u.Shared with
                        | Some _ -> $"{own} + ``{c.Name} shared``"
                        | None -> own
                    | _ -> field

                line $"              {identifier c.Name} = {value}"

            line "            })"

    line ""
    line "#if TAMBOUR"

    for s in sources do
        let t = s.Table
        let name = typeName t.Name
        if isMeasured t then
            line ("let mutable " + identifier t.Name + ": " + name + "[] = Tambour.Data.Bound.rows<Raw." + name + "> " + literal t.Name + " |> Measured." + identifier t.Name)
        else
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

        // A measured table's fields are the raw record's, each named by its type.
        let measured = isMeasured t
        let fieldLine (_: int) (text: string) = if measured then $"Raw.{typeName t.Name}.{text}" else text
        let measure () = if measured then line $"    |> Measured.{identifier t.Name}"

        if s.File.EndsWith ".arrow" then
            line $"let {identifier t.Name}: {typeName t.Name}[] ="
            line $"    let batches = ReadArrow.batches ({path})"
            line "    let columns ="
            line "        [|"
            t.Columns |> List.iteri (fun j c -> line $"            {snd (value j c)}")
            line "        |]"
            line $"    Array.init (if columns.Length = 0 then 0 else columns.[0].Length) (fun i ->"
            line "        {"
            t.Columns |> List.iteri (fun j c -> line $"          {fieldLine j (fst (value j c))}")
            line "        })"
            measure ()
        else
            line $"let {identifier t.Name}: {typeName t.Name}[] ="
            line $"    Read.rows ({path})"
            line "    |> Array.map (fun r ->"
            line "        {"
            t.Columns |> List.iteri (fun j c -> line $"          {fieldLine j (fst (value j c))}")
            line "        })"
            measure ()

    line "#endif"
    text.ToString()

