/// A float column's uncertainty: the column `u(name)` that holds each reading's own, the text a
/// cell shows and takes for a value with its uncertainty, and the declaration as JSON for the
/// files that keep it (`workspace.json` beside a CSV, the field's metadata in Arrow).
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Tambour.Data.Uncertainty

open System
open System.Globalization
open System.Text.Json
open System.Text.Json.Nodes
open System.Text.RegularExpressions

let private invariant = CultureInfo.InvariantCulture

/// The name of the column holding each row's own uncertainty of a column: `u(h)`.
let companion (name: string) = $"u({name})"

/// The column a companion belongs to, if the name is one: `h` for `u(h)`.
let companionOf (name: string) =
    if not (isNull name) && name.StartsWith "u(" && name.EndsWith ")" && name.Length > 3 then
        Some(name.Substring(2, name.Length - 3))
    else
        None

/// The declaration of a reading's own uncertainty, each row's in `u(name)`.
let perRow (spread: Spread) (source: string) : Uncertainty =
    { PerReading = Some { Spread = spread; Value = nan; Source = source }
      Shared = None
      Dof = infinity }

/// Whether a part's value is each row's own, in the companion column.
let fromColumn (p: Part) = Double.IsNaN p.Value

/// Whether a column's declaration takes its readings' own uncertainties from `u(name)`.
let usesCompanion (c: Column) =
    match c.Uncertainty with
    | Some { PerReading = Some p } -> fromColumn p
    | _ -> false

/// The column holding a column's readings' own uncertainties, and its index, if the table has it.
let companionIn (t: Table) (c: Column) =
    t.Columns |> List.tryFindIndex (fun x -> x.Name = companion c.Name) |> Option.map (fun i -> i, t.Columns.[i])

/// The standard uncertainty a part's value comes to.
let standard (spread: Spread) (value: float) =
    match spread with
    | Spread.Standard -> value
    | Spread.Rectangular -> value / sqrt 3.0
    | Spread.Triangular -> value / sqrt 6.0
    | Spread.Resolution -> value / sqrt 12.0
    | Spread.Expanded k -> value / k

/// How a spread is named in the menus and the column's tip.
let describe (spread: Spread) =
    match spread with
    | Spread.Standard -> "standard uncertainty u"
    | Spread.Rectangular -> "half-width a, rectangular (u = a/√3)"
    | Spread.Triangular -> "half-width a, triangular (u = a/√6)"
    | Spread.Resolution -> "resolution, the last digit's step (u = step/√12)"
    | Spread.Expanded k -> $"expanded U at k = {k.ToString(invariant)} (u = U/k)"

/// What a column's header adds below its name to say it has an uncertainty: ` ± u`, ` ± a`,
/// ` ± U`, with ` + shared` where a part is shared by every reading.
let mark (c: Column) =
    match c.Values, c.Uncertainty with
    | Floats _, Some u ->
        let own =
            match u.PerReading with
            | Some { Spread = Spread.Standard } -> " ± u"
            | Some { Spread = Spread.Rectangular }
            | Some { Spread = Spread.Triangular } -> " ± a"
            | Some { Spread = Spread.Resolution } -> " ± step"
            | Some { Spread = Spread.Expanded _ } -> " ± U"
            | None -> " ±"

        own + (if u.Shared.IsSome then " + shared" else "")
    | _ -> ""

/// A declaration in words, a line per part, as the header's tip and the form say it.
let explain (name: string) (u: Uncertainty) =
    let part (kind: string) (p: Part) =
        let value = if fromColumn p then $"each row's in {companion name}" else p.Value.ToString(invariant)
        let source = if String.IsNullOrEmpty p.Source then "" else $" — {p.Source}"
        $"{kind}: {describe p.Spread}, {value}{source}"

    [ u.PerReading |> Option.map (part "Each reading's own")
      u.Shared |> Option.map (part "Shared by every reading")
      if Double.IsFinite u.Dof then Some $"Degrees of freedom of each reading's: {u.Dof.ToString(invariant)}" else None ]
    |> List.choose id
    |> String.concat "\n"

// ---- A value with its uncertainty, as a cell writes and reads it ----------------------------------

/// How the grid writes a value with its uncertainty: `1.234(5)` or `1.234 ± 0.005`.
type Form =
    | Concise
    | PlusMinus

/// The decimal place of a number's last significant digit when it is given to `digits`.
let private lastPlace (x: float) (digits: int) =
    int (floor (log10 (abs x))) - (digits - 1)

/// A value and its uncertainty, rounded as GUM 7.2.6 has it: the uncertainty to two significant
/// digits, the value to the same place. A missing or zero uncertainty leaves the value as it is.
let format (form: Form) (value: float) (u: float) =
    if Double.IsNaN value then
        ""
    elif Double.IsNaN u || u <= 0.0 || Double.IsInfinity u || Double.IsInfinity value then
        value.ToString("R", invariant)
    else
        let place = lastPlace u 2
        let decimals = max 0 (-place)
        let scale = Math.Pow(10.0, float place)
        let v = Math.Round(value / scale) * scale
        let r = Math.Round(u / scale) * scale
        let text (x: float) = x.ToString("F" + string decimals, invariant)

        match form with
        | PlusMinus -> $"{text v} ± {text r}"
        | Concise ->
            let digits = Math.Round(u / scale)
            // In the value's last places when it has decimals; in its unit when it has none.
            if decimals > 0 then $"{text v}({digits.ToString(invariant)})" else $"{text v}({text r})"

let private number = @"[+-]?(?:\d+\.?\d*|\.\d+)(?:[eE][+-]?\d+)?"

let private plusMinus = Regex($@"^\s*(?<v>{number})\s*(?:±|\+-|\+/-)\s*(?<u>{number})\s*$")

let private concise = Regex(@"^\s*(?<v>[+-]?(?:\d+\.?(?<d>\d*)|\.(?<d>\d+)))(?<e>[eE][+-]?\d+)?\s*\((?<u>\d+\.?\d*)\)\s*$")

/// A value and its uncertainty from a cell's text: `1.234 ± 0.005`, `1.234 +- 0.005` or the
/// concise `1.234(5)`, whose digits are in the value's last places. None for any other text.
let parse (text: string) : (float * float) option =
    if isNull text then
        None
    else
        let m = plusMinus.Match text

        if m.Success then
            Some(Double.Parse(m.Groups.["v"].Value, NumberStyles.Float, invariant), Double.Parse(m.Groups.["u"].Value, NumberStyles.Float, invariant))
        else
            let m = concise.Match text

            if not m.Success then
                None
            else
                let exponent = if m.Groups.["e"].Success then Int32.Parse(m.Groups.["e"].Value.Substring 1, invariant) else 0
                let value = Double.Parse(m.Groups.["v"].Value + m.Groups.["e"].Value, NumberStyles.Float, invariant)
                let digits = m.Groups.["u"].Value

                // `1.234(5)` is 0.005; `1234(5)` and `1.2(0.3)` read as they stand.
                let u =
                    if digits.Contains "." || m.Groups.["d"].Value = "" then
                        Double.Parse(digits, invariant) * Math.Pow(10.0, float exponent)
                    else
                        Double.Parse(digits, invariant) * Math.Pow(10.0, float (exponent - m.Groups.["d"].Value.Length))

                Some(value, u)

// ---- As JSON ---------------------------------------------------------------------------------

let private spreadText (s: Spread) =
    match s with
    | Spread.Standard -> "standard"
    | Spread.Rectangular -> "rectangular"
    | Spread.Triangular -> "triangular"
    | Spread.Resolution -> "resolution"
    | Spread.Expanded k -> "expanded k=" + k.ToString("R", invariant)

let private spreadOf (text: string) =
    match text with
    | "standard" -> Some Spread.Standard
    | "rectangular" -> Some Spread.Rectangular
    | "triangular" -> Some Spread.Triangular
    | "resolution" -> Some Spread.Resolution
    | t when not (isNull t) && t.StartsWith "expanded k=" ->
        match Double.TryParse(t.Substring 11, NumberStyles.Float, invariant) with
        | true, k when k > 0.0 -> Some(Spread.Expanded k)
        | _ -> None
    | _ -> None

let private partNode (p: Part) =
    let o = JsonObject()
    o.["spread"] <- JsonValue.Create(spreadText p.Spread)
    // Each row's own, in the companion column, is written as no value at all.
    if not (fromColumn p) then o.["value"] <- JsonValue.Create(p.Value)
    if not (String.IsNullOrEmpty p.Source) then o.["source"] <- JsonValue.Create(p.Source)
    o

/// A declaration as a JSON object.
let toNode (u: Uncertainty) : JsonObject =
    let o = JsonObject()
    u.PerReading |> Option.iter (fun p -> o.["perReading"] <- partNode p)
    u.Shared |> Option.iter (fun p -> o.["shared"] <- partNode p)
    if not (Double.IsPositiveInfinity u.Dof) then o.["dof"] <- JsonValue.Create(u.Dof)
    o

let toJson (u: Uncertainty) = (toNode u).ToJsonString()

let private partOf (node: JsonNode) : Part option =
    match node with
    | :? JsonObject as o ->
        let text (key: string) =
            match o.[key] with
            | null -> null
            | n -> try n.GetValue<string>() with _ -> null

        match spreadOf (text "spread") with
        | None -> None
        | Some spread ->
            let value =
                match o.["value"] with
                | null -> nan
                | n -> try n.GetValue<float>() with _ -> nan

            Some { Spread = spread; Value = value; Source = (match text "source" with null -> "" | s -> s) }
    | _ -> None

/// A declaration from a JSON object; None for one that states neither part.
let ofNode (node: JsonNode) : Uncertainty option =
    match node with
    | :? JsonObject as o ->
        let dof =
            match o.["dof"] with
            | null -> infinity
            | n -> try n.GetValue<float>() with _ -> infinity

        match o.["perReading"] |> partOf, o.["shared"] |> partOf with
        | None, None -> None
        | perReading, shared -> Some { PerReading = perReading; Shared = shared; Dof = (if dof > 0.0 then dof else infinity) }
    | _ -> None

let ofJson (json: string) : Uncertainty option =
    try ofNode (JsonNode.Parse json) with _ -> None

// ---- Combining repeated readings -------------------------------------------------------------

/// Readings of one quantity in columns side by side, combined row by row: the mean, its standard
/// uncertainty s/√n and the readings counted, over the readings a row has. A row with fewer than
/// two has no uncertainty from them (nan), and none at all a mean (nan).
let combine (readings: float[][]) : float[] * float[] * int[] =
    let rows = if readings.Length = 0 then 0 else readings.[0].Length

    let each =
        Array.init rows (fun i ->
            let xs = readings |> Array.map (fun c -> c.[i]) |> Array.filter Double.IsFinite
            let n = xs.Length

            if n = 0 then
                nan, nan, 0
            else
                let mean = Array.average xs

                if n < 2 then
                    mean, nan, n
                else
                    let s = sqrt ((xs |> Array.sumBy (fun x -> (x - mean) * (x - mean))) / float (n - 1))
                    mean, s / sqrt (float n), n)

    each |> Array.map (fun (m, _, _) -> m), each |> Array.map (fun (_, u, _) -> u), each |> Array.map (fun (_, _, n) -> n)

/// The name a column of combined readings takes: `t` for `t1 … t5`, `T` for `T_1 …`, or the first
/// reading's name with ` mean`.
let combinedName (names: string list) =
    let stem (n: string) = Regex.Replace(n, @"[ _\-]*\d+$", "")

    match names |> List.map stem |> List.distinct with
    | [ s ] when s <> "" && not (names |> List.contains s) -> s
    | _ -> (List.head names) + " mean"
