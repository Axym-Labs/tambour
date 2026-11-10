namespace Tambour.Data

open System
open Marey

/// How a float column's unit reaches a script.
type Typing =
    /// No unit: a plain `float`.
    | NoUnit
    /// A unit SI composes: the field is `float<Measure>`, in the coherent SI unit, so each value is
    /// multiplied by `Factor` on its way (`km` is `float<m>`, times 1000).
    | Typed of measure: string * factor: float
    /// A unit outside SI, or one that does not parse: a plain `float`, and why.
    | Untyped of reason: string

module Units =

    /// The SI units FSharp.Core spells, by the symbol Marey reads them with, and the litre, which
    /// is exactly a cubic decimetre, so a column in `mL` is `float<m^3>` (times 10^-6). Marey spells
    /// it `L` whether a header writes `L` or `l`.
    let private si =
        set
            [ "m"; "g"; "s"; "A"; "K"; "mol"; "cd"
              "Hz"; "N"; "Pa"; "J"; "W"; "C"; "V"; "F"; "Ω"; "S"; "Wb"; "T"; "H"; "lm"; "lx"; "Bq"; "Gy"; "Sv"; "kat"
              "L" ]

    let private prefixes =
        [ "da"; "Q"; "R"; "Y"; "Z"; "E"; "P"; "T"; "G"; "M"; "k"; "h"; "d"; "c"; "m"; "µ"; "μ"; "u"; "n"; "p"; "f"; "a"; "z"; "y"; "r"; "q" ]

    /// Whether a symbol is an SI unit, with or without a prefix.
    let private isSi (symbol: string) =
        si.Contains symbol
        || prefixes |> List.exists (fun p -> symbol.Length > p.Length && symbol.StartsWith p && si.Contains(symbol.Substring p.Length))

    /// A dimension as F# spells it in SI base units: `kg m^-1 s^-2`, which FSharp.Core's `Pa`
    /// abbreviates, so a field of it plots under `U.pascal`.
    let measureOf (d: Dim) =
        [ "m", d.L; "kg", d.M; "s", d.T; "A", d.I; "K", d.Th; "mol", d.N; "cd", d.J ]
        |> List.filter (fun (_, e) -> e <> 0)
        |> List.map (fun (s, e) -> if e = 1 then s else $"{s}^{e}")
        |> function
            | [] -> "1"
            | parts -> String.Join(" ", parts)

    /// How a unit, as a column's header states it, reaches a script.
    let typing (unitText: string) : Typing =
        if String.IsNullOrWhiteSpace unitText then
            NoUnit
        else
            match U.tryParse unitText with
            | Error why -> Untyped why
            | Ok u ->
                let symbols =
                    if List.isEmpty u.Factors then [ u.Symbol ] else u.Factors |> List.map fst

                match symbols |> List.tryFind (isSi >> not) with
                | Some symbol -> Untyped $"{symbol} is not an SI unit"
                | None when u.OffsetSI <> 0.0 -> Untyped $"{unitText} has an offset from zero"
                | None -> Typed(measureOf u.Dim, u.ToSI)
