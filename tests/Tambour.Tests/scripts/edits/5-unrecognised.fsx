open Marey
open Marey.UnitsOfMeasure
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

let measured = true

/// A helper the user wrote; the app does not look inside it.
let decay (tau: float<s>) =
    let t = Array.init 100 (fun i -> float i * 0.1<s>)
    Chart.lineIn U.second U.metre t (t |> Array.map (fun t -> 1.0<m> * exp (-t / tau)))

let fig =
    if measured then
        decay 2.0<s>
    else
        decay 3.0<s> |> Chart.withTitle "model"
