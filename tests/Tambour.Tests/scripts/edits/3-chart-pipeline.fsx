open Marey
open Marey.UnitsOfMeasure
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

let t = Array.init 200 (fun i -> float i * 0.05<s>)
let amp = t |> Array.map (fun t -> 0.04<m> * exp (-t / 3.0<s>) * cos (2.0 * System.Math.PI * float t))

let fig =
    Chart.lineIn U.second U.metre t amp
    |> Chart.withTitle "Ringdown"   (* the working title *)
    // keep the envelope in view
    |> Chart.withYDomain -0.05 0.05
    |> Chart.withXLabel "time" |> Chart.withTheme Theme.modern
