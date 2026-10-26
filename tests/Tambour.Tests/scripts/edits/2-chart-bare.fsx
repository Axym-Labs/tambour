open Marey
open Marey.UnitsOfMeasure
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

let t = Array.init 24 (fun i -> float i * 0.4<s>)
let x = t |> Array.map (fun t -> 0.5 * 9.81<m/s^2> * t * t)

let fig = Chart.lineIn U.second U.metre t x // the README's figure, untouched
