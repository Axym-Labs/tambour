open Marey
open Marey.UnitsOfMeasure
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

type Trial = { Time: float<s>; Displacement: float<m>; Sample: string }

// Three samples dropped from rest; one row per 10 ms.
let trials =
    Array.init 300 (fun i ->
        let t = float (i % 100) * 0.01<s>
        { Time = t; Displacement = 0.5 * 9.81<m/s^2> * t * t; Sample = sprintf "S%d" (i / 100) })

let fig =
    plot trials {
        x U.second (fun r -> r.Time)          // elapsed
        y U.metre  (fun r -> r.Displacement)  // fallen distance
        // one colour per sample, legend from the levels
        color (fun r -> r.Sample)
        layer GeomPoint
    }
