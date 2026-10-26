open Marey
open Marey.UnitsOfMeasure
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

type Reading = { Temperature: float<K>; Length: float<m>; Run: string }

let readings =
    [| for run in [ "A"; "B" ] do
         for i in 0 .. 49 ->
             let T = 280.0<K> + float i * 2.0<K>
             { Temperature = T; Length = 1.0<m> + 1.2e-5<m/K> * (T - 280.0<K>) * (if run = "A" then 1.0 else 1.1); Run = run } |]

let fig =
    plot readings {
        x U.kelvin (fun r -> r.Temperature)
        y U.metre  (fun r -> r.Length)
        theme Theme.publication
        layer GeomLine
    }
    |> Chart.withLegend LegendBottomRight
