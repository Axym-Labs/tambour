type Trial = { Time: float<s>; Displacement: float<m>; Sample: string }

let trials =
    Array.init 300 (fun i -> let t = float (i % 100) * 0.01<s> in { Time = t; Displacement = 0.5 * 9.81<m/s^2> * t * t; Sample = sprintf "S%d" (i / 100) })

let fig =
    plot trials {
        x U.second (fun r -> r.Time)
        y U.second (fun r -> r.Displacement)
        layer GeomPoint
    }
