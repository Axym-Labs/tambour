/// From the table to a figure and back: a `plot` block written for selected columns, and the rows
/// nearest the points a reader picks in the figure.
module Tambour.Plotting

open System
open System.Text.RegularExpressions
open Tambour.Data

/// A column's accessor in a plot block's `x` or `y`: under its witness where the script sees it
/// typed, as a plain float otherwise; a column that is not a number cannot be an axis.
let private axis (name: string) (c: Column) : Result<string, string> =
    let field = $"r.{Prelude.identifier c.Name}"

    match c.Values with
    | Floats _ ->
        match Prelude.witness c.Unit with
        | Some w -> Ok $"{name} {w} (fun r -> {field})"
        | None -> Ok $"{name} (fun r -> {field})"
    | Ints _ -> Ok $"{name} (fun r -> float {field})"
    | v -> Error $"{c.Name} holds {v.Kind}s, and an axis holds numbers"

/// A name for a new figure the script does not bind yet: `fig`, `fig2`, ….
let freshFigure (script: string) =
    let taken = Regex.Matches(script, @"^let\s+(fig\d*)\b", RegexOptions.Multiline) |> Seq.map (fun m -> m.Groups.[1].Value) |> Set.ofSeq
    Seq.initInfinite (fun i -> if i = 0 then "fig" else $"fig{i + 1}") |> Seq.find (taken.Contains >> not)

/// The script with a figure of the table's selected columns added: the first is x, the second y,
/// a third the colour. The prelude is loaded and opened at the top where the script does not yet.
let plotColumns (script: string) (table: Table) (columns: int list) : Result<string * string, string> =
    match columns |> List.map (fun i -> table.Columns.[i]) with
    | x :: y :: rest ->
        match axis "x" x, axis "y" y with
        | Error why, _
        | _, Error why -> Error why
        | Ok xOp, Ok yOp ->
            let name = freshFigure script
            let colour = match rest with c :: _ -> [ $"color (fun r -> r.{Prelude.identifier c.Name})" ] | [] -> []
            let ops = [ xOp; yOp ] @ colour @ [ "layer GeomPoint" ]
            let block = String.Join("\n", [ $"let {name} ="; $"    plot {Prelude.identifier table.Name} {{" ] @ (ops |> List.map (fun o -> "        " + o)) @ [ "    }" ])

            let header =
                [ if not (Regex.IsMatch(script, @"^[ \t]*#load[ \t]+""prelude\.fsx""", RegexOptions.Multiline)) then "#load \"prelude.fsx\""
                  if not (Regex.IsMatch(script, @"^[ \t]*open[ \t]+Prelude\b", RegexOptions.Multiline)) then "open Prelude" ]

            let body = script.TrimEnd()
            let top = if header.IsEmpty then "" else String.Join("\n", header) + "\n"
            Ok(top + (if body = "" then "" else body + "\n\n") + block + "\n", name)
    | _ -> Error "select two columns, the x first and then the y"

/// A column's values as the axis has them: a typed float in its coherent SI unit.
let private axisValues (c: Column) : float[] option =
    match c.Values with
    | Floats a ->
        match Units.typing c.Unit with
        | Typed(_, factor) when factor <> 1.0 -> Some(a |> Array.map (fun v -> v * factor))
        | _ -> Some a
    | Ints a -> Some(a |> Array.map float)
    | _ -> None

let private columnNamed (table: Table) (field: string) =
    table.Columns |> List.tryFind (fun c -> Prelude.identifier c.Name = field || c.Name = field)

let private spread (a: float[]) =
    let finite = a |> Array.filter Double.IsFinite
    if finite.Length = 0 then 1.0 else max 1e-300 (Array.max finite - Array.min finite)

/// For each picked point, as data, the row nearest it, each axis scaled by its column's spread;
/// none for a point off every plot.
let nearest (table: Table) (xField: string) (yField: string) (points: (float * float)[]) : int option[] =
    match columnNamed table xField |> Option.bind axisValues, columnNamed table yField |> Option.bind axisValues with
    | Some xs, Some ys ->
        let sx, sy = spread xs, spread ys

        points
        |> Array.map (fun (px, py) ->
            if Double.IsNaN px || Double.IsNaN py then
                None
            else
                let mutable best = -1
                let mutable distance = infinity

                for i in 0 .. xs.Length - 1 do
                    let dx = (xs.[i] - px) / sx
                    let dy = (ys.[i] - py) / sy
                    let d = dx * dx + dy * dy

                    if d < distance then
                        distance <- d
                        best <- i

                if best < 0 then None else Some best)
    | _ -> Array.create points.Length None

/// The rows whose point lies inside a rectangle of data, its corners in either order.
let inside (table: Table) (xField: string) (yField: string) (x0: float, y0: float) (x1: float, y1: float) : int list =
    match columnNamed table xField |> Option.bind axisValues, columnNamed table yField |> Option.bind axisValues with
    | Some xs, Some ys ->
        let lx, hx, ly, hy = min x0 x1, max x0 x1, min y0 y1, max y0 y1
        [ for i in 0 .. xs.Length - 1 do
              if xs.[i] >= lx && xs.[i] <= hx && ys.[i] >= ly && ys.[i] <= hy then i ]
    | _ -> []

/// The data of rows, as the axes have it, to be placed in the figure.
let dataOf (table: Table) (xField: string) (yField: string) (rows: int list) : (float * float) list =
    match columnNamed table xField |> Option.bind axisValues, columnNamed table yField |> Option.bind axisValues with
    | Some xs, Some ys -> rows |> List.map (fun i -> xs.[i], ys.[i])
    | _ -> []
