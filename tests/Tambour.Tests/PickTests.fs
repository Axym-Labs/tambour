/// From the table to a figure and back: a plot block written for two columns, and a pick at each
/// marker naming its row.
[<Xunit.Collection("workers")>]
module Tambour.Tests.PickTests

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open Marey
open Tambour
open Tambour.Data
open Tambour.Protocol
open Xunit

let private command: Command =
    { File = Path.Combine(AppContext.BaseDirectory, (if OperatingSystem.IsWindows() then "tambour.exe" else "tambour"))
      Arguments = [] }

let private init: Init =
    { References = [||]
      Prelude = Shell.prelude
      Skia = { Dpi = float Space.screenDpi; FontFiles = [||] } }

/// Each marker's centre in the figure's px, through the transforms above it, in drawing order.
let rec private centres (ctm: Affine<nd, px>) (n: Node) : Vec2<px> seq =
    match n with
    | Markers b -> b.Points |> Seq.map (Affine.apply ctm)
    | Group ns -> ns |> Seq.collect (centres ctm)
    | Transform(a, c) -> centres (Affine.compose a ctm) c
    | Clip(_, c)
    | Opacity(_, c)
    | Keyed(_, c)
    | Annotated(_, c) -> centres ctm c
    | _ -> Seq.empty

let private until (workers: Workers) (pick: Reply -> 'a option) : Task<'a> =
    task {
        use cts = new CancellationTokenSource(TimeSpan.FromSeconds 60.0)
        let mutable found = None

        while found.IsNone do
            let! reply = workers.Replies.ReadAsync(cts.Token).AsTask()
            found <- pick reply

        return found.Value
    }

[<Fact>]
let ``the plot block for two columns compiles under their witnesses, and an untyped column plots as a plain float under U.one`` () =
    Assert.Equal(Some "U.second", Prelude.witness "s")
    Assert.Equal(Some "U.metre", Prelude.witness "km")
    Assert.Equal(Some "(U.div U.volt U.metre)", Prelude.witness "V/m")
    Assert.Equal(Some "(U.div (U.mul U.kilogram U.metre) (U.square U.second))", Prelude.witness "kg m s-2")
    Assert.Equal(None, Prelude.witness "ppm")

    let table = { Name = "trials"; Columns = [ { Name = "Time"; Unit = "s"; Values = Floats [| 0.0 |] }; { Name = "conc"; Unit = "ppm"; Values = Floats [| 1.0 |] }; { Name = "Run"; Unit = null; Values = Texts [| "a" |] } ] }

    match Plotting.plotColumns "let fig = 1\n" table [ 0; 1; 2 ] with
    | Ok(text, name) ->
        Assert.Equal("fig2", name)
        Assert.StartsWith("#load \"prelude.fsx\"\nopen Prelude\nlet fig = 1\n\nlet fig2 =\n    plot trials {\n        x U.second (fun r -> r.Time)\n        y U.one (fun r -> float r.conc)\n        color (fun r -> r.Run)\n", text)
    | Error e -> failwith e

    Assert.True(Plotting.plotColumns "" table [ 2; 0 ] |> Result.isError)

/// Gate: on a scatter of 10^4 rows, a pick at each marker's centre names that marker's row.
[<Fact>]
let ``a pick at each marker's centre names its row, on a scatter of ten thousand`` () : Task =
    task {
        let n = 100
        let random = Random 11
        let xs = Array.init (n * n) (fun k -> float (k % n) + 0.2 * random.NextDouble())
        let ys = Array.init (n * n) (fun k -> float (k / n) * 0.01 + 0.002 * random.NextDouble())
        let table = { Name = "lattice"; Columns = [ { Name = "X"; Unit = "s"; Values = Floats xs }; { Name = "Y"; Unit = "km"; Values = Floats ys } ] }
        let dir = (Directory.CreateTempSubdirectory "tambour-pick").FullName
        Tables.writePrelude dir [ { Table = table; File = "lattice.csv" } ] |> ignore
        let text, name = Plotting.plotColumns "" table [ 0; 1 ] |> Result.defaultWith failwith
        let path = Path.Combine(dir, "figure.fsx")

        use workers = new Workers(command, init)
        do! workers.StartAsync()
        do! workers.SendAsync(Request.Live { Kind = "size"; Args = [| 1200.0; 900.0 |] })
        do! workers.SendAsync(Tables.request table)
        do! workers.SendAsync(Request.Evaluate { Text = text; Path = path })
        let! evaluated = until workers (function Reply.Evaluated e -> Some e | _ -> None)
        Assert.True(evaluated.Ok, sprintf "%A" evaluated.Diagnostics)
        let! frame = until workers (function Reply.Scene f -> Some f | _ -> None)
        let node = Wire.decodeWith frame.Bytes |> Result.map fst |> Result.defaultWith failwith
        let extent: Extent<px> = { Width = frame.Width * 1.0<px>; Height = frame.Height * 1.0<px> }
        let marks = centres (SkiaPainter.RootTransform extent) node |> Array.ofSeq
        Assert.Equal(n * n, marks.Length)

        // The block's rows and fields, as the language service reads them.
        let rows, xField, yField = Tambour.Edits.Edits.plotted path name text |> Option.get
        Assert.Equal(("lattice", "X", "Y"), (rows, xField, yField))

        do! workers.SendAsync(Request.Pick { Values = marks |> Array.collect (fun p -> [| float p.X; float p.Y |]); Clamp = false })
        let! picked = until workers (function Reply.Picked p -> Some p | _ -> None)
        let data = Array.init marks.Length (fun i -> picked.Values.[2 * i], picked.Values.[2 * i + 1])
        let found = Plotting.nearest table xField yField data
        let wrong = found |> Array.indexed |> Array.filter (fun (i, r) -> r <> Some i)
        TestContext.Current.TestOutputHelper.WriteLine $"{marks.Length} picks, {wrong.Length} naming another row"
        Assert.Empty wrong

        // Placed back, each row's datum is its marker's centre.
        do! workers.SendAsync(Request.Place { Values = Plotting.dataOf table xField yField [ 0 .. marks.Length - 1 ] |> List.collect (fun (x, y) -> [ x; y ]) |> Array.ofList; Clamp = false })
        let! placed = until workers (function Reply.Placed p -> Some p | _ -> None)
        let worst = marks |> Array.mapi (fun i p -> max (abs (placed.Values.[2 * i] - float p.X)) (abs (placed.Values.[2 * i + 1] - float p.Y))) |> Array.max
        Assert.True(worst < 0.01, $"a row is placed {worst} px from its marker")
    }
