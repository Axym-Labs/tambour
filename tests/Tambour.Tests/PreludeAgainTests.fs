/// A worker's session holds every version of the prelude it evaluated; a script runs against the
/// latest, through a rename and its undo and the values sent after.
[<Xunit.Collection("workers")>]
module Tambour.Tests.PreludeAgainTests

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open Tambour
open Tambour.Data
open Tambour.Protocol
open Xunit

[<Fact>]
let ``a prelude changed and changed back runs the script again`` () : Task =
    task {
        let dir = (Directory.CreateTempSubdirectory "tambour-again").FullName
        let t = Array.init 10 float
        let a = { Name = "trials"; Columns = [ { Name = "Time"; Unit = "s"; Values = Floats t }; { Name = "Displacement"; Unit = "m"; Values = Floats t } ] }
        let b = { a with Columns = [ a.Columns.[0]; { a.Columns.[1] with Name = "Height" } ] }
        let script = "#load \"prelude.fsx\"\nopen Prelude\nlet fig = plot trials { x U.second (fun r -> r.Time); y U.metre (fun r -> r.Displacement); layer GeomPoint }\n"
        let path = Path.Combine(dir, "figure.fsx")
        let command: Command = { File = Path.Combine(AppContext.BaseDirectory, "tambour"); Arguments = [] }
        use workers = new Workers(command, { References = [||]; Prelude = Shell.prelude; Skia = { Dpi = 3.78; FontFiles = [||] } })
        do! workers.StartAsync()

        let run (table: Table) =
            task {
                Tables.writePrelude dir [ { Table = table; File = "trials.csv" } ] |> ignore
                do! workers.SendAsync(Tables.request table)
                do! workers.SendAsync(Request.Evaluate { Text = script; Path = path })
                use cts = new CancellationTokenSource(TimeSpan.FromSeconds 60.0)
                let mutable e = None
                while e.IsNone do
                    match! workers.Replies.ReadAsync(cts.Token).AsTask() with
                    | Reply.Evaluated x -> e <- Some x
                    | _ -> ()
                return e.Value
            }

        let edited (k: float) (t: Table) = { t with Columns = [ t.Columns.[0]; { t.Columns.[1] with Values = Floats(Array.init 10 (fun i -> float i * k)) } ] }
        let! first = run a
        // As the window did it: a value, taken back, then the rename and its undo.
        let! _ = run (edited 100.0 a)
        let! _ = run a
        let! second = run b
        let! third = run a
        // New values, the schema as it was: read into the binding, the prelude not evaluated.
        let! fourth = run { a with Columns = [ a.Columns.[0]; { a.Columns.[1] with Values = Floats(Array.init 10 (fun i -> float (i * 2))) } ] }
        Assert.True(fourth.Ok, sprintf "%A" fourth.Diagnostics)
        Assert.True first.Ok
        Assert.False second.Ok
        Assert.True(third.Ok, sprintf "%A" third.Diagnostics)
    }
