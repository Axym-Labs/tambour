/// The table in the script: a column's unit is a compile-time type in the editor, in the worker and
/// in plain `dotnet fsi`, and new values reach the figure without the prelude being compiled again.
[<Xunit.Collection("workers")>]
module Tambour.Tests.TableScriptTests

open System
open System.Diagnostics
open System.IO
open System.Threading
open System.Threading.Tasks
open Tambour
open Tambour.Data
open Tambour.Protocol
open Xunit

let private log (line: string) = TestContext.Current.TestOutputHelper.WriteLine line

let private limit (seconds: float) =
    if Environment.GetEnvironmentVariable "GITHUB_ACTIONS" = "true" then 3.0 * seconds else seconds

let private command: Command =
    { File = Path.Combine(AppContext.BaseDirectory, (if OperatingSystem.IsWindows() then "tambour.exe" else "tambour"))
      Arguments = [] }

let private init: Init =
    { References = [||]
      Prelude = Shell.prelude
      Skia = { Dpi = float Marey.Space.screenDpi; FontFiles = [||] } }

/// A figure from the table, stating its own opens, so plain `dotnet fsi` runs it too.
let private script (yUnit: string) =
    $"#load \"prelude.fsx\"\nopen Marey\nopen Marey.UnitsOfMeasure\nopen Prelude\n\nlet fig =\n    plot trials {{\n        x U.second (fun r -> r.Time)\n        y {yUnit} (fun r -> r.Displacement)\n        layer GeomPoint\n    }}\n\nprintfn \"%%d rows\" trials.Length\n"

/// A project with a table `trials` of `rows` rows: time in s, fallen distance in m, and, below
/// Arrow's threshold, a sample's name.
let private project (rows: int) =
    let dir = (Directory.CreateTempSubdirectory "tambour-table").FullName
    let t = Array.init rows (fun i -> float i * 0.001)

    let columns =
        [ { Name = "Time"; Unit = "s"; Values = Floats t }
          { Name = "Displacement"; Unit = "m"; Values = Floats(t |> Array.map (fun t -> 4.905 * t * t)) }
          if rows <= Arrow.Threshold then
              { Name = "Sample"; Unit = null; Values = Texts(Array.init rows (fun i -> $"S{i % 3}")) } ]

    let table = { Name = "trials"; Columns = columns }
    let file = if rows > Arrow.Threshold then "trials.arrow" else "trials.csv"
    if rows > Arrow.Threshold then Arrow.writeFile (Path.Combine(dir, file)) table else Csv.writeFile (Path.Combine(dir, file)) table
    Tables.writePrelude dir [ { Table = table; File = file } ] |> ignore
    dir, table

let private until (workers: Workers) (seconds: float) (pick: Reply -> 'a option) : Task<'a * Reply list> =
    task {
        use cts = new CancellationTokenSource(TimeSpan.FromSeconds seconds)
        let seen = ResizeArray()
        let mutable found = None

        try
            while found.IsNone do
                let! reply = workers.Replies.ReadAsync(cts.Token).AsTask()
                seen.Add reply
                found <- pick reply
        with :? OperationCanceledException ->
            failwithf "no such reply within %g s; read %A" seconds (List.ofSeq seen)

        return found.Value, List.ofSeq seen
    }

let private evaluated =
    function
    | Reply.Evaluated e -> Some e
    | _ -> None

let private dotnetFsi (dir: string) (file: string) =
    let marey = Directory.GetFiles(AppContext.BaseDirectory, "Marey*.dll") |> Array.sort
    let info = ProcessStartInfo("dotnet", WorkingDirectory = dir, RedirectStandardOutput = true, RedirectStandardError = true)
    info.ArgumentList.Add "fsi"
    info.ArgumentList.Add "--quiet"
    for d in marey do info.ArgumentList.Add("-r:" + d)
    info.ArgumentList.Add file
    use p = Process.Start info
    let out = p.StandardOutput.ReadToEndAsync()
    let err = p.StandardError.ReadToEnd()
    p.WaitForExit()
    p.ExitCode, out.Result, err

[<Fact>]
let ``a column under the wrong unit is FS0001 in the editor, in the worker, and in plain dotnet fsi`` () : Task =
    task {
        let dir, table = project 300
        let wrong = script "U.second"
        let right = script "U.metre"
        let path = Path.Combine(dir, "figure.fsx")
        File.WriteAllText(path, wrong)
        let line = (wrong.Split('\n') |> Array.findIndex (fun l -> l.Contains "r.Displacement")) + 1

        // The editor: the language service, through the script's #load.
        let service = Analysis.Service { References = [||]; Prelude = Shell.prelude }
        let checkedScript = service.Check { Id = 1; Text = wrong; Path = path }
        let error = checkedScript.Diagnostics |> Array.find (fun d -> d.Severity = "error")
        Assert.Equal("FS0001", error.Code)
        Assert.Equal(line, error.StartLine)
        Assert.DoesNotContain(service.Check({ Id = 2; Text = right; Path = path }).Diagnostics, fun d -> d.Severity = "error")

        // The worker, from the columns the app sends.
        use workers = new Workers(command, init)
        do! workers.StartAsync()
        do! workers.SendAsync(Tables.request table)
        do! workers.SendAsync(Request.Evaluate { Text = wrong; Path = path })
        let! e, _ = until workers 60.0 evaluated
        Assert.False e.Ok
        Assert.Contains(e.Diagnostics, fun d -> d.Code = "FS0001" && d.StartLine = line)
        do! workers.SendAsync(Request.Evaluate { Text = right; Path = path })
        let! e, seen = until workers 60.0 evaluated
        Assert.True(e.Ok, sprintf "%A" e)
        Assert.Equal<string list>([ "fig" ], e.Figures |> Array.map (fun f -> f.Name) |> List.ofArray)
        Assert.Contains(seen, fun r -> match r with Reply.Output o -> o.Text = "300 rows\n" | _ -> false)

        // A worker put on duty after a cancel holds the tables too.
        do! workers.CancelAsync()
        do! workers.SendAsync(Request.Evaluate { Text = right; Path = path })
        let! again, _ = until workers 60.0 evaluated
        Assert.True(again.Ok, sprintf "%A" again)

        // Plain dotnet fsi, from the project's files alone.
        if Evaluation.sdkInstalled () then
            let code, _, err = dotnetFsi dir "figure.fsx"
            Assert.NotEqual(0, code)
            Assert.Contains("FS0001", err)
            File.WriteAllText(path, right)
            let code, out, err = dotnetFsi dir "figure.fsx"
            Assert.True((code = 0), $"dotnet fsi exited {code}: {err}")
            Assert.Contains("300 rows", out)
    }

[<Fact>]
let ``a value edit to a million rows reaches the figure without the prelude compiled again`` () : Task =
    task {
        let rows = 1_000_000
        let dir, table = project rows
        let path = Path.Combine(dir, "figure.fsx")
        let text = script "U.metre"
        use workers = new Workers(command, init)
        do! workers.StartAsync()
        do! workers.SendAsync(Tables.request table)
        do! workers.SendAsync(Request.Evaluate { Text = text; Path = path })
        let! first, _ = until workers 120.0 evaluated
        Assert.True(first.Ok, sprintf "%A" first)
        Assert.True first.Prelude
        let! before, _ = until workers 60.0 (function Reply.Scene f -> Some f | _ -> None)

        // One value changed, the column sent again, the figure run again.
        let edited =
            { table with
                Columns =
                    table.Columns
                    |> List.map (fun c ->
                        match c.Name, c.Values with
                        | "Displacement", Floats a ->
                            let a = Array.copy a
                            a.[0] <- 1000.0
                            { c with Values = Floats a }
                        | _ -> c) }

        let clock = Stopwatch.StartNew()
        do! workers.SendAsync(Tables.request edited)
        do! workers.SendAsync(Request.Evaluate { Text = text; Path = path })
        let! frame, seen = until workers 60.0 (function Reply.Scene f -> Some f | _ -> None)
        let elapsed = clock.Elapsed.TotalSeconds
        let e = seen |> List.pick evaluated
        log $"a value edit to 10^6 rows, to the scene: {elapsed:F2} s (evaluate {e.Timings.Evaluate:F0} ms, layout {e.Timings.Layout:F0} ms, encode {e.Timings.Encode:F0} ms; {frame.Bytes.Length / 1_000_000} MB)"
        Assert.True(e.Ok)
        Assert.False(e.Prelude, "the prelude was compiled again")
        // The value moved the y axis, so the scene is not the one before.
        Assert.False(System.Linq.Enumerable.SequenceEqual(before.Bytes, frame.Bytes), "the figure did not change")
        Assert.True(elapsed < limit 1.0, $"the edit took {elapsed:F2} s")
    }
