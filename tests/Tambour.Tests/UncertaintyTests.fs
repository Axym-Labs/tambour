/// A column's uncertainty: declared once where the data is, kept in the files, written and typed in
/// the grid, typed `Uncertain<…>` in the script, plotted with its error bars, and a bound value's
/// budget shown beside the figures.
[<Xunit.Collection("workers")>]
module Tambour.Tests.UncertaintyTests

open System
open System.Diagnostics
open System.IO
open System.Threading
open System.Threading.Tasks
open Marey
open Tambour
open Tambour.Data
open Tambour.Protocol
open Xunit

let private part spread value source : Part = { Spread = spread; Value = value; Source = source }

/// A pendulum's lengths in mm, each read off a ruler to 1 mm (each row's in `u(L)`) with a zero
/// offset of ±0.5 mm shared by every reading, and its periods in s, each ±4 ms from five readings.
let private pendulum () : Table =
    { Name = "pendulum"
      Columns =
        [ { Name = "L"
            Unit = "mm"
            Values = Floats [| 1000.0; 800.0; 600.0 |]
            Uncertainty =
              Some
                  { PerReading = Some(part Spread.Resolution nan "ruler")
                    Shared = Some(part Spread.Rectangular 0.5 "zero offset")
                    Dof = infinity } }
          { Name = "u(L)"; Unit = "mm"; Values = Floats [| 1.0; 1.0; nan |]; Uncertainty = None }
          { Name = "T"
            Unit = "s"
            Values = Floats [| 2.006; 1.79; 1.55 |]
            Uncertainty = Some { PerReading = Some(part Spread.Standard 0.004 ""); Shared = None; Dof = 4.0 } }
          { Name = "run"; Unit = null; Values = Texts [| "a"; "b"; "c" |]; Uncertainty = None } ] }

// ---- The text of a value with its uncertainty -----------------------------------------------

[<Theory>]
[<InlineData("1.234 ± 0.005", 1.234, 0.005)>]
[<InlineData("1.234 +- 0.005", 1.234, 0.005)>]
[<InlineData(" 1.234+/-0.005 ", 1.234, 0.005)>]
[<InlineData("1.234(5)", 1.234, 0.005)>]
[<InlineData("9.811(39)", 9.811, 0.039)>]
[<InlineData("1234(5)", 1234.0, 5.0)>]
[<InlineData("-0.50(12)", -0.5, 0.12)>]
[<InlineData("1.2(0.3)", 1.2, 0.3)>]
let ``a cell takes a value with its uncertainty as ±, +- or the concise form`` (text: string, value: float, u: float) =
    match Uncertainty.parse text with
    | Some(v, w) ->
        Assert.Equal(value, v, 12)
        Assert.Equal(u, w, 12)
    | None -> failwith $"{text} did not read"

[<Theory>]
[<InlineData("1.234")>]
[<InlineData("")>]
[<InlineData("1.2(x)")>]
[<InlineData("± 0.005")>]
let ``other text is no value with an uncertainty`` (text: string) = Assert.Equal(None, Uncertainty.parse text)

[<Fact>]
let ``a value is written to the place of its uncertainty's second significant digit`` () =
    Assert.Equal("9.811(39)", Uncertainty.format Uncertainty.Concise 9.81137 0.03894)
    Assert.Equal("9.811 ± 0.039", Uncertainty.format Uncertainty.PlusMinus 9.81137 0.03894)
    Assert.Equal("1000.0(10)", Uncertainty.format Uncertainty.Concise 1000.0 1.0)
    Assert.Equal("1234(45)", Uncertainty.format Uncertainty.Concise 1234.0 45.0)
    Assert.Equal("1230(450)", Uncertainty.format Uncertainty.Concise 1234.0 450.0)
    // No uncertainty leaves the value as it is.
    Assert.Equal("1.5", Uncertainty.format Uncertainty.Concise 1.5 nan)

[<Fact>]
let ``what the grid writes it reads back`` () =
    for form in [ Uncertainty.Concise; Uncertainty.PlusMinus ] do
        let text = Uncertainty.format form 2.00613 0.00402
        let v, u = (Uncertainty.parse text).Value
        Assert.Equal(2.0061, v, 10)
        Assert.Equal(0.0040, u, 10)

// ---- The declaration in the files -------------------------------------------------------------

[<Fact>]
let ``a declaration round-trips through JSON, a missing value as each row's own`` () =
    for c in (pendulum ()).Columns do
        match c.Uncertainty with
        | Some u ->
            let back = (Uncertainty.ofJson (Uncertainty.toJson u)).Value
            Assert.Equal(Uncertainty.toJson u, Uncertainty.toJson back)
            Assert.Equal(u.PerReading.Value.Spread, back.PerReading.Value.Spread)
            Assert.Equal(Double.IsNaN u.PerReading.Value.Value, Double.IsNaN back.PerReading.Value.Value)
            Assert.Equal(u.Dof, back.Dof)
        | None -> ()

    Assert.Equal(Some(Spread.Expanded 2.5), (Uncertainty.ofJson """{"shared":{"spread":"expanded k=2.5","value":0.1}}""").Value.Shared |> Option.map (fun p -> p.Spread))
    Assert.Equal(None, Uncertainty.ofJson """{"perReading":{"spread":"sideways"}}""")

[<Fact>]
let ``a table in Arrow keeps its declarations in its fields`` () =
    let t = pendulum ()
    use stream = new MemoryStream()
    Arrow.write stream t
    stream.Position <- 0L
    let back = match Arrow.read t.Name stream with Ok b -> b | Error e -> failwith e
    Assert.Equal<string list>(t.Columns |> List.map (fun c -> c.Uncertainty |> Option.map Uncertainty.toJson |> Option.defaultValue ""), back.Columns |> List.map (fun c -> c.Uncertainty |> Option.map Uncertainty.toJson |> Option.defaultValue ""))

[<Fact>]
let ``a table in CSV keeps its declarations in workspace.json, and a CSV reads anywhere`` () =
    let dir = (Directory.CreateTempSubdirectory "tambour-uncertainty").FullName
    Project.makeWorkspace dir
    let t = pendulum ()
    let path = Path.Combine(dir, "tables", "pendulum.csv")
    Csv.writeFile path t
    Project.write dir { Project.defaults "figure.fsx" with Uncertainty = Project.uncertaintiesOf [ t ] }

    // The CSV holds plain columns: the companion is one more column in the unit.
    Assert.StartsWith("L,u(L),T,run\nmm,mm,s,#string\n1000,1,2.006,a\n", File.ReadAllText path)

    let read = match Csv.readFile path with Ok r -> r | Error e -> failwith e
    Assert.True(read.Columns |> List.forall (fun c -> c.Uncertainty.IsNone))
    let declared = Project.declare (Project.read dir) read
    Assert.Equal(Uncertainty.toJson t.Columns.[0].Uncertainty.Value, Uncertainty.toJson declared.Columns.[0].Uncertainty.Value)
    Assert.Equal(Uncertainty.toJson t.Columns.[2].Uncertainty.Value, Uncertainty.toJson declared.Columns.[2].Uncertainty.Value)
    Assert.Equal(None, declared.Columns.[1].Uncertainty)

[<Fact>]
let ``a workspace from before opens with no declarations`` () =
    let dir = (Directory.CreateTempSubdirectory "tambour-uncertainty").FullName
    File.WriteAllText(Path.Combine(dir, Project.WorkspaceFile), """{ "Script": "figure.fsx", "Marey": "1.0.0-rc.3", "Skia": { "Dpi": 3.78, "FontFiles": [] }, "Split": 0.5, "Bottom": 0.0, "Columns": [] }""")
    let settings = (Project.read dir).Value
    Assert.Empty settings.Uncertainty

// ---- The edits ---------------------------------------------------------------------------------

let private applied (t: Table) (edits: TableEdit list) =
    (edits |> List.fold TableEdit.apply (Map.ofList [ t.Name, t ])).[t.Name]

let private undone (t: Table) (edits: TableEdit list) =
    (edits |> List.rev |> List.map TableEdit.invert |> List.fold TableEdit.apply (Map.ofList [ t.Name, t ])).[t.Name]

let private plain () : Table =
    { Name = "drop"
      Columns =
        [ { Name = "h"; Unit = "m"; Values = Floats [| 1.0; 2.0; 3.0 |]; Uncertainty = None }
          { Name = "t"; Unit = "s"; Values = Floats [| 0.45; 0.64; 0.78 |]; Uncertainty = None } ] }

[<Fact>]
let ``typing a value with its uncertainty makes u(name) beside it and declares it, undone as one`` () =
    let t = plain ()
    let edits = TableEdit.setMeasured t 0 1 2.05 0.01
    let after = applied t edits

    Assert.Equal<string list>([ "h"; "u(h)"; "t" ], after.Columns |> List.map (fun c -> c.Name))
    Assert.Equal("m", after.Columns.[1].Unit)
    match after.Columns.[0].Values, after.Columns.[1].Values with
    | Floats h, Floats u ->
        Assert.Equal(2.05, h.[1])
        Assert.Equal(0.01, u.[1])
        Assert.True(Double.IsNaN u.[0])
    | _ -> failwith "not floats"

    Assert.True(Uncertainty.usesCompanion after.Columns.[0])
    Assert.True((Table.create after.Name after.Columns) |> Result.isOk)

    // Once there, the next row's goes in both columns and nothing else.
    let next = TableEdit.setMeasured after 0 2 3.1 0.02
    Assert.Equal(2, next.Length)
    Assert.True(next |> List.forall (function Cells _ -> true | _ -> false))

    // The history takes the first as one entry; back, the table is as it was.
    let back = undone after edits
    Assert.Equal<string list>([ "h"; "t" ], back.Columns |> List.map (fun c -> c.Name))
    Assert.Equal(None, back.Columns.[0].Uncertainty)
    Assert.True(back.Columns.[0].Values.Equals(t.Columns.[0].Values))

[<Fact>]
let ``a value the same for every row becomes each row's own in u(name) when one row's is typed`` () =
    let t = plain ()
    let declared = applied t (TableEdit.declare t 0 (Some { PerReading = Some(part Spread.Resolution 0.001 "tape"); Shared = None; Dof = infinity }))
    Assert.Equal(2, declared.Columns.Length)
    let after = applied declared (TableEdit.setMeasured declared 0 0 1.002 0.002)

    match after.Columns.[1].Values with
    | Floats u -> Assert.Equal<float list>([ 0.002; 0.001; 0.001 ], List.ofArray u)
    | _ -> failwith "not floats"

    // Its spread stays the declaration's.
    Assert.Equal(Spread.Resolution, after.Columns.[0].Uncertainty.Value.PerReading.Value.Spread)

[<Fact>]
let ``repeated readings combine into their means and s over the root of n, n − 1 degrees of freedom`` () =
    let t: Table =
        { Name = "timing"
          Columns =
            [ { Name = "t1"; Unit = "s"; Values = Floats [| 2.01; 1.80 |]; Uncertainty = None }
              { Name = "t2"; Unit = "s"; Values = Floats [| 2.00; 1.78 |]; Uncertainty = None }
              { Name = "t3"; Unit = "s"; Values = Floats [| 2.01; nan |]; Uncertainty = None } ] }

    let edits = match TableEdit.combineReadings t [ 0; 1; 2 ] with Ok e -> e | Error why -> failwith why
    let after = applied t edits
    Assert.Equal<string list>([ "t1"; "t2"; "t3"; "t"; "u(t)" ], after.Columns |> List.map (fun c -> c.Name))

    match after.Columns.[3].Values, after.Columns.[4].Values with
    | Floats m, Floats u ->
        Assert.Equal((2.01 + 2.00 + 2.01) / 3.0, m.[0], 12)
        // Each row's over the readings it has: the second has two.
        Assert.Equal((1.80 + 1.78) / 2.0, m.[1], 12)
        let expected = (Uncertain.ofReadings "t" U.second [ 2.01<s>; 2.00<s>; 2.01<s> ]).Uncertainty |> float
        Assert.Equal(expected, u.[0], 12)
        Assert.Equal(0.01, u.[1], 12)
    | _ -> failwith "not floats"

    // The fewest readings a row has set ν: two, so one.
    Assert.Equal(1.0, after.Columns.[3].Uncertainty.Value.Dof)
    Assert.True(Uncertainty.usesCompanion after.Columns.[3])
    Assert.True(after.Columns.[0].Uncertainty.IsNone)

    Assert.True(TableEdit.combineReadings t [ 0 ] |> Result.isError)
    let mixed = { t with Columns = t.Columns |> List.mapi (fun i c -> if i = 2 then { c with Unit = "ms" } else c) }
    Assert.Contains("one unit", match TableEdit.combineReadings mixed [ 0; 1; 2 ] with Error why -> why | Ok _ -> "")

[<Theory>]
[<InlineData("t1|t2|t3", "t")>]
[<InlineData("T_1|T_2", "T")>]
[<InlineData("a|b", "a mean")>]
let ``a column of combined readings is named for them`` (names: string, expected: string) =
    Assert.Equal(expected, Uncertainty.combinedName (List.ofArray (names.Split '|')))

// ---- The grid -----------------------------------------------------------------------------------

[<Fact>]
let ``the grid writes a value with its own uncertainty and edits it whole`` () =
    let t = pendulum ()
    // T: a standard uncertainty, concise.
    Assert.Equal("2.0060(40)", CellText.shownIn t 2 0)
    // L: a resolution step is no standard uncertainty, so it is written with ±.
    Assert.Equal("1000.0 ± 1.0", CellText.shownIn t 0 0)
    // A row with no own uncertainty in u(L) shows its value alone.
    Assert.Equal("600", CellText.shownIn t 0 2)
    Assert.Equal("1000 ± 1", CellText.exactIn t 0 0)
    Assert.Equal("run", t.Columns.[3].Name)
    Assert.Equal("a", CellText.shownIn t 3 0)
    Assert.Equal(" ± step + shared", Uncertainty.mark t.Columns.[0])

// ---- The prelude ----------------------------------------------------------------------------------

[<Fact>]
let ``a table with no uncertainty gets the prelude it always had`` () =
    let text = Prelude.generate [ { Table = plain (); File = "tables/drop.csv" } ]
    Assert.DoesNotContain("Raw", text)
    Assert.DoesNotContain("Measured", text)
    Assert.DoesNotContain("open Marey", text)
    Assert.Contains("let mutable drop: Drop[] = Tambour.Data.Bound.rows<Drop> \"drop\"\n", text)

[<Fact>]
let ``an uncertain column is Uncertain of its unit, and its companion is no field`` () =
    let text = Prelude.generate [ { Table = pendulum (); File = "tables/pendulum.csv" } ]
    Assert.Contains("type Pendulum =", text)
    Assert.Contains("{ L: Uncertain<m>", text)
    Assert.Contains("T: Uncertain<s>", text)
    Assert.Contains("let mutable pendulum: Pendulum[] = Tambour.Data.Bound.rows<Raw.Pendulum> \"pendulum\" |> Measured.pendulum", text)
    let publicType = text.Substring(text.IndexOf "\ntype Pendulum =")
    Assert.DoesNotContain("``u(L)``:", publicType.Substring(0, publicType.IndexOf "module Measured"))

[<Fact>]
let ``plot selected columns writes the uncertain accessors`` () =
    match Plotting.plotColumns "" (pendulum ()) [ 0; 2 ] with
    | Ok(text, _) ->
        Assert.Contains("xUncertain (fun r -> r.L)", text)
        Assert.Contains("yUncertain (fun r -> r.T)", text)
    | Error why -> failwith why

// ---- In the worker, the editor and plain dotnet fsi ------------------------------------------------

let private command: Command =
    { File = Path.Combine(AppContext.BaseDirectory, (if OperatingSystem.IsWindows() then "tambour.exe" else "tambour"))
      Arguments = [] }

let private init: Init =
    { References = [||]
      Prelude = Shell.prelude
      Skia = { Dpi = float Marey.Space.screenDpi; FontFiles = [||] } }

let private script =
    "#load \"prelude.fsx\"\nopen Marey\nopen Marey.UnitsOfMeasure\nopen Prelude\n\n"
    + "let row = pendulum.[0]\n"
    + "let g = 4.0 * System.Math.PI ** 2.0 * row.L / (row.T * row.T)\n"
    + "let difference = pendulum.[0].L - pendulum.[1].L\n"
    + "let fig =\n    plot pendulum {\n        xUncertain (fun r -> r.L)\n        yUncertain (fun r -> r.T)\n        layer GeomPoint\n    }\n\n"
    + "printfn \"g %s; L0 - L1 %s\" (Uncertain.report g) (Uncertain.report difference)\n"

let private project () =
    let dir = (Directory.CreateTempSubdirectory "tambour-uncertainty").FullName
    Project.makeWorkspace dir
    let t = pendulum ()
    Csv.writeFile (Path.Combine(dir, "tables", "pendulum.csv")) t
    Tables.writePrelude dir [ { Table = t; File = "tables/pendulum.csv" } ] |> ignore
    File.WriteAllText(Path.Combine(dir, "figure.fsx"), script)
    dir, t

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

let private printed (seen: Reply list) =
    seen |> List.choose (function Reply.Output o -> Some o.Text | _ -> None) |> String.concat ""

[<Fact>]
let ``a script reads uncertain columns in the worker, sees a shared part cancel, and a bound value's budget`` () : Task =
    task {
        let dir, table = project ()
        let path = Path.Combine(dir, "figure.fsx")

        // The editor checks it clean.
        let service = Analysis.Service { References = [||]; Prelude = Shell.prelude }
        Assert.DoesNotContain(service.Check({ Id = 1; Text = script; Path = path }).Diagnostics, fun d -> d.Severity = "error")

        use workers = new Workers(command, init)
        do! workers.StartAsync()
        do! workers.SendAsync(Tables.request table)
        do! workers.SendAsync(Request.Evaluate { Text = script; Path = path })
        let! e, seen = until workers 90.0 evaluated
        Assert.True(e.Ok, sprintf "%A" e)

        // The zero offset is shared, so it cancels in the difference: √2 × 1 mm/√12 is left.
        Assert.Contains("L0 - L1 0.20000(41) m", printed seen)
        Assert.Contains("g 9.811(39) m·s⁻²", printed seen)

        // The figure and the two uncertain values, each shown as its budget.
        Assert.Equal<string list>([ "difference"; "fig"; "g" ], e.Figures |> Array.map (fun f -> f.Name) |> Array.sort |> List.ofArray)

        // New lengths reach the script without the prelude compiled again, measured as before.
        let longer =
            { table with
                Columns = table.Columns |> List.mapi (fun i c -> if i = 0 then { c with Values = Floats [| 1200.0; 800.0; 600.0 |] } else c) }

        do! workers.SendAsync(Tables.request longer)
        do! workers.SendAsync(Request.Evaluate { Text = script; Path = path })
        let! e, seen = until workers 90.0 evaluated
        Assert.True(e.Ok, sprintf "%A" e)
        Assert.False(e.Prelude)
        Assert.Contains("L0 - L1 0.40000(41) m", printed seen)
    }

[<Fact>]
let ``the same script runs in plain dotnet fsi from the files alone`` () =
    let dir, _ = project ()
    let marey = Directory.GetFiles(AppContext.BaseDirectory, "Marey*.dll") |> Array.sort
    let info = ProcessStartInfo("dotnet", WorkingDirectory = dir, RedirectStandardOutput = true, RedirectStandardError = true)
    info.ArgumentList.Add "fsi"
    info.ArgumentList.Add "--quiet"
    for d in marey do info.ArgumentList.Add("-r:" + d)
    info.ArgumentList.Add "figure.fsx"
    use p = Process.Start info
    let out = p.StandardOutput.ReadToEndAsync()
    let err = p.StandardError.ReadToEnd()
    p.WaitForExit()
    Assert.True((p.ExitCode = 0), err)
    Assert.Contains("L0 - L1 0.20000(41) m", out.Result)

// ---- The budget view --------------------------------------------------------------------------------

[<Fact>]
let ``a budget's bars are Uncertain.budget's shares, in percent, a bar per input`` () =
    let l = Uncertain.resolution "L" U.metre 1.0<m> 0.001<m>
    let l2 = Uncertain.resolution "L" U.metre 0.8<m> 0.001<m>
    let t = Uncertain.standard "T" U.second 2.006<s> 0.004<s>
    let g = 4.0 * System.Math.PI ** 2.0 * (l + l2 - l2) / (t * t)
    let bars = Evaluation.Budgets.bars g
    let budget = Uncertain.budget g

    Assert.Equal(budget.Rows.Length, bars.Length)
    for (_, share), row in Array.zip bars budget.Rows do
        Assert.Equal(100.0 * row.Share, share, 12)

    Assert.Equal(100.0, bars |> Array.sumBy snd, 9)
    Assert.Equal("T", fst bars.[0])

    // Two inputs of one label are two bars.
    let both = l + l2
    Assert.Equal<string list>([ "L"; "L (2)" ], Evaluation.Budgets.bars both |> Array.map fst |> List.ofArray)

    // An exact value has no budget to show.
    Assert.Equal(None, Evaluation.Budgets.figure "x" (Uncertain.exact U.metre 1.0<m>))
    Assert.True((Evaluation.Budgets.figure "g" g).IsSome)
    Assert.Contains("95", Evaluation.Budgets.expanded g)
