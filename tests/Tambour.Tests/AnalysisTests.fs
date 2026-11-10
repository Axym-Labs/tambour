/// The language service: what the editor is told about a script.
[<Xunit.Collection("workers")>]
module Tambour.Tests.AnalysisTests

open System
open System.Diagnostics
open System.IO
open Tambour
open Tambour.Protocol
open Xunit

let private init: AnalysisInit = { References = [||]; Prelude = Shell.prelude }

/// One service for the tests that ask it in this process.
let private service = lazy (Analysis.Service init)

let private unitError = File.ReadAllText(Path.Combine(AppContext.BaseDirectory, "scripts", "unit-error.fsx"))

let private at (text: string) (line: int) (column: int) : At =
    { Id = 1
      Text = text
      Path = null
      Line = line
      Column = column }

/// Where `marker` first is in `text`, as a line from 1 and a column from 0, plus `shift` columns.
let private find (text: string) (marker: string) (shift: int) =
    let lines = text.Split('\n')
    let line = lines |> Array.findIndex (fun l -> l.Contains marker)
    line + 1, lines.[line].IndexOf marker + shift

[<Fact>]
let ``the unit error is where the script has it, in short units`` () =
    let checkedScript = service.Value.Check { Id = 7; Text = unitError; Path = null }
    Assert.Equal(7, checkedScript.Id)
    let error = checkedScript.Diagnostics |> Array.find (fun d -> d.Severity = "error")
    let line, column = find unitError "r.Displacement" 0
    Assert.Equal("FS0001", error.Code)
    Assert.Equal(line, error.StartLine)
    Assert.Equal(column, error.StartColumn)
    Assert.Contains("float<s>", error.Message)
    Assert.Contains("float<m>", error.Message)
    Assert.DoesNotContain("UnitSymbols", error.Message)

[<Fact>]
let ``a script that compiles has no errors, and the prelude's lines are not counted`` () =
    let fixedScript = unitError.Replace("y U.second (fun r -> r.Displacement)", "y U.metre (fun r -> r.Displacement)")
    let ok = service.Value.Check { Id = 1; Text = fixedScript; Path = null }
    Assert.DoesNotContain(ok.Diagnostics, fun d -> d.Severity = "error")
    let first = service.Value.Check { Id = 2; Text = "let a: int = \"x\"\n"; Path = null }
    Assert.Equal(1, (first.Diagnostics |> Array.find (fun d -> d.Severity = "error")).StartLine)

/// A directive's reference to an assembly the app supplies gives way to the app's; any other stays.
[<Fact>]
let ``a script's reference to an assembly the app supplies is dropped, and no other`` () =
    let own = [| "/app/FSharp.Core.dll"; "/app/Marey.Plot.dll"; "/app/Marey.Units.dll" |]

    let options =
        [| "--define:TAMBOUR"
           "-r:/home/u/.nuget/packages/marey.plot/0.7.0/lib/netstandard2.1/Marey.Plot.dll"
           "-r:/home/u/.nuget/packages/marey.units/0.7.0/lib/netstandard2.1/marey.units.dll"
           "-r:/home/u/.nuget/packages/newtonsoft.json/13.0.3/lib/net6.0/Newtonsoft.Json.dll" |]

    Assert.Equal<string[]>(
        [| "--define:TAMBOUR"; "-r:/home/u/.nuget/packages/newtonsoft.json/13.0.3/lib/net6.0/Newtonsoft.Json.dll" |],
        Analysis.withoutShadowed own options
    )

/// `#r "nuget: Marey.Sim"` restores the newest Marey on nuget.org; the script is still checked
/// against the app's, as the worker runs it, so names newer than that release are known.
[<Fact>]
let ``a script that fetches Marey from nuget is checked against the app's Marey`` () =
    if not (Tambour.Evaluation.sdkInstalled ()) then
        Assert.Skip "no .NET SDK, so no package restore"

    let script =
        "#r \"nuget: Marey.Sim\"\n"
        + "let energy = U.joule\n"
        + "open Marey.UnitsOfMeasure\n"
        + "let figure = Chart.lineIn U.radian U.second [| 1.0<rad> |] [| 2.0<s> |]\n"

    let path = Path.Combine(Path.GetTempPath(), "tambour-nuget-marey.fsx")
    let answer = service.Value.Check { Id = 3; Text = script; Path = path }
    let errors = answer.Diagnostics |> Array.filter (fun d -> d.Severity = "error")
    Assert.True(errors.Length = 0, String.Join("\n", errors |> Array.map (fun d -> $"{d.Code} {d.Message}")))

[<Fact>]
let ``completion after a row's dot offers its columns`` () =
    let text = unitError.Replace("        layer GeomPoint\n", "        color (fun r -> r.)\n        layer GeomPoint\n")
    let line, column = find text "r.)" 2
    let names = (service.Value.Complete(at text line column)).Items |> Array.map (fun i -> i.Name)
    Assert.Contains("Time", names)
    Assert.Contains("Displacement", names)
    Assert.Contains("Sample", names)

[<Fact>]
let ``hover on a column says its unit`` () =
    let line, column = find unitError "r.Time" 3
    let hovered = service.Value.Hover(at unitError line column)
    Assert.Contains("Time", hovered.Text)
    Assert.Contains("float<s>", hovered.Text)

[<Fact>]
let ``signature help names the overloads and the parameter the place is at`` () =
    let text = "let m = System.Math.Max(1.0, \n"
    let signatures = service.Value.Signature(at text 1 (text.IndexOf ", " + 2))
    Assert.NotEmpty signatures.Methods
    Assert.All(signatures.Methods, fun m -> Assert.StartsWith("Max(", m))
    Assert.Equal(1, signatures.Parameter)

[<Fact>]
let ``uses of a binding the script makes are found, and it can be renamed; a library's cannot`` () =
    let line, column = find unitError "plot trials" 6
    let used = service.Value.Uses(at unitError line column)
    Assert.Equal("trials", used.Name)
    Assert.True used.Renamable
    Assert.Equal(2, used.Spans.Length)

    for s in used.Spans do
        let text = unitError.Split('\n').[s.StartLine - 1]
        Assert.Equal("trials", text.Substring(s.StartColumn, s.EndColumn - s.StartColumn))

    let line, column = find unitError "U.second" 3
    let library = service.Value.Uses(at unitError line column)
    Assert.Equal("second", library.Name)
    Assert.False library.Renamable

/// The line a definition is on in the source the app carries, which must name it there.
let private carriedLine (d: Defined) =
    let text = Sources.read d.File |> Option.defaultWith (fun () -> failwith $"{d.File} is not carried")
    text.Split('\n').[d.Line - 1]

[<Fact>]
let ``go to definition finds a binding the script makes in the script`` () =
    let line, column = find unitError "plot trials" 6
    let defined = service.Value.Definition(at unitError line column)
    Assert.Equal("script", defined.Kind)
    let definedLine, definedColumn = find unitError "let trials" 4
    Assert.Equal(definedLine, defined.Line)
    Assert.Equal(definedColumn, defined.Column)

[<Fact>]
let ``go to definition finds Marey's names in the source the app carries`` () =
    let line, column = find unitError "U.second" 3
    let defined = service.Value.Definition(at unitError line column)
    Assert.Equal("source", defined.Kind)
    Assert.StartsWith("marey/src/", defined.File)
    Assert.Contains("second", carriedLine defined)

[<Fact>]
let ``go to definition finds FSharp.Core's names in the source the app carries`` () =
    let text = "let xs = List.map string [ 1; 2 ]\n"
    let defined = service.Value.Definition(at text 1 (text.IndexOf "map" + 1))
    Assert.Equal("source", defined.Kind)
    Assert.Equal("fsharp-core/src/FSharp.Core/list.fsi", defined.File)
    Assert.Contains("map", carriedLine defined)
    Assert.Contains("FSharp.Core", Sources.describe defined.File)

[<Fact>]
let ``go to definition names what it cannot show, .NET's own`` () =
    let text = "let m = System.Math.Max(1.0, 2.0)\n"
    let defined = service.Value.Definition(at text 1 (text.IndexOf "Max" + 1))
    Assert.Equal("none", defined.Kind)
    Assert.Contains("Max", defined.Name)
    Assert.NotEmpty defined.Assembly
    let nothing = service.Value.Definition(at text 1 1)
    Assert.Equal("none", nothing.Kind)

[<Fact>]
let ``a carried Marey file is checked in its project: its names are classified, hovered and gone to`` () =
    let path = "marey/src/Marey.Plot/UnitsOfMeasure.fs"
    let text = (Sources.read path).Value
    let lines = text.Split('\n')
    let line = (lines |> Array.findIndex (fun l -> l.Contains "let scatterIn")) + 1
    let clock = Stopwatch.StartNew()
    let checkedFile = service.Value.Check { Id = 3; Text = text; Path = path }
    let elapsed = clock.Elapsed.TotalSeconds
    Assert.Empty checkedFile.Diagnostics

    // `U` in `(ux: U<'ux>)` is a type, as the script's editor draws a Marey type.
    let u = lines.[line - 1].IndexOf "U<'ux>"
    let kindAt column = checkedFile.Tokens |> Array.tryFind (fun t -> t.Line = line && t.StartColumn <= column && column < t.EndColumn) |> Option.map (fun t -> t.Kind)
    Assert.True((kindAt u).IsSome, $"U has no class (checked in {elapsed:F1} s)")
    Assert.NotEqual(Some "variable", kindAt u)

    let hovered = service.Value.Hover { Id = 4; Text = text; Path = path; Line = line; Column = u + 1 }
    Assert.Contains("U", hovered.Text)

    // `Marey.Chart.scatter` on the next lines goes to Chart.fs, carried too.
    let call = (lines |> Array.findIndex (fun l -> l.Contains "Marey.Chart.scatter ux")) + 1
    let column = lines.[call - 1].IndexOf "scatter ux" + 2
    let defined = service.Value.Definition { Id = 5; Text = text; Path = path; Line = call; Column = column }
    Assert.Equal("source", defined.Kind)
    Assert.Equal("marey/src/Marey.Plot/Chart.fs", defined.File)
    Assert.Contains("scatter", carriedLine defined)

[<Fact>]
let ``an attribute's name is drawn whole as its type`` () =
    let text = "[<Literal>]\nlet private MaxStreak = 10_000\n"
    let checkedScript = service.Value.Check { Id = 9; Text = text; Path = null }
    let first = checkedScript.Tokens |> Array.filter (fun t -> t.Line = 1)
    Assert.Equal<(int * int * string) list>([ 2, 9, "class" ], first |> Array.map (fun t -> t.StartColumn, t.EndColumn, t.Kind) |> List.ofArray)

/// A shared runner is held to three times the limit, as the timed tests are.
let private limit (seconds: float) =
    if Environment.GetEnvironmentVariable "GITHUB_ACTIONS" = "true" then 3.0 * seconds else seconds

/// The unit error is shown within 3 s of the app starting its language service, cold. (On a
/// machine with no .NET: the same, in the sandbox `tools/no-dotnet.sh` makes.)
[<Fact>]
let ``a cold language service reports the unit error within three seconds`` () =
    let command: Command =
        { File = Path.Combine(AppContext.BaseDirectory, (if OperatingSystem.IsWindows() then "tambour.exe" else "tambour"))
          Arguments = [] }

    // Other processes of the app still running would share the CPU with this one: none should be.
    let others = Process.GetProcessesByName "tambour" |> Array.length
    let clock = Stopwatch.StartNew()
    use languages = new LanguageService(command, init)
    languages.Ready.Wait()
    let ready = clock.Elapsed.TotalSeconds
    let checkedScript = languages.CheckAsync(unitError, null).Result
    let elapsed = clock.Elapsed.TotalSeconds
    let measured = $"the unit error took {elapsed:F2} s, {ready:F2} s of it to start; {others} other processes of the app ran"
    TestContext.Current.TestOutputHelper.WriteLine measured
    Assert.Contains(checkedScript.Diagnostics, fun d -> d.Code = "FS0001")
    Assert.True(elapsed < limit 3.0, measured)

[<Fact>]
let ``a check classifies the script's names, in the script's own lines`` () =
    let checkedScript = service.Value.Check { Id = 1; Text = unitError; Path = null }
    let lines = unitError.Split('\n')
    let named (t: Token) = lines.[t.Line - 1].Substring(t.StartColumn, t.EndColumn - t.StartColumn)
    let kindOf (name: string) = checkedScript.Tokens |> Array.filter (fun t -> named t = name) |> Array.map (fun t -> t.Kind) |> Array.distinct

    Assert.Contains("type", Array.append (kindOf "Trial") (kindOf "float"))
    Assert.Equal<string[]>([| "variable" |], kindOf "trials")
    Assert.Equal<string[]>([| "property" |], kindOf "Displacement")
    Assert.Contains("function", kindOf "sprintf")

    // In order, and none overlapping.
    for a, b in Array.pairwise checkedScript.Tokens do
        Assert.True((a.Line, a.EndColumn) <= (b.Line, b.StartColumn), $"{a} and {b}")

[<Fact>]
let ``hover on a library's name shows its documentation`` () =
    // FSharp.Core's, and Marey's.
    for marker, shift in [ "Array.init", 7; "x U.second", 0 ] do
        let line, column = find unitError marker shift
        let hovered = service.Value.Hover(at unitError line column)
        TestContext.Current.TestOutputHelper.WriteLine $"{hovered.Text}\n---\n{hovered.Doc}\n"
        Assert.False(String.IsNullOrWhiteSpace hovered.Doc, $"no documentation for {marker}")

[<Fact>]
let ``documentation reads as text`` () =
    let xml = """<summary>Draws <c>points</c>; see <see cref="T:Marey.Plot.Geom"/>.<para>Second   paragraph.</para></summary><param name="x">ignored</param>"""
    Assert.Equal("Draws `points`; see Geom.\n\nSecond paragraph.", Analysis.readable xml)

    // Markdown as Marey writes it: its lines, less their indent, a code block's spaces kept.
    let markdown = "<summary>\n **Bold** words,\n two   lines.\n\n ```fsharp\n let x =\n     1\n ```\n</summary>"
    Assert.Equal("**Bold** words,\ntwo lines.\n\n```fsharp\nlet x =\n    1\n```", Analysis.readable markdown)

/// The prelude Tambour writes for a project's tables opens with `module Prelude`: checked as it
/// is, nothing is in front of the module, and it has no problems.
[<Fact>]
let ``the project's prelude is checked as its own module, without problems`` () =
    let table: Tambour.Data.Table =
        { Name = "trial"
          Columns =
            [ { Name = "t"; Unit = "s"; Values = Tambour.Data.Floats [| 0.0; 1.0 |] }
              { Name = "test_results.AUC"; Unit = null; Values = Tambour.Data.Floats [| 0.9; 0.95 |] } ] }

    let prelude = Tambour.Data.Prelude.generate [ { Tambour.Data.Prelude.Table = table; Tambour.Data.Prelude.File = "trial.csv" } ]
    Assert.True(Analysis.ownsModule prelude)
    let checkedPrelude = service.Value.Check { Id = 1; Text = prelude; Path = null }
    Assert.Empty(checkedPrelude.Diagnostics |> Array.filter (fun d -> d.Severity = "error"))

    // An error in such a script is still on its own line.
    let broken = "// a module of its own\nmodule Mine\n\nlet x: int = \"one\"\n"
    let error = (service.Value.Check { Id = 2; Text = broken; Path = null }).Diagnostics |> Array.find (fun d -> d.Severity = "error")
    Assert.Equal(4, error.StartLine)
    Assert.False(Analysis.ownsModule "module Nested =\n    let x = 1\n")

/// A script beside the project's prelude completes its tables by name, whether it loads the
/// prelude itself or is given it; a script elsewhere cannot see them.
[<Fact>]
let ``a script beside the prelude completes a table's name, whether it loads it or not`` () =
    let dir = (Directory.CreateTempSubdirectory "tambour-complete").FullName

    let table: Tambour.Data.Table =
        { Name = "pretrained"
          Columns = [ { Name = "soma_rmse_mv"; Unit = null; Values = Tambour.Data.Floats [| 3.2; 3.5 |] } ] }

    File.WriteAllText(Path.Combine(dir, "prelude.fsx"), Tambour.Data.Prelude.generate [ { Tambour.Data.Prelude.Table = table; Tambour.Data.Prelude.File = "pretrained.csv" } ])
    let path = Path.Combine(dir, "figure.fsx")

    let namesAt (text: string) =
        let lines = text.Split('\n')
        (service.Value.Complete { Id = 1; Text = text; Path = path; Line = lines.Length; Column = lines.[lines.Length - 1].Length }).Items
        |> Array.map (fun i -> i.Name)

    Assert.Contains("pretrained", namesAt "#load \"prelude.fsx\"\nopen Prelude\n\nlet a = pre")
    Assert.Contains("pretrained", namesAt "let a = pre")
    let elsewhere = Path.Combine((Directory.CreateTempSubdirectory "tambour-alone").FullName, "figure.fsx")
    let lines = "let a = pre"
    Assert.DoesNotContain("pretrained", (service.Value.Complete { Id = 2; Text = lines; Path = elsewhere; Line = 1; Column = lines.Length }).Items |> Array.map (fun i -> i.Name))

/// A completion list's entry is described as its hover would be: its signature and its
/// documentation; a name the list does not hold has nothing.
[<Fact>]
let ``a completion entry is described by its signature and documentation`` () =
    let text = "/// Twice the number given.\nlet doubled (x: float) = 2.0 * x\nlet a = dou"
    let entry name : Entry = { Id = 4; Text = text; Path = null; Line = 3; Column = "let a = dou".Length; Name = name }
    let described = service.Value.Describe(entry "doubled")
    Assert.Equal(4, described.Id)
    Assert.Contains("doubled", described.Text)
    Assert.Contains("float", described.Text)
    Assert.Contains("Twice the number given.", described.Doc)
    Assert.Equal("", (service.Value.Describe(entry "nothingLikeThis")).Text)

// ---- Marey's witness fixes -------------------------------------------------------------------------

/// Under the prelude an axis takes a unit only where the script gives one.
[<Fact>]
let ``an axis with no unit beside one with a unit checks clean`` () =
    let script =
        "type Row = { Index: int; Height: float<m> }\n"
        + "let rows = [| { Index = 0; Height = 1.0<m> }; { Index = 1; Height = 2.0<m> } |]\n"
        + "let fig =\n    plot rows {\n        x (fun r -> float r.Index)\n        y U.metre (fun r -> r.Height)\n        layer GeomPoint\n    }\n"
        + "let plain = Chart.Line([| 0.0; 1.0 |], [| 2.0; 3.0 |])\n"

    let found = service.Value.Check { Id = 1; Text = script; Path = null }
    let errors = found.Diagnostics |> Array.filter (fun d -> d.Severity = "error")
    Assert.True(errors.Length = 0, String.Join("\n", errors |> Array.map (fun d -> $"{d.Code} {d.Message}")))
    Assert.Empty found.Findings

let private measured = "let t = [| 0.0<s>; 1.0<s>; 2.0<s> |]\nlet x = [| 0.0<m>; 2.0<m>; 3.0<m> |]\n"

/// A call short of the right witnesses is found where the script has it, and its first fix makes
/// the call this one; each of its fixes makes the script check clean. The calls on measured
/// arrays are the unit-required mode's, which the script opens itself.
[<Theory>]
[<InlineData("Chart.lineIn t x", "MAREY001", "Chart.lineIn U.second U.metre t x")>]
[<InlineData("Chart.lineIn U.metre U.metre t x", "MAREY002", "Chart.lineIn U.second U.metre t x")>]
[<InlineData("Chart.line t x", "MAREY003", "Chart.lineIn U.second U.metre t x")>]
let ``a call short of its witnesses is found, and its fixes check clean`` (call: string, code: string, first: string) =
    let before = "open Marey.UnitsOfMeasure\n" + measured
    let script = before + $"let figure = {call}\n"
    let found = service.Value.Check { Id = 1; Text = script; Path = null }
    let finding = found.Findings |> Array.find (fun f -> f.Code = code)
    Assert.Equal(4, finding.StartLine)
    Assert.Equal("let figure = ".Length, finding.StartColumn)
    Assert.NotEmpty finding.Fixes
    Assert.Equal(before + $"let figure = {first}\n", Analysis.applyFix script finding.Fixes.[0])

    for fix in finding.Fixes do
        let fixedScript = Analysis.applyFix script fix
        let after = service.Value.Check { Id = 2; Text = fixedScript; Path = null }
        Assert.True(after.Diagnostics |> Array.forall (fun d -> d.Severity <> "error"), $"{fix.Title}: {fixedScript}\n%A{after.Diagnostics}")
        Assert.Empty after.Findings

/// A finding the prelude's lines hold is not the script's, nor a fix that would edit them.
[<Fact>]
let ``a finding or a fix in the prelude is dropped`` () =
    let range (line: int) = FSharp.Compiler.Text.Range.mkRange "s.fsx" (FSharp.Compiler.Text.Position.mkPos line 4) (FSharp.Compiler.Text.Position.mkPos line 9)
    let edit line : Marey.Witnesses.Edit = { Range = range line; Text = "U.second " }

    let findings: Marey.Witnesses.Finding list =
        [ { Code = "MAREY001"; Range = range 2; Message = "in the prelude"; Fixes = [ { Title = "a"; Edits = [ edit 2 ] } ] }
          { Code = "MAREY001"
            Range = range 5
            Message = "in the script"
            Fixes = [ { Title = "kept"; Edits = [ edit 5; edit 6 ] }; { Title = "dropped"; Edits = [ edit 5; edit 3 ] } ] } ]

    let found = Analysis.findingsOf 3 findings
    let f = Assert.Single found
    Assert.Equal(2, f.StartLine)
    Assert.Equal(4, f.StartColumn)
    let fix = Assert.Single f.Fixes
    Assert.Equal("kept", fix.Title)
    Assert.Equal<int[]>([| 2; 3 |], fix.Edits |> Array.map (fun e -> e.StartLine))

/// A fix's edits are made each where it was before any of them, whatever their order; two at one
/// place go in in the fix's order, and an edit across lines takes them out.
[<Fact>]
let ``a fix's edits are made where they were, two at one place in their order`` () =
    let edit line column endLine endColumn text : FixEdit =
        { StartLine = line
          StartColumn = column
          EndLine = endLine
          EndColumn = endColumn
          Text = text }

    let script = "let a = f t x\nlet b =\n    g t\n"

    let fix: Fix =
        { Title = "both"
          Edits = [| edit 1 10 1 10 "U.second "; edit 2 7 3 4 " "; edit 1 10 1 10 "U.metre " |] }

    Assert.Equal("let a = f U.second U.metre t x\nlet b = g t\n", Analysis.applyFix script fix)

/// A script beside a table's prelude is checked with the tables before it, whether it is given
/// them or loads them itself; its finding is still on its own line, and the fix checks clean.
[<Theory>]
[<InlineData(false)>]
[<InlineData(true)>]
let ``a call on a table's columns is found on the script's line, and fixed`` (loads: bool) =
    let dir = (Directory.CreateTempSubdirectory "tambour-witness").FullName

    let table: Tambour.Data.Table =
        { Name = "runs"
          Columns =
            [ { Name = "time"; Unit = "s"; Values = Tambour.Data.Floats [| 0.0; 1.0 |] }
              { Name = "height"; Unit = "m"; Values = Tambour.Data.Floats [| 2.0; 3.0 |] } ] }

    File.WriteAllText(Path.Combine(dir, "prelude.fsx"), Tambour.Data.Prelude.generate [ { Tambour.Data.Prelude.Table = table; Tambour.Data.Prelude.File = "runs.csv" } ])
    let path = Path.Combine(dir, "figure.fsx")

    let script =
        (if loads then "#load \"prelude.fsx\"\nopen Prelude\n" else "")
        + "open Marey.UnitsOfMeasure\nlet t = runs |> Array.map (fun r -> r.time)\nlet x = runs |> Array.map (fun r -> r.height)\nlet figure = Chart.lineIn t x\n"

    let line = script.Split('\n') |> Array.findIndex (fun l -> l.StartsWith "let figure") |> (+) 1
    let found = service.Value.Check { Id = 1; Text = script; Path = path }
    let finding = found.Findings |> Array.find (fun f -> f.Code = "MAREY001")
    Assert.Equal(line, finding.StartLine)
    Assert.Equal("let figure = ".Length, finding.StartColumn)
    let fixedScript = Analysis.applyFix script finding.Fixes.[0]
    Assert.EndsWith("let figure = Chart.lineIn U.second U.metre t x\n", fixedScript)
    let after = service.Value.Check { Id = 2; Text = fixedScript; Path = path }
    Assert.True(after.Diagnostics |> Array.forall (fun d -> d.Severity <> "error"), $"%A{after.Diagnostics}")
    Assert.Empty after.Findings
