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

    let clock = Stopwatch.StartNew()
    use languages = new LanguageService(command, init)
    let checkedScript = languages.CheckAsync(unitError, null).Result
    let elapsed = clock.Elapsed.TotalSeconds
    TestContext.Current.TestOutputHelper.WriteLine $"cold start to the unit error: {elapsed:F2} s"
    Assert.Contains(checkedScript.Diagnostics, fun d -> d.Code = "FS0001")
    Assert.True(elapsed < limit 3.0, $"the unit error took {elapsed:F2} s")

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
