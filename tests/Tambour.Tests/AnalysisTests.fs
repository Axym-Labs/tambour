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
