/// GUI actions as script edits: every edit leaves a script that compiles with no warning as a file,
/// runs, draws what the action asked, keeps its comments, and settles when done twice.
[<Xunit.Collection("workers")>]
module Tambour.Tests.EditsTests

open System
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Interactive.Shell
open FSharp.Compiler.Text
open Marey
open Tambour.Edits
open Tambour.Edits.Edits
open Xunit

let private marey = Directory.GetFiles(AppContext.BaseDirectory, "Marey*.dll") |> Array.sort

let private log (line: string) = TestContext.Current.TestOutputHelper.WriteLine line

/// Every comment in a script, as text.
let private comments (source: string) =
    Regex.Matches(source, @"//[^\n]*|\(\*.*?\*\)", RegexOptions.Singleline)
    |> Seq.map (fun m -> m.Value.TrimEnd())
    |> List.ofSeq

/// An F# interactive session over Marey and nothing of the app's: what `dotnet fsi` with Marey
/// referenced gives a script.
let private fsi =
    lazy
        (FsiEvaluationSession.Create(
            FsiEvaluationSession.GetDefaultConfiguration(),
            Array.append [| "fsi.exe"; "--noninteractive"; "--nologo"; "--quiet"; "--gui-" |] (marey |> Array.map (fun d -> "-r:" + d)),
            new StringReader(""),
            TextWriter.Null,
            TextWriter.Null,
            collectible = false
        ))

let private out = lazy (Directory.CreateTempSubdirectory "tambour-edits").FullName

let mutable private evaluated = 0

/// Runs a script as a file and then its figure, as a person running it would see it: Ok with the
/// figure's SVG, or Error with every error and warning.
let private evaluate (name: string) (source: string) =
    evaluated <- evaluated + 1
    let path = Path.Combine(out.Value, name)
    File.WriteAllText(path, source)
    let _, fileDiagnostics = fsi.Value.EvalScriptNonThrowing path
    let result, diagnostics = fsi.Value.EvalInteractionNonThrowing(source, path)

    let problems =
        Array.append fileDiagnostics diagnostics
        |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error || d.Severity = FSharpDiagnosticSeverity.Warning)

    match result, problems with
    | Choice1Of2 _, [||] ->
        match fsi.Value.EvalExpressionNonThrowing "fig" with
        | Choice1Of2(Some v), _ ->
            let extent = Chart.size 640.0 400.0
            Ok(SvgBackend.render extent (Chart.toScene extent (v.ReflectionValue :?> Figure)))
        | _ -> Error "no figure named fig"
    | _, ps -> Error(String.Join(" | ", ps |> Array.map (fun d -> $"{d.Severity} {d.StartLine}:{d.StartColumn} {d.Message}")))

/// The text an action must leave in the drawing, where there is text to check.
let private shows action =
    match action with
    | Title _ -> Some "λ"
    | XLabel _ -> Some "elapsed"
    | YLabel _ -> Some "height"
    | _ -> None

let private actionsFor (column: string) =
    [ Title "Free \"fall\" — λ"
      XLabel "elapsed"
      YLabel "height"
      YLog
      YDomain(0.0, 1e3)
      ThemeIs "presentation"
      Legend "LegendTopLeft"
      ColorBy column
      Facet column
      Fit
      AddLayer "GeomLine" ]

let private scripts = Path.Combine(AppContext.BaseDirectory, "scripts", "edits")

/// Gate: the spike's harness, five scripts and eleven actions, each alone and all in sequence.
[<Fact>]
let ``every action on the five scripts compiles as a file, draws, keeps its comments and settles`` () =
    evaluated <- 0
    let failures = ResizeArray()

    for file in Directory.GetFiles(scripts, "*.fsx") |> Array.sort do
        let name = Path.GetFileNameWithoutExtension file
        let original = File.ReadAllText file
        let kept = comments original

        match evaluate (name + ".orig.fsx") original with
        | Error e -> failures.Add $"{name}: the original fails: {e}"
        | Ok _ -> ()

        let actions = actionsFor (if name.StartsWith "4" then "Run" else "Sample")

        for action in actions do
            let edited, outcome = apply file "fig" action original

            match outcome with
            | Refused why -> log $"{name} {action}: refused ({why})"
            | _ ->
                match evaluate $"{name}.{evaluated}.fsx" edited with
                | Error e -> failures.Add $"{name} {action}: {e}"
                | Ok svg ->
                    match shows action with
                    | Some text when not (svg.Contains text) -> failures.Add $"{name} {action}: compiles, but '{text}' is not drawn"
                    | _ -> ()

                for c in kept do
                    if not (edited.Contains c) then
                        failures.Add $"{name} {action}: lost the comment {c}"

                if not (settlesTwice file "fig" action original) then
                    failures.Add $"{name} {action}: done twice is not done once"

                log $"{name} {action}: {outcome}"

        let all = actions |> List.fold (fun s a -> fst (apply file "fig" a s)) original

        match evaluate (name + ".all.fsx") all with
        | Error e -> failures.Add $"{name}, every action in sequence: {e}"
        | Ok _ -> ()

        for c in kept do
            if not (all.Contains c) then
                failures.Add $"{name}, every action in sequence: lost the comment {c}"

    Assert.True(failures.Count = 0, String.Join("\n", failures))
    Assert.Equal(53, evaluated)

/// The composed scripts in plain `dotnet fsi`, with Marey referenced and nothing of the app's.
[<Fact>]
let ``every action in sequence runs in plain dotnet fsi`` () =
    if not (Tambour.Evaluation.sdkInstalled ()) then
        Assert.Skip "no .NET SDK, so no dotnet fsi"

    for file in Directory.GetFiles(scripts, "*.fsx") |> Array.sort do
        let original = File.ReadAllText file
        let column = if Path.GetFileName(file).StartsWith "4" then "Run" else "Sample"
        let all = actionsFor column |> List.fold (fun s a -> fst (apply file "fig" a s)) original
        let path = Path.Combine(out.Value, "plain-" + Path.GetFileName file)
        File.WriteAllText(path, all + "\nprintfn \"%s\" fig.Title\n")
        let info = ProcessStartInfo("dotnet", RedirectStandardOutput = true, RedirectStandardError = true)
        info.ArgumentList.Add "fsi"
        info.ArgumentList.Add "--quiet"

        for d in marey do
            info.ArgumentList.Add("-r:" + d)

        info.ArgumentList.Add path
        use p = Process.Start info
        let stdout = p.StandardOutput.ReadToEndAsync()
        let stderr = p.StandardError.ReadToEnd()
        p.WaitForExit()
        Assert.True(p.ExitCode = 0, $"{Path.GetFileName file}: dotnet fsi exited {p.ExitCode}: {stderr}")
        Assert.Contains("λ", stdout.Result)

// ---- Every Marey example with a figure ------------------------------------------------------------

/// Where Marey's examples are: beside this checkout, or where CI puts them.
let private examples () =
    [ Environment.GetEnvironmentVariable "MAREY_EXAMPLES"
      Path.Combine(AppContext.BaseDirectory, "../../../../../../marey/examples")
      Path.Combine(AppContext.BaseDirectory, "../../../../../marey/examples") ]
    |> List.filter (fun d -> not (String.IsNullOrEmpty d) && Directory.Exists d)
    |> List.tryHead
    |> Option.map Path.GetFullPath

let private checker = lazy (FSharpChecker.Create())

/// A Marey example as this checkout compiles it: its `#r "nuget: Marey…"` lines, which would fetch
/// a published Marey, stand for the pinned one the harness references.
let private local (source: string) =
    Regex.Replace(source, @"^#r\s+""nuget:\s*Marey[^""]*""", "// Marey: referenced by the harness", RegexOptions.Multiline)

let private referenced =
    lazy
        (let runtime = Path.GetDirectoryName(typeof<obj>.Assembly.Location)

         [| for f in Directory.GetFiles(runtime, "*.dll") do
                let n = Path.GetFileName f

                if (n.StartsWith "System." || n = "netstandard.dll" || n = "mscorlib.dll") && not (n.Contains "Native") then
                    yield "-r:" + f
            yield "-r:" + Path.Combine(AppContext.BaseDirectory, "FSharp.Core.dll")
            for d in marey do
                yield "-r:" + d |])

/// Type-checks a script as a file: its errors, and the names of its top-level `Figure` values.
let private check (path: string) (source: string) =
    let options, _ =
        checker.Value.GetProjectOptionsFromScript(
            path,
            SourceText.ofString source,
            assumeDotNetFramework = false,
            useSdkRefs = false,
            otherFlags = [| "--targetprofile:netcore"; "--simpleresolution"; "--noframework" |]
        )
        |> Async.RunSynchronously

    let options = { options with OtherOptions = Array.append options.OtherOptions referenced.Value }
    let _, answer = checker.Value.ParseAndCheckFileInProject(path, source.GetHashCode(), SourceText.ofString source, options) |> Async.RunSynchronously

    match answer with
    | FSharpCheckFileAnswer.Succeeded r ->
        let errors = r.Diagnostics |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)

        let figures =
            [ for e in r.PartialAssemblySignature.Entities do
                  for v in e.MembersFunctionsAndValues do
                      if v.IsModuleValueOrMember && v.CurriedParameterGroups.Count = 0 && not v.IsCompilerGenerated then
                          let t = v.FullType
                          if t.HasTypeDefinition && t.TypeDefinition.TryFullName = Some "Marey.Figure" then
                              v.DisplayName ]

        errors, figures, r
    | FSharpCheckFileAnswer.Aborted -> failwith $"{path}: the check was aborted"

/// Gate: on every Marey example that compiles and binds a figure, no action breaks it.
[<Fact>]
let ``no action breaks a Marey example that compiled`` () =
    match examples () with
    | None -> Assert.Skip "no checkout of marey's examples beside this one, nor MAREY_EXAMPLES"
    | Some dir ->
        let failures = ResizeArray()
        let mutable figuresSeen = 0
        let mutable edits = 0

        for file in Directory.GetFiles(dir, "*.fsx") |> Array.sort do
            let source = local (File.ReadAllText file)
            let errors, figures, results = check file source

            if errors.Length > 0 then
                log $"{Path.GetFileName file}: does not compile here, skipped ({errors.[0].Message.Split('\n').[0]})"
            elif figures.IsEmpty then
                log $"{Path.GetFileName file}: binds no Figure"
            else
                for figure in figures do
                    figuresSeen <- figuresSeen + 1

                    // Colour and facet by a column the rows have, as the panel offers them.
                    let column =
                        blockRange file figure source
                        |> Option.map (columns results)
                        |> Option.bind List.tryHead
                        |> Option.defaultValue "NoSuchColumn"

                    for action in actionsFor column do
                        let edited, outcome = apply file figure action source

                        match outcome with
                        | Refused _ -> ()
                        | _ ->
                            edits <- edits + 1
                            let errors, _, _ = check file edited

                            if errors.Length > 0 then
                                failures.Add $"{Path.GetFileName file} {figure} {action}: {errors.[0].StartLine}:{errors.[0].StartColumn} {errors.[0].Message}"

                log $"{Path.GetFileName file}: {figures.Length} figures"

        log $"{figuresSeen} figures, {edits} edits checked"
        Assert.True(figuresSeen > 0, "no example bound a figure")
        Assert.True(failures.Count = 0, String.Join("\n", failures))
