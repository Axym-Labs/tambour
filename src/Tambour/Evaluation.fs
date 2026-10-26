/// The worker's F# interactive session: a script run, and the figures it left bound.
module Tambour.Evaluation

open System
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Interactive.Shell
open Marey
open Tambour.Protocol

/// A figure a run bound, as the live figure the worker drives: a static one is a live figure with
/// no controls, so zoom, pan, hover and resize take one path for both.
type Found = { Name: string; Live: Live }

/// What a run came to.
type Run =
    { /// The figures this run bound, in binding order with `it` last. Empty when it failed.
      Found: Found list
      Diagnostics: Diagnostic[]
      /// The exception's message, when the script compiled and threw.
      Exception: string option
      /// Compiled and ran to the end.
      Ok: bool
      Elapsed: TimeSpan }

let private diagnostic (d: FSharpDiagnostic) : Diagnostic =
    { Severity =
        match d.Severity with
        | FSharpDiagnosticSeverity.Error -> "error"
        | FSharpDiagnosticSeverity.Warning -> "warning"
        | _ -> "info"
      Code = d.ErrorNumberText
      Message = d.Message
      StartLine = d.StartLine
      StartColumn = d.StartColumn
      EndLine = d.EndLine
      EndColumn = d.EndColumn }

// ---- #r "nuget: …" without an SDK -----------------------------------------------------------

let private nugetReference = Regex(@"^[ \t]*#r[ \t]+""nuget:", RegexOptions.Multiline)

/// Where a .NET installation may be: `DOTNET_ROOT`, the `dotnet` on the path, and the usual
/// places a package manager or the installer puts one.
let private dotnetRoots () =
    [ yield Environment.GetEnvironmentVariable "DOTNET_ROOT"

      let exe = if OperatingSystem.IsWindows() then "dotnet.exe" else "dotnet"

      for dir in (Environment.GetEnvironmentVariable "PATH" |> Option.ofObj |> Option.defaultValue "").Split(Path.PathSeparator) do
          if not (String.IsNullOrWhiteSpace dir) then
              let candidate = FileInfo(Path.Combine(dir, exe))

              if candidate.Exists then
                  match (try candidate.ResolveLinkTarget true with _ -> null) with
                  | null -> yield candidate.DirectoryName
                  | target -> yield Path.GetDirectoryName target.FullName

      if OperatingSystem.IsWindows() then
          yield Path.Combine(Environment.GetFolderPath Environment.SpecialFolder.ProgramFiles, "dotnet")
      else
          yield! [ "/usr/share/dotnet"; "/usr/lib/dotnet"; "/usr/local/share/dotnet"; "/opt/dotnet" ]
          yield Path.Combine(Environment.GetFolderPath Environment.SpecialFolder.UserProfile, ".dotnet") ]
    |> List.filter (String.IsNullOrWhiteSpace >> not)

/// Whether a .NET SDK is installed: some root above has a version under `sdk/`.
let sdkInstalled () : bool =
    dotnetRoots ()
    |> List.exists (fun root ->
        let sdk = Path.Combine(root, "sdk")
        Directory.Exists sdk && Directory.EnumerateDirectories(sdk) |> Seq.isEmpty |> not)

/// The sentence a failed run gets in place of NuGet's own error when the script references a
/// package and no SDK is installed to restore it. A run that did not fail, or that references no
/// package, keeps its diagnostics.
let explainNuget (text: string) (sdk: bool) (ok: bool) (diagnostics: Diagnostic[]) : Diagnostic[] =
    if ok || sdk || isNull text then
        diagnostics
    else
        let m = nugetReference.Match text

        if not m.Success then
            diagnostics
        else
            let line = text.Substring(0, m.Index).Split('\n').Length
            let lineText = text.Split('\n').[line - 1].TrimEnd('\r')

            [| { Severity = "error"
                 Code = ""
                 Message =
                   "#r \"nuget: …\" needs the .NET SDK to fetch a package, and this machine has none. "
                   + "Install it from https://dotnet.microsoft.com/download, then run the script again."
                 StartLine = line
                 StartColumn = lineText.Length - lineText.TrimStart().Length
                 EndLine = line
                 EndColumn = lineText.Length } |]

// ---- The session ----------------------------------------------------------------------------

/// The script the worker runs once after the prelude, so that the first script the user runs
/// does not pay for compiling a plot: Marey's builders and layout are compiled here.
let private warmup =
    """
module __TambourWarmup =
    open Marey
    open Marey.UnitsOfMeasure
    open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
    type Row = { Time: float<s>; Displacement: float<m>; Sample: string }
    let rows = Array.init 200 (fun i -> let t = float i * 0.01<s> in { Time = t; Displacement = 0.5 * 9.81<m/s^2> * t * t; Sample = string (i % 3) })
    let figure =
        plot rows {
            x U.second (fun r -> r.Time)
            y U.metre (fun r -> r.Displacement)
            color (fun r -> r.Sample)
            layer GeomPoint
        }
"""

/// The value of a binding as a live figure, if it is a figure.
let private asLive (value: obj) : Live option =
    match value with
    | :? Figure as f -> Some(Live.ofControl (Control.constant f))
    | :? Live as l -> Some l
    | _ -> None

/// An F# interactive session over Marey's assemblies and the stated references. Output the
/// script writes goes to the console, which the worker points at the host; the session's own
/// echo is off, and its errors come back as diagnostics.
type Session(references: string seq) =
    let marey =
        Directory.GetFiles(AppContext.BaseDirectory, "Marey*.dll") |> Array.sort

    let argv =
        Array.append
            [| "fsi.exe"; "--noninteractive"; "--nologo"; "--quiet"; "--gui-"; "--optimize+" |]
            (Seq.append marey references |> Seq.map (fun r -> "-r:" + r) |> Array.ofSeq)

    let fsi =
        FsiEvaluationSession.Create(
            FsiEvaluationSession.GetDefaultConfiguration(),
            argv,
            new StringReader(""),
            TextWriter.Null,
            TextWriter.Null,
            collectible = false
        )

    let bound () =
        fsi.GetBoundValues()
        |> List.map (fun b -> b.Name, b.Value.ReflectionValue)

    /// Evaluates a script. A figure is what this run bound: a name it bound anew, or bound to a
    /// different value than before. A figure a run no longer binds is therefore no longer
    /// found, though the session still holds it.
    member _.Run(text: string, path: string) : Run =
        let clock = Stopwatch.StartNew()
        let before = bound () |> dict
        let path = if String.IsNullOrEmpty path then "script.fsx" else path
        let result, diagnostics = fsi.EvalInteractionNonThrowing(text, path)
        let diagnostics = diagnostics |> Array.map diagnostic

        let compiled = diagnostics |> Array.forall (fun d -> d.Severity <> "error")

        // A script that does not compile also comes back as an exception, FSI's sentence that
        // the errors stopped it, which says nothing the diagnostics do not.
        let exn =
            match result with
            | Choice2Of2 e when compiled -> Some e.Message
            | _ -> None

        let ok = compiled && (match result with Choice1Of2 _ -> true | Choice2Of2 _ -> false)

        let found =
            if not ok then
                []
            else
                let fresh =
                    bound ()
                    |> List.filter (fun (name, value) ->
                        not (isNull value)
                        && (match before.TryGetValue name with
                            | true, old -> not (obj.ReferenceEquals(old, value))
                            | _ -> true))
                    |> List.choose (fun (name, value) -> asLive value |> Option.map (fun l -> { Name = name; Live = l }))

                let it, named = fresh |> List.partition (fun f -> f.Name = "it")
                named @ it

        { Found = found
          Diagnostics = explainNuget text (ok || sdkInstalled ()) ok diagnostics
          Exception = exn
          Ok = ok
          Elapsed = clock.Elapsed }

    /// Evaluates the prelude, then the warm-up, and hands back the warm-up's figure for the
    /// worker to lay out once. Throws when either fails, since a worker without its prelude is
    /// no worker.
    member _.Start(prelude: string) : Live =
        let evaluate (label: string) (code: string) =
            if not (String.IsNullOrWhiteSpace code) then
                match fsi.EvalInteractionNonThrowing(code, label) with
                | Choice2Of2 e, ds ->
                    let messages = ds |> Array.map (fun d -> d.ToString())
                    failwithf "the %s failed: %s %s" label e.Message (String.Join(" ", messages))
                | _ -> ()

        evaluate "prelude" prelude
        evaluate "warm-up" warmup

        match fsi.EvalExpressionNonThrowing "__TambourWarmup.figure" with
        | Choice1Of2(Some v), _ ->
            match asLive v.ReflectionValue with
            | Some l -> l
            | None -> failwith "the warm-up's figure is not a figure"
        | _ -> failwith "the warm-up bound no figure"
