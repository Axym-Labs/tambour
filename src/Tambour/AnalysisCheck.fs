/// `tambour --check-analysis <script>`: starts the language service cold, as the app does on
/// opening a script, and reports how long the script's first error took. The exit code is 0 when
/// an error arrived within 3 s. Run in a sandbox with no .NET (`tools/no-dotnet.sh`), it is what
/// shows the editor needs no SDK.
module Tambour.AnalysisCheck

open System
open System.Diagnostics
open System.IO
open Tambour.Protocol

/// Three seconds; a shared CI runner is held to three times that, as the timed tests are.
let private limit =
    if Environment.GetEnvironmentVariable "GITHUB_ACTIONS" = "true" then 9.0 else 3.0

let run (script: string) : int =
    try
        let text = File.ReadAllText script
        let clock = Stopwatch.StartNew()
        use languages = new LanguageService(Command.ofThisApp (), { References = [||]; Prelude = Shell.prelude })
        let checkedScript = languages.CheckAsync(text, Path.GetFullPath script).Result
        let elapsed = clock.Elapsed.TotalSeconds

        for d in checkedScript.Diagnostics do
            eprintfn "%s:%d:%d %s %s: %s" (Path.GetFileName script) d.StartLine (d.StartColumn + 1) d.Severity d.Code d.Message

        match checkedScript.Diagnostics |> Array.tryFind (fun d -> d.Severity = "error") with
        | Some _ when elapsed < limit ->
            eprintfn "holds  the first error in %.2f s (limit %g s)" elapsed limit
            0
        | Some _ ->
            eprintfn "FAILS  the first error in %.2f s (limit %g s)" elapsed limit
            1
        | None ->
            eprintfn "FAILS  no error in %.2f s; the script was meant to have one" elapsed
            1
    with e ->
        eprintfn "the check failed: %O" e
        1
