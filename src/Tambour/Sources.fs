/// The source of the libraries a script names, as the app carries it: Marey's and FSharp.Core's,
/// each at the commit its package was built from (`tools/sources.fsx` makes the zip at build time).
/// Go to definition opens a name of theirs here, read-only.
module Tambour.Sources

open System
open System.IO
open System.IO.Compression
open System.Text.Json

type private Anchor =
    class
    end

let private archive =
    lazy
        (match typeof<Anchor>.Assembly.GetManifestResourceStream "Tambour.sources.zip" with
         | null -> None
         | s -> Some(new ZipArchive(s, ZipArchiveMode.Read)))

/// Every file carried, by its path in the zip.
let private entries =
    lazy
        (match archive.Value with
         | Some zip -> zip.Entries |> Seq.map (fun e -> e.FullName) |> Set.ofSeq
         | None -> Set.empty)

/// The text of a file carried, by its path in the zip.
let read (path: string) : string option =
    match archive.Value with
    | Some zip when entries.Value.Contains path ->
        // One reader at a time: a zip's entries share its stream.
        lock zip (fun () ->
            use r = new StreamReader(zip.GetEntry(path).Open())
            Some(r.ReadToEnd()))
    | _ -> None

/// Where in the zip an assembly's sources are, if the app carries them.
let private rootOf (assembly: string) =
    if assembly = "FSharp.Core" then Some "fsharp-core/"
    elif assembly = "Marey" || assembly.StartsWith "Marey." then Some "marey/"
    else None

/// The file carried for a path a library's assembly records for a definition (the path on the
/// machine that built it, such as `/_/src/Marey.Core/Dim.fs`): the carried file whose path ends
/// in the longest part of it.
let locate (assembly: string) (recorded: string) : string option =
    match rootOf assembly with
    | None -> None
    | Some root ->
        let parts = recorded.Replace('\\', '/').Split('/', StringSplitOptions.RemoveEmptyEntries)

        seq { 0 .. parts.Length - 1 }
        |> Seq.map (fun i -> root + String.Join("/", parts.[i..]))
        |> Seq.tryFind entries.Value.Contains

/// The library a carried file is of, and the version and commit carried, as a tab's tip says it.
let describe (path: string) : string =
    let library, key =
        if path.StartsWith "fsharp-core/" then "FSharp.Core", "fsharpCore" else "Marey", "marey"

    let version, commit =
        match read "sources.json" with
        | Some json ->
            try
                let e = JsonDocument.Parse(json).RootElement.GetProperty key
                e.GetProperty("version").GetString(), e.GetProperty("commit").GetString()
            with _ ->
                "", ""
        | None -> "", ""

    let at = if String.IsNullOrEmpty commit then "" else $" at {commit.Substring(0, min 7 commit.Length)}"
    let rest = path.Substring(path.IndexOf '/' + 1)
    $"{library} {version}{at}: {rest} (read-only)"

/// A carried file's name, as its tab shows it.
let nameOf (path: string) = Path.GetFileName path
