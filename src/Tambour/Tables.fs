/// A project's tables: read from its directory, their prelude written beside the scripts, and
/// their columns as the bytes a worker takes.
module Tambour.Tables

open System.IO
open Tambour.Data
open Tambour.Protocol

/// The project's tables, by file: `<name>.arrow` where there is one, `<name>.csv` otherwise, in
/// `tables/` in a workspace. A file that does not read is reported, not skipped silently.
let load (directory: string) : Prelude.Source list * string list =
    let files =
        Directory.GetFiles(Project.tablesIn directory)
        |> Array.filter (fun f -> f.EndsWith ".csv" || f.EndsWith ".arrow")
        |> Array.groupBy Path.GetFileNameWithoutExtension
        |> Array.sortBy fst
        |> Array.map (fun (_, fs) -> fs |> Array.tryFind (fun f -> f.EndsWith ".arrow") |> Option.defaultValue fs.[0])

    let read (file: string) =
        match (if file.EndsWith ".arrow" then Arrow.readFile file else Csv.readFile file) with
        | Ok table -> Ok { Prelude.Table = table; Prelude.File = Project.tableFile directory (Path.GetFileName file) }
        | Error why -> Error $"{Path.GetFileName file}: {why}"

    let results = files |> Array.map read |> List.ofArray
    (results |> List.choose (function Ok s -> Some s | Error _ -> None)), (results |> List.choose (function Error e -> Some e | Ok _ -> None))

/// Writes the prelude for the tables when it differs from what is on disk; whether it did.
let writePrelude (directory: string) (sources: Prelude.Source list) : bool =
    let path = Path.Combine(directory, Prelude.FileName)
    let text = Prelude.generate sources

    if File.Exists path && File.ReadAllText path = text then
        false
    else
        File.WriteAllText(path, text)
        true

/// A table as the request that sends it to a worker.
let request (table: Table) : Request =
    use stream = new MemoryStream()
    Arrow.write stream table
    Request.Table { Name = table.Name; Bytes = stream.ToArray() }
