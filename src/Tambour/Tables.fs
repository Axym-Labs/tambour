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

/// The script with a table's uses renamed: in its code, not its strings or comments, the binding
/// the prelude makes for the table and its rows' type where that changes, alone or after
/// `Prelude.`. With how many there were.
let renameUses (script: string) (before: string) (after: string) : string * int =
    let names =
        [ Prelude.identifier before, Prelude.identifier after
          if Prelude.typeName before <> Prelude.typeName after then Prelude.typeName before, Prelude.typeName after ]

    let code = Typing.codeMask script
    let isWord (c: char) = System.Char.IsLetterOrDigit c || c = '_' || c = '\''
    let text = System.Text.StringBuilder()
    let mutable count = 0
    let mutable i = 0

    let usedAt (name: string) =
        let stop = i + name.Length

        System.String.CompareOrdinal(script, i, name, 0, name.Length) = 0
        && code.[i]
        && (i = 0 || not (isWord script.[i - 1]))
        && (stop = script.Length || not (isWord script.[stop]))
        // After a dot only as the prelude's: `r.runs` is some record's field.
        && (i = 0 || script.[i - 1] <> '.' || script.Substring(0, i).EndsWith "Prelude.")

    while i < script.Length do
        match names |> List.tryFind (fst >> usedAt) with
        | Some(name, renamed) ->
            text.Append renamed |> ignore
            count <- count + 1
            i <- i + name.Length
        | None ->
            text.Append script.[i] |> ignore
            i <- i + 1

    text.ToString(), count
