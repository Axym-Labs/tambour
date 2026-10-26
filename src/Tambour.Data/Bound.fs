/// The tables a worker holds, as the app sent them, and their rows as a script's records. The
/// generated prelude reads its tables from here when it runs in Tambour (`#if TAMBOUR`).
module Tambour.Data.Bound

open System
open System.Collections.Concurrent
open Microsoft.FSharp.Reflection

let private held = ConcurrentDictionary<string, Table>()
let private versions = ConcurrentDictionary<string, int>()

/// Holds a table, in place of any of its name; its version counts how often.
let set (table: Table) =
    held.[table.Name] <- table
    versions.AddOrUpdate(table.Name, 1, (fun _ v -> v + 1)) |> ignore

/// How often a table has been set, 0 for never.
let version (name: string) =
    match versions.TryGetValue name with
    | true, v -> v
    | _ -> 0

/// A table's rows as records of the prelude's type for it, field for column in order, a typed
/// float's value in the SI unit its field is in.
let rows<'r> (name: string) : 'r[] =
    match held.TryGetValue name with
    | false, _ -> failwith $"Tambour holds no table named {name}"
    | true, table ->
        let fields = FSharpType.GetRecordFields typeof<'r>

        if fields.Length <> table.Columns.Length then
            failwith $"the prelude's {typeof<'r>.Name} has {fields.Length} fields and the table {name} {table.Columns.Length} columns; the prelude is written again when the project is saved"

        let ctor = FSharpValue.PreComputeRecordConstructor typeof<'r>

        let cell: (int -> obj)[] =
            table.Columns
            |> List.map (fun c ->
                match c.Values with
                | Floats a ->
                    match Units.typing c.Unit with
                    | Typed(_, factor) when factor <> 1.0 -> fun i -> box (a.[i] * factor)
                    | _ -> fun i -> box a.[i]
                | Ints a -> fun i -> box a.[i]
                | Texts a -> fun i -> box (if isNull a.[i] then "" else a.[i])
                | Bools a -> fun i -> box a.[i]
                | Dates a -> fun i -> box a.[i])
            |> Array.ofList

        Array.init table.Rows (fun i -> ctor (Array.map (fun get -> get i) cell) :?> 'r)
