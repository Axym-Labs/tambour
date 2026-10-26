namespace Tambour.Data

open System

/// A change to a project's tables, holding what it replaced, so it can be taken back exactly.
type TableEdit =
    /// The cells of one column from `row`: `before` gave way to `after`, of one type and length.
    | Cells of table: string * column: int * row: int * before: Values * after: Values
    | AddColumn of table: string * index: int * column: Column
    | RemoveColumn of table: string * index: int * column: Column
    /// A column changed as a whole: renamed, its unit set, or its type changed.
    | ChangeColumn of table: string * index: int * before: Column * after: Column
    /// Rows inserted at `row`: a run of values per column, in column order.
    | AddRows of table: string * row: int * values: Values list
    | RemoveRows of table: string * row: int * values: Values list
    | AddTable of Table
    | RemoveTable of Table

/// Runs of values: cut out, put in, and replaced, whatever their type.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Values =

    let private map (f: Array -> Array) (v: Values) =
        match v with
        | Floats a -> Floats(f a :?> float[])
        | Ints a -> Ints(f a :?> int[])
        | Texts a -> Texts(f a :?> string[])
        | Bools a -> Bools(f a :?> bool[])
        | Dates a -> Dates(f a :?> DateTime[])

    let private array (v: Values) : Array =
        match v with
        | Floats a -> a :> Array
        | Ints a -> a
        | Texts a -> a
        | Bools a -> a
        | Dates a -> a

    /// `count` values from `start`.
    let slice (start: int) (count: int) (v: Values) =
        v
        |> map (fun a ->
            let r = Array.CreateInstance(a.GetType().GetElementType(), count)
            Array.Copy(a, start, r, 0, count)
            r)

    /// `run` in place of as many values from `start`.
    let replace (start: int) (run: Values) (v: Values) =
        if run.Kind <> v.Kind then invalidArg "run" $"a run of {run.Kind} in a column of {v.Kind}"
        let source = array run

        v
        |> map (fun a ->
            let r = a.Clone() :?> Array
            Array.Copy(source, 0, r, start, source.Length)
            r)

    /// `run` put in before `start`.
    let insert (start: int) (run: Values) (v: Values) =
        if run.Kind <> v.Kind then invalidArg "run" $"a run of {run.Kind} in a column of {v.Kind}"
        let source = array run

        v
        |> map (fun a ->
            let r = Array.CreateInstance(a.GetType().GetElementType(), a.Length + source.Length)
            Array.Copy(a, 0, r, 0, start)
            Array.Copy(source, 0, r, start, source.Length)
            Array.Copy(a, start, r, start + source.Length, a.Length - start)
            r)

    /// `count` values from `start` taken out.
    let remove (start: int) (count: int) (v: Values) =
        v
        |> map (fun a ->
            let r = Array.CreateInstance(a.GetType().GetElementType(), a.Length - count)
            Array.Copy(a, 0, r, 0, start)
            Array.Copy(a, start + count, r, start, a.Length - start - count)
            r)

    /// `count` missing or empty values of a column's type: nan, "", 0, false, the epoch.
    let blank (kind: string) (count: int) =
        match kind with
        | "float" -> Floats(Array.create count nan)
        | "int" -> Ints(Array.zeroCreate count)
        | "string" -> Texts(Array.create count "")
        | "bool" -> Bools(Array.zeroCreate count)
        | _ -> Dates(Array.create count DateTime.UnixEpoch)

module TableEdit =

    let invert (e: TableEdit) =
        match e with
        | Cells(t, c, r, before, after) -> Cells(t, c, r, after, before)
        | AddColumn(t, i, c) -> RemoveColumn(t, i, c)
        | RemoveColumn(t, i, c) -> AddColumn(t, i, c)
        | ChangeColumn(t, i, before, after) -> ChangeColumn(t, i, after, before)
        | AddRows(t, r, v) -> RemoveRows(t, r, v)
        | RemoveRows(t, r, v) -> AddRows(t, r, v)
        | AddTable t -> RemoveTable t
        | RemoveTable t -> AddTable t

    /// Whether the edit changes a table's columns, and so the prelude: a cell's value or a row does
    /// not.
    let changesSchema (e: TableEdit) =
        match e with
        | Cells _
        | AddRows _
        | RemoveRows _ -> false
        | _ -> true

    let private table (tables: Map<string, Table>) (name: string) =
        match tables.TryFind name with
        | Some t -> t
        | None -> invalidArg "name" $"there is no table {name}"

    let private withColumns (t: Table) (columns: Column list) = { t with Columns = columns }

    /// The tables after the edit.
    let apply (tables: Map<string, Table>) (e: TableEdit) : Map<string, Table> =
        match e with
        | Cells(name, c, r, _, after) ->
            let t = table tables name
            let columns = t.Columns |> List.mapi (fun i col -> if i = c then { col with Values = Values.replace r after col.Values } else col)
            tables.Add(name, withColumns t columns)
        | AddColumn(name, i, column) ->
            let t = table tables name
            tables.Add(name, withColumns t (List.insertAt i column t.Columns))
        | RemoveColumn(name, i, _) ->
            let t = table tables name
            tables.Add(name, withColumns t (List.removeAt i t.Columns))
        | ChangeColumn(name, i, _, after) ->
            let t = table tables name
            tables.Add(name, withColumns t (List.updateAt i after t.Columns))
        | AddRows(name, r, values) ->
            let t = table tables name
            tables.Add(name, withColumns t (List.map2 (fun col v -> { col with Values = Values.insert r v col.Values }) t.Columns values))
        | RemoveRows(name, r, values) ->
            let t = table tables name
            tables.Add(name, withColumns t (List.map2 (fun col (v: Values) -> { col with Values = Values.remove r v.Length col.Values }) t.Columns values))
        | AddTable t -> tables.Add(t.Name, t)
        | RemoveTable t -> tables.Remove t.Name

    // ---- The edits a user makes, each holding what it replaces ---------------------------------

    /// Sets the cells of a column from a row to `values`.
    let setCells (t: Table) (column: int) (row: int) (values: Values) =
        let before = Values.slice row values.Length t.Columns.[column].Values
        Cells(t.Name, column, row, before, values)

    let addColumn (t: Table) (index: int) (name: string) (kind: string) (unit: string) =
        AddColumn(t.Name, index, { Name = name; Unit = unit; Values = Values.blank kind t.Rows })

    let removeColumn (t: Table) (index: int) = RemoveColumn(t.Name, index, t.Columns.[index])

    let changeColumn (t: Table) (index: int) (after: Column) = ChangeColumn(t.Name, index, t.Columns.[index], after)

    /// `count` rows of blank values inserted before `row`.
    let addRows (t: Table) (row: int) (count: int) =
        AddRows(t.Name, row, t.Columns |> List.map (fun c -> Values.blank c.Values.Kind count))

    let removeRows (t: Table) (row: int) (count: int) =
        RemoveRows(t.Name, row, t.Columns |> List.map (fun c -> Values.slice row count c.Values))
