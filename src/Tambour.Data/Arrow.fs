/// A table as an Arrow IPC file: one field per column, its type in Arrow's terms (double, int32,
/// utf8, bool, timestamp in microseconds and UTC, which spans every year a date-time can hold)
/// and its unit in the field's metadata. What a table
/// above the size a CSV handles well is kept as.
module Tambour.Data.Arrow

open System
open System.IO
open Apache.Arrow
open Apache.Arrow.Ipc
open Apache.Arrow.Types

/// The rows above which a table is kept as Arrow rather than CSV.
[<Literal>]
let Threshold = 100_000

[<Literal>]
let private UnitKey = "tambour.unit"

let private timestamp = TimestampType(TimeUnit.Microsecond, "UTC")

let private arrayOf (values: Values) : IArrowArray * IArrowType =
    match values with
    | Floats a -> DoubleArray.Builder().AppendRange(a).Build() :> IArrowArray, DoubleType.Default :> IArrowType
    | Ints a -> Int32Array.Builder().AppendRange(a).Build() :> IArrowArray, Int32Type.Default :> IArrowType
    | Texts a ->
        let b = StringArray.Builder()
        for s in a do b.Append(if isNull s then "" else s) |> ignore
        b.Build() :> IArrowArray, StringType.Default :> IArrowType
    | Bools a ->
        let b = BooleanArray.Builder()
        for x in a do b.Append x |> ignore
        b.Build() :> IArrowArray, BooleanType.Default :> IArrowType
    | Dates a ->
        let b = TimestampArray.Builder(timestamp)
        for d in a do b.Append(DateTimeOffset(d)) |> ignore
        b.Build() :> IArrowArray, timestamp :> IArrowType

let write (stream: Stream) (table: Tambour.Data.Table) =
    let arrays = table.Columns |> List.map (fun c -> c, arrayOf c.Values)

    let fields =
        arrays
        |> List.map (fun (c, (_, t)) ->
            let metadata =
                if String.IsNullOrEmpty c.Unit then null
                else Collections.Generic.Dictionary(dict [ UnitKey, c.Unit ]) :> Collections.Generic.IReadOnlyDictionary<string, string>

            Field(c.Name, t, false, metadata))

    let schema = Schema(fields, null)
    use writer = new ArrowFileWriter(stream, schema, leaveOpen = true)
    writer.WriteRecordBatch(new RecordBatch(schema, arrays |> List.map (fun (_, (a, _)) -> a), table.Rows))
    writer.WriteEnd()

let private valuesOf (field: Field) (arrays: IArrowArray list) : Values =
    match field.DataType with
    | :? DoubleType ->
        Floats [| for a in arrays do
                      let d = a :?> DoubleArray
                      for i in 0 .. d.Length - 1 do
                          yield (if d.IsNull i then nan else d.GetValue(i).Value) |]
    | :? Int32Type ->
        Ints [| for a in arrays do
                    let d = a :?> Int32Array
                    for i in 0 .. d.Length - 1 do
                        if d.IsNull i then failwith $"column {field.Name} is int and holds a missing value"
                        yield d.GetValue(i).Value |]
    | :? StringType ->
        Texts [| for a in arrays do
                     let d = a :?> StringArray
                     for i in 0 .. d.Length - 1 do
                         yield (if d.IsNull i then "" else d.GetString i) |]
    | :? BooleanType ->
        Bools [| for a in arrays do
                     let d = a :?> BooleanArray
                     for i in 0 .. d.Length - 1 do
                         if d.IsNull i then failwith $"column {field.Name} is bool and holds a missing value"
                         yield d.GetValue(i).Value |]
    | :? TimestampType ->
        Dates [| for a in arrays do
                     let d = a :?> TimestampArray
                     for i in 0 .. d.Length - 1 do
                         if d.IsNull i then failwith $"column {field.Name} is a date and holds a missing value"
                         yield d.GetTimestamp(i).Value.UtcDateTime |]
    | t -> failwith $"column {field.Name} is of Arrow type {t.Name}, which a table does not hold"

let read (name: string) (stream: Stream) : Result<Tambour.Data.Table, string> =
    try
        use reader = new ArrowFileReader(stream, leaveOpen = true)
        let schema = reader.Schema
        let batches = ResizeArray<RecordBatch>()
        let mutable batch = reader.ReadNextRecordBatch()

        while not (isNull batch) do
            batches.Add batch
            batch <- reader.ReadNextRecordBatch()

        let columns =
            [ for j in 0 .. schema.FieldsList.Count - 1 ->
                  let field = schema.FieldsList.[j]

                  let unit =
                      match field.Metadata with
                      | null -> null
                      | m ->
                          match m.TryGetValue UnitKey with
                          | true, u -> u
                          | _ -> null

                  { Name = field.Name
                    Unit = unit
                    Values = valuesOf field [ for b in batches -> b.Column j ] } ]

        Tambour.Data.Table.create name columns
    with e ->
        Error e.Message

let readFile (path: string) : Result<Tambour.Data.Table, string> =
    use stream = File.OpenRead path
    read (Path.GetFileNameWithoutExtension path) stream

let writeFile (path: string) (table: Tambour.Data.Table) =
    use stream = File.Create path
    write stream table
