namespace Tambour.Data

open System

/// A column's values, all of one type, in one array.
///
/// A missing value is `nan` in a float column and `""` in a string column. An int, bool or
/// date-time column has none.
[<CustomEquality; NoComparison>]
type Values =
    | Floats of float[]
    | Ints of int[]
    | Texts of string[]
    | Bools of bool[]
    /// Instants, in UTC, to the microsecond.
    | Dates of DateTime[]

    member this.Length =
        match this with
        | Floats a -> a.Length
        | Ints a -> a.Length
        | Texts a -> a.Length
        | Bools a -> a.Length
        | Dates a -> a.Length

    /// The type's name, as the file and the header write it.
    member this.Kind =
        match this with
        | Floats _ -> "float"
        | Ints _ -> "int"
        | Texts _ -> "string"
        | Bools _ -> "bool"
        | Dates _ -> "date"

    /// Value for value: two NaNs are the same missing value, and -0.0 is not 0.0.
    override this.Equals(other) =
        let bits (a: float[]) = a |> Array.map (fun x -> if Double.IsNaN x then Double.NaN else x) |> Array.map BitConverter.DoubleToInt64Bits

        match this, other with
        | Floats a, (:? Values as Floats b) -> bits a = bits b
        | Ints a, (:? Values as Ints b) -> a = b
        | Texts a, (:? Values as Texts b) -> a = b
        | Bools a, (:? Values as Bools b) -> a = b
        | Dates a, (:? Values as Dates b) -> Array.map (fun (d: DateTime) -> d.Ticks, d.Kind) a = Array.map (fun (d: DateTime) -> d.Ticks, d.Kind) b
        | _ -> false

    override this.GetHashCode() = hash (this.Kind, this.Length)

/// How a stated uncertainty is given, as a lab's instrument or certificate gives it.
[<RequireQualifiedAccess>]
type Spread =
    /// A standard uncertainty, u itself.
    | Standard
    /// The half-width a of a rectangular distribution: u = a/√3.
    | Rectangular
    /// The half-width a of a triangular distribution: u = a/√6.
    | Triangular
    /// The step of the last digit read: a rectangular half-width of step/2, u = step/√12.
    | Resolution
    /// An expanded uncertainty U at a coverage factor k: u = U/k.
    | Expanded of k: float

/// One part of a column's uncertainty: how it is given, its value in the column's unit, and what
/// it is in the lab's words ("ruler resolution"). Two parts are equal when each row's own value
/// (nan) is on both.
[<CustomEquality; NoComparison>]
type Part =
    { Spread: Spread
      /// In the column's unit; nan where each row has its own, in the column `u(name)`.
      Value: float
      Source: string }

    override this.Equals(other) =
        match other with
        | :? Part as p -> p.Spread = this.Spread && p.Source = this.Source && (p.Value = this.Value || (Double.IsNaN p.Value && Double.IsNaN this.Value))
        | _ -> false

    override this.GetHashCode() = hash (this.Spread, this.Source)

/// What a float column's uncertainty is, as declared. A reading's own part is a new independent
/// input for every row; a shared part is one input for the whole column, so it cancels in a
/// difference and does not average down.
type Uncertainty =
    { PerReading: Part option
      Shared: Part option
      /// The degrees of freedom of each reading's own part: infinity for one stated (type B),
      /// n − 1 for a mean of n readings.
      Dof: float }

/// A column: its name, the unit its values are in (a float column's only; null or empty for
/// none), its values, and the uncertainty declared for them (a float column's only).
type Column =
    { Name: string
      Unit: string
      Values: Values
      Uncertainty: Uncertainty option }

/// A table: its name and its columns, every one the same length.
type Table =
    { Name: string
      Columns: Column list }

    member this.Rows =
        match this.Columns with
        | c :: _ -> c.Values.Length
        | [] -> 0

module Table =

    /// A table, checked: every column the same length, names unique and not empty, a unit only on
    /// a float column, and every date in UTC.
    let create (name: string) (columns: Column list) : Result<Table, string> =
        let lengths = columns |> List.map (fun c -> c.Values.Length) |> List.distinct
        let names = columns |> List.map (fun c -> c.Name)

        if String.IsNullOrWhiteSpace name then
            Error "a table needs a name"
        elif lengths.Length > 1 then
            Error $"""the columns are of different lengths: {String.Join(", ", lengths)}"""
        elif names |> List.exists String.IsNullOrWhiteSpace then
            Error "a column needs a name"
        elif (names |> List.distinct).Length < names.Length then
            Error "two columns have the same name"
        elif columns |> List.exists (fun c -> not (String.IsNullOrEmpty c.Unit) && c.Values.Kind <> "float") then
            Error "only a float column carries a unit"
        elif columns |> List.exists (fun c -> c.Uncertainty.IsSome && c.Values.Kind <> "float") then
            Error "only a float column carries an uncertainty"
        elif columns |> List.exists (fun c -> not (isNull c.Unit) && c.Unit.StartsWith "#") then
            Error "a unit does not start with #, which marks a column's type in the file"
        elif
            columns
            |> List.exists (fun c ->
                match c.Values with
                | Dates a -> a |> Array.exists (fun d -> d.Kind <> DateTimeKind.Utc)
                | _ -> false)
        then
            Error "a date-time column holds instants in UTC"
        elif
            columns
            |> List.exists (fun c ->
                match c.Values with
                | Dates a -> a |> Array.exists (fun d -> d.Ticks % 10L <> 0L)
                | _ -> false)
        then
            Error "a date-time column holds instants to the microsecond"
        else
            Ok { Name = name; Columns = columns }
