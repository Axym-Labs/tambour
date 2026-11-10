/// The table's data and its file: units as a script will see them, and every table read back from
/// CSV and from Arrow as it was written.
module Tambour.Tests.TableTests

open System
open System.Diagnostics
open System.IO
open FsCheck
open FsCheck.FSharp
open Tambour.Data
open Xunit

[<Fact>]
let ``a unit SI composes is typed in SI base units; anything else is a plain float`` () =
    Assert.Equal(NoUnit, Units.typing "")
    Assert.Equal(Typed("m kg s^-3 A^-1", 1.0), Units.typing "V/m")
    Assert.Equal(Typed("m", 1000.0), Units.typing "km")
    Assert.Equal(Typed("m^-1 kg s^-2", 1000.0), Units.typing "kPa")
    Assert.Equal(Typed("kg", 0.001), Units.typing "g")
    Assert.Equal(Typed("s^-1", 1.0), Units.typing "1/s")
    Assert.Equal(Typed("m^3", 1e-6), Units.typing "mL")
    Assert.Equal(Typed("m^3", 1e-6), Units.typing "ml")
    Assert.Equal(Typed("m^3", 1e-3), Units.typing "L")
    Assert.Equal(Typed("m^3 s^-1", 1e-6), Units.typing "mL/s")

    for outside in [ "degC"; "eV"; "ppm"; "km/h"; "min"; "furlong" ] do
        match Units.typing outside with
        | Untyped _ -> ()
        | other -> failwith $"{outside} is {other}"

// ---- Round trips ----------------------------------------------------------------------------------

let private awkward =
    Gen.elements [ ""; " "; "a"; "x,y"; "\"quoted\""; "line\nbreak"; "cr\r\nlf"; " lead"; "trail "; "λ μ 漢字"; "#hash"; "NaN"; "1.5"; "true" ]

let private floats =
    Gen.oneof
        [ Gen.elements [ nan; infinity; -infinity; 0.0; -0.0; Double.Epsilon; Double.MaxValue; Double.MinValue; 1e-300; 0.1 ]
          Gen.map2 (fun (a: int) (b: int) -> float a / float (max 1 (abs b))) (Gen.choose (-1_000_000, 1_000_000)) (Gen.choose (1, 1000)) ]

let private dates =
    Gen.map (fun (t: int64) -> DateTime((abs t % DateTime.MaxValue.Ticks) / 10L * 10L, DateTimeKind.Utc)) (Gen.map2 (fun (a: int) (b: int) -> int64 a * 1_000_000_007L + int64 b) (Gen.choose (Int32.MinValue, Int32.MaxValue)) (Gen.choose (0, Int32.MaxValue)))

let private units = Gen.elements [ null; "m"; "km"; "V/m"; "kg m s-2"; "degC"; "ppm"; "furlong"; "m, s"; "1/s" ]

let private values (rows: int) =
    Gen.oneof
        [ Gen.map Floats (Gen.arrayOfLength rows floats)
          Gen.map Ints (Gen.arrayOfLength rows (Gen.choose (Int32.MinValue, Int32.MaxValue)))
          Gen.map Texts (Gen.arrayOfLength rows awkward)
          Gen.map Bools (Gen.arrayOfLength rows (Gen.elements [ true; false ]))
          Gen.map Dates (Gen.arrayOfLength rows dates) ]

let private tables =
    gen {
        let! width = Gen.choose (1, 6)
        let! rows = Gen.choose (0, 60)
        let! names = Gen.arrayOfLength width (Gen.elements [ "t"; "x"; "a,b"; "\"q\""; "Time"; "λ"; "two words"; "#n" ])
        let names = names |> Array.mapi (fun i n -> $"{n}{i}")

        let! columns =
            names
            |> Array.map (fun name ->
                gen {
                    let! v = values rows
                    let! u = units
                    return { Name = name; Unit = (match v with Floats _ -> u | _ -> null); Values = v }
                })
            |> Gen.sequenceToList

        return { Name = "trial"; Columns = columns }
    }

/// A unit of none is written as an empty cell and read as none.
let private normal (t: Table) =
    { t with Columns = t.Columns |> List.map (fun c -> { c with Unit = (if String.IsNullOrEmpty c.Unit then null else c.Unit) }) }

let private same (a: Table) (b: Table) =
    let a, b = normal a, normal b
    a.Name = b.Name
    && a.Columns.Length = b.Columns.Length
    && List.forall2 (fun (x: Column) (y: Column) -> x.Name = y.Name && x.Unit = y.Unit && x.Values.Equals y.Values) a.Columns b.Columns

[<Fact>]
let ``a table read back from CSV is the table written, value for value`` () =
    let property =
        Prop.forAll (Arb.fromGen tables) (fun table ->
            match Csv.read table.Name (Csv.write table) with
            | Ok back when same table back -> true
            | Ok back -> failwithf "read back differently:\n%A\n---\n%A\n---\n%s" table back (Csv.write table)
            | Error e -> failwithf "not read back: %s\n%s" e (Csv.write table))

    Check.One(Config.QuickThrowOnFailure.WithMaxTest 400, property)

[<Fact>]
let ``a table read back from Arrow is the table written, value for value`` () =
    let property =
        Prop.forAll (Arb.fromGen tables) (fun table ->
            use stream = new MemoryStream()
            Arrow.write stream table
            stream.Position <- 0L

            match Arrow.read table.Name stream with
            | Ok back when same table back -> true
            | Ok back -> failwithf "read back differently:\n%A\n---\n%A" table back
            | Error e -> failwithf "not read back: %s" e)

    Check.One(Config.QuickThrowOnFailure.WithMaxTest 300, property)

[<Fact>]
let ``a CSV from elsewhere, with no unit row, is typed by what its cells hold`` () =
    let text = "n,x,flag,when,name,gap,odd\n1,1.5,true,2026-10-26T12:00:00Z,a,1,true\n2,2,false,2026-10-27T00:00:00+02:00,b,,\n"

    match Csv.read "elsewhere" text with
    | Error e -> failwith e
    | Ok t ->
        let kind name = (t.Columns |> List.find (fun c -> c.Name = name)).Values.Kind
        Assert.Equal<string list>([ "int"; "float"; "bool"; "date"; "string"; "float"; "string" ], [ for n in [ "n"; "x"; "flag"; "when"; "name"; "gap"; "odd" ] -> kind n ])
        Assert.Equal(2, t.Rows)

        match (t.Columns |> List.find (fun c -> c.Name = "when")).Values with
        | Dates d -> Assert.Equal(DateTime(2026, 10, 26, 22, 0, 0, DateTimeKind.Utc), d.[1])
        | _ -> ()

/// A shared runner is held to three times the limit, as the timed tests are.
let private limit (seconds: float) =
    if Environment.GetEnvironmentVariable "GITHUB_ACTIONS" = "true" then 3.0 * seconds else seconds

[<Fact>]
let ``a million rows of five float columns load from Arrow within a second`` () =
    let rows = 1_000_000
    let column i = { Name = $"c{i}"; Unit = "m"; Values = Floats(Array.init rows (fun r -> float (r * (i + 1)))) }
    let table = { Name = "big"; Columns = [ for i in 0..4 -> column i ] }
    let path = Path.Combine(Path.GetTempPath(), $"tambour-{Guid.NewGuid():N}.arrow")

    try
        Arrow.writeFile path table
        Arrow.readFile path |> ignore // the first read compiles the reader
        let clock = Stopwatch.StartNew()
        let back = Arrow.readFile path
        let elapsed = clock.Elapsed.TotalSeconds
        TestContext.Current.TestOutputHelper.WriteLine $"10^6 rows x 5 columns from Arrow: {elapsed:F3} s, {FileInfo(path).Length / 1_000_000L} MB"
        Assert.True(back |> Result.map (fun b -> same table { b with Name = table.Name }) = Ok true)
        Assert.True(elapsed < limit 1.0, $"the load took {elapsed:F2} s")
    finally
        File.Delete path
