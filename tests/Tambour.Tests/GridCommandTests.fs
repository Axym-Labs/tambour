/// Gate 14 (c): each grid command, on a selection of cells, of rows or of columns, gives the table
/// expected, and is undone to the table before, value for value.
module Tambour.Tests.GridCommandTests

open System
open FsCheck
open FsCheck.FSharp
open Tambour
open Tambour.Data
open Xunit

let private values (rows: int) =
    let floats = Gen.elements [ 0.0; 1.5; -2.25; 1e-300; nan; 3.0e8; 0.1 ]
    let texts = Gen.elements [ ""; "a"; "two words"; "tab\there"; "line\nbreak"; "\"q\"" ]
    let dates = Gen.choose (0, 1_000_000) |> Gen.map (fun s -> DateTime.UnixEpoch.AddSeconds(float s))

    Gen.oneof
        [ Gen.map Floats (Gen.arrayOfLength rows floats)
          Gen.map Ints (Gen.arrayOfLength rows (Gen.choose (-1000, 1000)))
          Gen.map Texts (Gen.arrayOfLength rows texts)
          Gen.map Bools (Gen.arrayOfLength rows (Gen.elements [ true; false ]))
          Gen.map Dates (Gen.arrayOfLength rows dates) ]

let private tables =
    gen {
        let! width = Gen.choose (1, 5)
        let! rows = Gen.choose (1, 12)
        let! columns = Gen.listOfLength width (values rows)
        return { Name = "trial"; Columns = columns |> List.mapi (fun i v -> { Name = $"c{i}"; Unit = null; Values = v }) }
    }

/// A selection in a table, from fractions of its width and length.
let private within (t: Table) (a: float, b: float, c: float, d: float) : GridCommand.Selection =
    let pick (f: float) n = min (n - 1) (int (f * float n))
    let c0, c1 = pick a t.Columns.Length, pick b t.Columns.Length
    let r0, r1 = pick c t.Rows, pick d t.Rows
    { C0 = min c0 c1; C1 = max c0 c1; R0 = min r0 r1; R1 = max r0 r1; Hidden = Set.empty }

/// The kinds of selection the gate names: some cells, whole rows, whole columns.
let private selections (t: Table) (fractions: float * float * float * float) (shape: int) =
    let s = within t fractions

    match shape with
    | 0 -> s
    | 1 -> { s with C0 = 0; C1 = t.Columns.Length - 1 }
    | _ -> { s with R0 = 0; R1 = t.Rows - 1 }

/// A table as the model the expected tables are written in: its rows of exact cell text.
let private rowsOf (t: Table) =
    [ for r in 0 .. t.Rows - 1 -> [ for c in t.Columns -> CellText.exact c.Values r ] ]

let private blankText (kind: string) = CellText.exact (Values.blank kind 1) 0

let private apply (t: Table) (edits: TableEdit list) =
    (edits |> List.fold TableEdit.apply (Map.ofList [ t.Name, t ])).[t.Name]

let private undo (t: Table) (edits: TableEdit list) =
    apply t (edits |> List.rev |> List.map TableEdit.invert)

/// What each command should leave, written on rows of text: none of it calls the commands.
let private expected (kind: GridCommand.Kind) (t: Table) (s: GridCommand.Selection) =
    let rows = rowsOf t
    let kinds = t.Columns |> List.map (fun c -> c.Values.Kind)
    let selected = [ s.R0 .. s.R1 ] |> List.map (fun r -> rows.[r])
    let blankRow = kinds |> List.map blankText

    match kind with
    | GridCommand.Cut ->
        rows
        |> List.mapi (fun r row ->
            row |> List.mapi (fun c cell -> if r >= s.R0 && r <= s.R1 && c >= s.C0 && c <= s.C1 then blankText kinds.[c] else cell))
    | GridCommand.DuplicateRows -> List.take (s.R1 + 1) rows @ selected @ List.skip (s.R1 + 1) rows
    | GridCommand.InsertRowAbove -> List.insertAt s.R0 blankRow rows
    | GridCommand.InsertRowBelow -> List.insertAt (s.R1 + 1) blankRow rows
    | GridCommand.DeleteRows -> List.take s.R0 rows @ List.skip (s.R1 + 1) rows
    | GridCommand.FillDown ->
        rows
        |> List.mapi (fun r row ->
            row |> List.mapi (fun c cell -> if r > s.R0 && r <= s.R1 && c >= s.C0 && c <= s.C1 then rows.[s.R0].[c] else cell))
    | GridCommand.DeleteColumns when s.C1 - s.C0 + 1 < t.Columns.Length ->
        rows |> List.map (fun row -> row |> List.indexed |> List.filter (fun (c, _) -> c < s.C0 || c > s.C1) |> List.map snd)
    | _ -> rows

let private kindsWithEdits =
    [ GridCommand.Cut
      GridCommand.DuplicateRows
      GridCommand.InsertRowAbove
      GridCommand.InsertRowBelow
      GridCommand.DeleteRows
      GridCommand.FillDown
      GridCommand.DeleteColumns ]

let private config = Config.QuickThrowOnFailure.WithMaxTest(400)

let private fractions =
    let f = Gen.map (fun n -> float n / 1000.0) (Gen.choose (0, 999))
    Gen.zip (Gen.zip f f) (Gen.zip f f) |> Gen.map (fun ((a, b), (c, d)) -> a, b, c, d)

let private cases =
    Gen.zip3 tables (Gen.zip (fractions) (Gen.choose (0, 2))) (Gen.elements kindsWithEdits)

[<Fact>]
let ``each grid command gives the table expected, and is undone to the table before`` () =
    Check.One(
        config,
        Prop.forAll (Arb.fromGen cases) (fun (t, (fractions, shape), kind) ->
            let s = selections t fractions shape
            let edits = GridCommand.edits kind t s
            let after = apply t edits

            if rowsOf after <> expected kind t s then
                failwithf "%A on %A gave\n%A\nnot\n%A" kind s (rowsOf after) (expected kind t s)

            if not ((undo after edits).Equals t) then
                failwithf "%A on %A, undone, is not the table before" kind s

            true)
    )

/// Copy and paste as a spreadsheet's block: cells pasted where they were copied leave the table
/// as it was; rows copied and pasted at the selection are those rows again, above it.
[<Fact>]
let ``cells and rows copied and pasted come back as they were`` () =
    Check.One(
        config,
        Prop.forAll (Arb.fromGen (Gen.zip tables (Gen.zip (fractions) (Gen.choose (0, 2))))) (fun (t, (fractions, shape)) ->
            let s = selections t fractions shape

            match GridCommand.paste t s (CellText.ofTsv (GridCommand.copyCells t s)) with
            | Error why -> failwith why
            | Ok edits ->
                if rowsOf (apply t edits) <> rowsOf t then failwithf "cells pasted back changed the table at %A" s

            match GridCommand.pasteRows t s (CellText.ofTsv (GridCommand.copyRows t s)) with
            | Error why -> failwith why
            | Ok edits ->
                let rows = rowsOf t
                let selected = [ s.R0 .. s.R1 ] |> List.map (fun r -> rows.[r])
                let after = apply t edits
                if rowsOf after <> List.take s.R0 rows @ selected @ List.skip s.R0 rows then failwithf "rows pasted at %A are not the rows copied" s
                if not ((undo after edits).Equals t) then failwithf "rows pasted at %A, undone, are not the table before" s

            true)
    )

/// A paste refused for a cell its column cannot read changes nothing and says which column.
[<Fact>]
let ``a paste a column cannot read is refused whole`` () =
    let t = { Name = "t"; Columns = [ { Name = "n"; Unit = null; Values = Ints [| 1; 2 |] }; { Name = "x"; Unit = "m"; Values = Floats [| 1.0; 2.0 |] } ] }
    let s: GridCommand.Selection = { C0 = 0; C1 = 1; R0 = 0; R1 = 0; Hidden = Set.empty }

    match GridCommand.paste t s [| [| "3"; "not a number" |] |] with
    | Error why -> Assert.StartsWith("x:", why)
    | Ok edits -> failwithf "pasted: %A" edits

    match GridCommand.pasteRows t s [| [| "half"; "1.5" |] |] with
    | Error why -> Assert.StartsWith("n:", why)
    | Ok edits -> failwithf "pasted rows: %A" edits

/// A hidden column inside a selected range is left alone: copied, cleared, filled, pasted into and
/// deleted only where the columns are shown.
[<Fact>]
let ``a selection's commands pass over the hidden columns inside it`` () =
    let column name values = { Name = name; Unit = null; Values = Ints values }
    let t = { Name = "t"; Columns = [ column "a" [| 1; 2 |]; column "b" [| 3; 4 |]; column "c" [| 5; 6 |]; column "d" [| 7; 8 |] ] }
    let s: GridCommand.Selection = { C0 = 0; C1 = 2; R0 = 0; R1 = 1; Hidden = set [ 1 ] }
    let after edits = (edits |> List.fold TableEdit.apply (Map.ofList [ t.Name, t ])).[t.Name]
    let cells (t: Table) = [ for c in t.Columns -> c.Name, [ for r in 0 .. t.Rows - 1 -> CellText.exact c.Values r ] ]

    Assert.Equal("1\t5\n2\t6", (GridCommand.copyCells t s).TrimEnd('\n').Replace("\r\n", "\n"))
    Assert.Equal<(string * string list) list>(
        [ "a", [ "0"; "0" ]; "b", [ "3"; "4" ]; "c", [ "0"; "0" ]; "d", [ "7"; "8" ] ],
        cells (after (GridCommand.clear t s)))
    Assert.Equal<(string * string list) list>(
        [ "a", [ "1"; "1" ]; "b", [ "3"; "4" ]; "c", [ "5"; "5" ]; "d", [ "7"; "8" ] ],
        cells (after (GridCommand.fillDown t s)))

    match GridCommand.paste t s [| [| "10"; "11"; "12" |] |] with
    | Ok edits ->
        Assert.Equal<(string * string list) list>(
            [ "a", [ "10"; "2" ]; "b", [ "3"; "4" ]; "c", [ "11"; "6" ]; "d", [ "12"; "8" ] ],
            cells (after edits))
    | Error why -> failwith why

    Assert.Equal<string list>([ "b"; "d" ], [ for c in (after (GridCommand.deleteColumns t s)).Columns -> c.Name ])
