/// The table grid: a million rows drawn at any scroll inside a frame, and its edits.
[<Xunit.Collection("workers")>]
module Tambour.Tests.GridTests

open System
open System.Diagnostics
open System.IO
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Headless.XUnit
open Avalonia.Threading
open Tambour
open Tambour.Data
open Xunit

let private log (line: string) = TestContext.Current.TestOutputHelper.WriteLine line

let private limit (ms: float) =
    if Environment.GetEnvironmentVariable "GITHUB_ACTIONS" = "true" then 3.0 * ms else ms

let private trials =
    { Name = "trials"
      Columns =
        [ { Name = "Time"; Unit = "s"; Values = Floats [| 0.0; 0.1; 0.2 |] }
          { Name = "Count"; Unit = null; Values = Ints [| 1; 2; 3 |] }
          { Name = "Sample"; Unit = null; Values = Texts [| "a"; "b"; "c" |] } ] }

let private shown (table: Table) =
    let grid = TableGrid(Table = Some table)
    let window = Window(Width = 900.0, Height = 500.0, Content = grid)
    window.Show()
    window.CaptureRenderedFrame().Dispose()
    let edits = ResizeArray<TableEdit list>()
    let refusals = ResizeArray<string>()
    grid.Edited.Add edits.Add
    grid.Refused.Add refusals.Add
    grid, window, edits, refusals

[<Fact>]
let ``a cell's text reads back as its value, and a block of text as it was copied`` () =
    for v in [ Floats [| 0.1; nan; -1e-300 |] :> obj; Ints [| -7; 0; 42 |]; Texts [| "x"; ""; "a b" |]; Bools [| true; false; true |]; Dates [| DateTime(2026, 10, 26, 12, 0, 0, 123, DateTimeKind.Utc); DateTime.UnixEpoch; DateTime(1, 1, 1, 0, 0, 0, DateTimeKind.Utc) |] ] do
        let values = v :?> Values

        for i in 0..2 do
            Assert.Equal(Values.slice i 1 values, (CellText.parse values.Kind (CellText.exact values i)) |> Result.toOption |> Option.get)

    let block = [| [| "1"; "tab\there"; "" |]; [| "quote \" mark"; "line\nbreak"; "x" |] |]
    Assert.Equal<string[][]>(block, CellText.ofTsv (CellText.toTsv block))
    Assert.True(CellText.parse "int" "" |> Result.isError)

[<AvaloniaFact>]
let ``typing into a cell is one edit of that cell, and a value its column cannot hold is refused`` () =
    let grid, _, edits, refusals = shown trials
    grid.Select({ Column = 0; Row = 1 }, { Column = 0; Row = 1 })
    grid.TypeInto "2.5"
    Assert.Equal<TableEdit list>([ Cells("trials", 0, 1, Floats [| 0.1 |], Floats [| 2.5 |]) ], Seq.exactlyOne edits)
    grid.Select({ Column = 1; Row = 0 }, { Column = 1; Row = 0 })
    grid.TypeInto "many"
    Assert.Equal(1, edits.Count)
    Assert.Contains("not a whole number", Seq.exactlyOne refusals)

[<AvaloniaFact>]
let ``a pasted block runs past the end by adding rows, and Delete blanks the selection`` () =
    let grid, window, edits, _ = shown trials
    grid.Select({ Column = 1; Row = 2 }, { Column = 1; Row = 2 })
    grid.Paste "10\tx\n11\ty"
    let pasted = Seq.exactlyOne edits
    Assert.Equal(AddRows("trials", 3, [ Floats [| nan |]; Ints [| 0 |]; Texts [| "" |] ]), pasted.Head)
    Assert.Contains(Cells("trials", 1, 2, Ints [| 3; 0 |], Ints [| 10; 11 |]), pasted)
    Assert.Contains(Cells("trials", 2, 2, Texts [| "c"; "" |], Texts [| "x"; "y" |]), pasted)

    edits.Clear()
    grid.Select({ Column = 0; Row = 0 }, { Column = 2; Row = 1 })
    Assert.Equal("0\t1\ta\n0.1\t2\tb", grid.Copied)
    grid.Focus() |> ignore
    window.KeyPress(Avalonia.Input.Key.Delete, Avalonia.Input.RawInputModifiers.None, Avalonia.Input.PhysicalKey.Delete, null)
    let cleared = Seq.exactlyOne edits
    Assert.Equal(3, cleared.Length)
    Assert.Contains(Cells("trials", 2, 0, Texts [| "a"; "b" |], Texts [| ""; "" |]), cleared)

/// Gate: a frame of the grid over a million rows, wherever it is scrolled, inside 16 ms.
[<AvaloniaFact>]
let ``a frame over a million rows is drawn within 16 ms wherever it is scrolled`` () =
    let rows = 1_000_000
    let column i unit = { Name = $"c{i}"; Unit = unit; Values = Floats(Array.init rows (fun r -> float r * float (i + 1) / 7.0)) }
    let table = { Name = "big"; Columns = [ column 0 "s"; column 1 "m"; column 2 "V/m"; column 3 "ppm"; column 4 null ] }
    let grid = TableGrid(Table = Some table)
    let window = Window(Width = 1280.0, Height = 800.0, Content = grid)
    window.Show()

    // A warm-up where the timed frames will be, so none of them is the first to run its code.
    let random = Random 7
    let warm n =
        for _ in 1..n do
            grid.Top <- random.Next rows
            window.CaptureRenderedFrame().Dispose()

    warm 5

    // What the earlier tests leave on the heap (2.7 GB in the full suite) is collected next, so the
    // timed frames' few allocations don't set off a young collection over it: one held a frame 18 ms.
    // Two more frames then bring back the caches the collection swept.
    GC.Collect()
    GC.WaitForPendingFinalizers()
    GC.Collect()
    warm 2

    let collections () = [ for g in 0..2 -> GC.CollectionCount g ]
    let before = collections ()
    let times = ResizeArray()
    let recorded = ResizeArray()

    for _ in 1..20 do
        grid.Top <- random.Next rows
        let clock = Stopwatch.StartNew()
        use frame = window.CaptureRenderedFrame()
        times.Add clock.Elapsed.TotalMilliseconds
        recorded.Add grid.LastRender

    let collected = List.map2 (-) (collections ()) before

    // What a frame of this window costs with no table in it: the headless raster and its copy.
    grid.Table <- None
    let empty = ResizeArray()

    for _ in 1..20 do
        grid.InvalidateVisual()
        let clock = Stopwatch.StartNew()
        use frame = window.CaptureRenderedFrame()
        empty.Add clock.Elapsed.TotalMilliseconds

    grid.Table <- Some table
    log $"""the same window empty: median {(Seq.sort empty |> Array.ofSeq).[10]:F1} ms"""
    let sorted = times |> Seq.sort |> Array.ofSeq
    log $"""a frame over 10^6 rows, 20 scroll positions: median {sorted.[10]:F1} ms, max {sorted.[19]:F1} ms; the grid's Render alone: median {(Seq.sort recorded |> Array.ofSeq).[10]:F2} ms, max {Seq.max recorded:F2} ms"""
    log $"collections during them, gen 0/1/2: {collected.[0]}/{collected.[1]}/{collected.[2]}"

    match Environment.GetEnvironmentVariable "TAMBOUR_CHECK_OUT" with
    | null | "" -> ()
    | dir ->
        grid.Top <- 500_000
        use frame = window.CaptureRenderedFrame()
        frame.Save(Path.Combine(dir, "grid.png"), Avalonia.Media.Imaging.PngBitmapEncoderOptions.Default)

    Assert.True(sorted.[19] < limit 16.0, $"the slowest frame took {sorted.[19]:F1} ms")

/// A row's header selects the row, Shift or Ctrl extends it; the context menu offers every grid
/// command, as the Table menu does; a command taken there is one change, as its edits.
[<AvaloniaFact>]
let ``a row header selects its row, and the grid's commands are on its context menu`` () =
    let grid, window, edits, _ = shown trials
    // Row 1's header: left of the cells, below the column headers (40 px) and row 0 (22 px).
    let header row = Avalonia.Point(6.0, 40.0 + 22.0 * float row + 11.0)
    window.MouseDown(header 1, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
    window.MouseUp(header 1, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
    Assert.Equal(({ Column = 0; Row = 1 }, { Column = 2; Row = 1 }), grid.Selection)
    window.MouseDown(header 2, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.Shift)
    window.MouseUp(header 2, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.Shift)
    Assert.Equal(({ Column = 0; Row = 1 }, { Column = 2; Row = 2 }), grid.Selection)

    let names =
        grid.ContextMenu.Items
        |> Seq.choose (function :? MenuItem as m -> Some(string m.Header) | _ -> None)
        |> List.ofSeq

    Assert.Equal<string list>(GridCommand.all |> List.map (fun (_, name, _) -> name), names)

    grid.Run GridCommand.DuplicateRows
    let expected = AddRows("trials", 3, trials.Columns |> List.map (fun c -> Values.slice 1 2 c.Values))
    Assert.Equal<TableEdit list>([ expected ], Seq.exactlyOne edits)
    window.Close()
