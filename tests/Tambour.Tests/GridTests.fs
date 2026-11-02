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

/// Gate: the grid's drawing of a frame over a million rows, wherever it is scrolled, inside 16 ms.
/// Its own Render is what is timed (the author's ruling): the whole frame, the headless raster and
/// the window's copy into a bitmap of its own, is logged beside it, and those copies set off full
/// collections whose length is the suite's heap, not the grid.
[<AvaloniaFact>]
let ``the grid draws a frame over a million rows within 16 ms wherever it is scrolled`` () =
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

    // What the earlier tests leave on the heap (2.7 GB in the full suite) is collected first, and two
    // more frames bring back the caches the collection swept.
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

    let slowest = Seq.max recorded
    Assert.True(slowest < limit 16.0, $"the grid's slowest Render took {slowest:F2} ms")

/// A row's header selects the row, Shift extends it, Ctrl adds another; the context menu offers every grid
/// command, as the Table menu does; a command taken there is one change, as its edits.
[<AvaloniaFact>]
let ``a row header selects its row, and the grid's commands are on its context menu`` () =
    let grid, window, edits, _ = shown trials
    // A row's header: left of the cells, below the column headers and the rows above it.
    let header row = Avalonia.Point(6.0, 40.0 + Tokens.Height.gridRow * (float row + 0.5))
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

    // The grid's commands, then how the column is shown.
    let layout = [ "_Hide column"; "Colla_pse column"; "Fit column _width"; "Show all colum_ns" ]
    Assert.Equal<string list>((GridCommand.all |> List.map (fun (_, name, _) -> name)) @ layout, names)

    grid.Run GridCommand.DuplicateRows
    let expected = AddRows("trials", 3, trials.Columns |> List.map (fun c -> Values.slice 1 2 c.Values))
    Assert.Equal<TableEdit list>([ expected ], Seq.exactlyOne edits)
    window.Close()

/// A drag selects a block of cells, as in a spreadsheet; with Ctrl it adds another beside those
/// selected, and Delete empties every block as one change. Copy takes one block only, and a click
/// without Ctrl leaves a single cell selected again.
[<AvaloniaFact>]
let ``a drag selects a block, Ctrl adds another, and Delete empties them all at once`` () =
    let grid, window, edits, refusals = shown trials
    let cell column row = Avalonia.Point(grid.ColumnEdge column - grid.ColumnWidth column / 2.0, 40.0 + Tokens.Height.gridRow * (float row + 0.5))
    let drag (a: Avalonia.Point) (b: Avalonia.Point) (modifiers: Avalonia.Input.RawInputModifiers) =
        window.MouseDown(a, Avalonia.Input.MouseButton.Left, modifiers)
        window.MouseMove(b, modifiers ||| Avalonia.Input.RawInputModifiers.LeftMouseButton)
        window.MouseUp(b, Avalonia.Input.MouseButton.Left, modifiers)
        Dispatcher.UIThread.RunJobs()

    drag (cell 0 0) (cell 1 1) Avalonia.Input.RawInputModifiers.None
    Assert.Equal(({ Column = 0; Row = 0 }, { Column = 1; Row = 1 }), grid.Selection)
    drag (cell 2 2) (cell 2 1) Avalonia.Input.RawInputModifiers.Control
    Assert.Equal<(CellRef * CellRef) list>([ { Column = 0; Row = 0 }, { Column = 1; Row = 1 }; { Column = 2; Row = 2 }, { Column = 2; Row = 1 } ], grid.Blocks)

    grid.Run GridCommand.Copy
    Assert.Contains("one block", Seq.exactlyOne refusals)
    Assert.Empty edits

    grid.Focus() |> ignore
    window.KeyPress(Avalonia.Input.Key.Delete, Avalonia.Input.RawInputModifiers.None, Avalonia.Input.PhysicalKey.Delete, null)
    let cleared = Seq.exactlyOne edits
    Assert.Equal<TableEdit list>(
        [ Cells("trials", 0, 0, Floats [| 0.0; 0.1 |], Floats [| nan; nan |])
          Cells("trials", 1, 0, Ints [| 1; 2 |], Ints [| 0; 0 |])
          Cells("trials", 2, 1, Texts [| "b"; "c" |], Texts [| ""; "" |]) ],
        cleared)

    window.MouseDown(cell 1 2, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
    window.MouseUp(cell 1 2, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
    Assert.Equal<(CellRef * CellRef) list>([ { Column = 1; Row = 2 }, { Column = 1; Row = 2 } ], grid.Blocks)
    window.Close()

/// Delete on whole rows empties them, and on whole rows already empty removes them, so a second
/// Delete removes what the first emptied; the blocks of rows Ctrl adds go with them. Cells short of
/// whole rows are only ever emptied.
[<AvaloniaFact>]
let ``Delete on empty whole rows removes them, so a second Delete removes the rows it emptied`` () =
    let five = { Name = "five"; Columns = [ { Name = "n"; Unit = null; Values = Ints [| 1; 2; 3; 4; 5 |] }; { Name = "s"; Unit = null; Values = Texts [| "a"; "b"; "c"; "d"; "e" |] } ] }
    let grid, window, edits, _ = shown five
    let delete () =
        grid.Focus() |> ignore
        window.KeyPress(Avalonia.Input.Key.Delete, Avalonia.Input.RawInputModifiers.None, Avalonia.Input.PhysicalKey.Delete, null)
        let last = Seq.last edits
        // The edits made the grid's table anew, as the shell does.
        grid.Table <- Some((Map.ofList [ "five", grid.Table.Value ], last) ||> List.fold TableEdit.apply |> Map.find "five")
        last

    let row r = Avalonia.Point(6.0, 40.0 + Tokens.Height.gridRow * (float r + 0.5))
    window.MouseDown(row 1, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
    window.MouseUp(row 1, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
    window.MouseDown(row 3, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.Control)
    window.MouseMove(row 4, Avalonia.Input.RawInputModifiers.Control ||| Avalonia.Input.RawInputModifiers.LeftMouseButton)
    window.MouseUp(row 4, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.Control)
    Assert.Equal(2, grid.Blocks.Length)

    Assert.All(delete (), fun e -> Assert.True(e.IsCells, $"the first Delete did more than empty cells: {e}"))
    Assert.Equal(5, grid.Table.Value.Rows)
    Assert.Equal<TableEdit list>([ RemoveRows("five", 3, [ Ints [| 0; 0 |]; Texts [| ""; "" |] ]); RemoveRows("five", 1, [ Ints [| 0 |]; Texts [| "" |] ]) ], delete ())
    Assert.Equal<Values list>([ Ints [| 1; 3 |]; Texts [| "a"; "c" |] ], grid.Table.Value.Columns |> List.map (fun c -> c.Values))
    Assert.Equal<(CellRef * CellRef) list>([ { Column = 0; Row = 1 }, { Column = 0; Row = 1 } ], grid.Blocks)

    // Empty cells short of a whole row stay.
    grid.Select({ Column = 0; Row = 0 }, { Column = 0; Row = 0 })
    delete () |> ignore
    delete () |> ignore
    Assert.Equal(2, grid.Table.Value.Rows)
    window.Close()

/// A drag down the row numbers selects whole rows, and past the grid's bottom it scrolls on.
[<AvaloniaFact>]
let ``a drag over the row numbers selects rows, and scrolls past the grid's edge`` () =
    let long = { Name = "long"; Columns = [ { Name = "n"; Unit = null; Values = Ints(Array.init 500 id) } ] }
    let grid, window, _, _ = shown long
    let header (y: float) = Avalonia.Point(6.0, y)
    let row r = 40.0 + Tokens.Height.gridRow * (float r + 0.5)
    window.MouseDown(header (row 2), Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
    window.MouseMove(header (row 4), Avalonia.Input.RawInputModifiers.LeftMouseButton)
    Assert.Equal(({ Column = 0; Row = 2 }, { Column = 0; Row = 4 }), grid.Selection)

    for _ in 1..5 do
        window.MouseMove(header (window.Height + 20.0), Avalonia.Input.RawInputModifiers.LeftMouseButton)

    window.MouseUp(header (window.Height + 20.0), Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
    let _, reached = grid.Selection
    Assert.True(grid.Top > 0, "the grid did not scroll")
    Assert.True(reached.Row >= grid.Top + 5, $"the selection reached row {reached.Row}, the grid's first row in view is {grid.Top}")

    // Released, the pointer selects nothing more as it moves.
    window.MouseMove(header (row 3))
    Assert.Equal(reached, snd grid.Selection)
    window.Close()

/// Gate 16 (b), the columns: each is as wide as its content asks, within the tokens' bounds; a drag
/// on a column's edge in the header resizes that column alone, and a double-click there fits it to
/// its content again.
[<AvaloniaFact>]
let ``columns fit their content, and an edge dragged or double-clicked changes that column alone`` () =
    let wide =
        { Name = "wide"
          Columns =
            [ { Name = "n"; Unit = null; Values = Ints [| 1; 2; 3 |] }
              { Name = "a header far longer than any column may be wide, so cut with an ellipsis"; Unit = "m"; Values = Floats [| 1.0; 2.0; 3.0 |] }
              { Name = "note"; Unit = null; Values = Texts [| "short"; "a somewhat longer note than the others"; "" |] } ] }

    let grid, window, _, _ = shown wide
    let widths () = [ for j in 0..2 -> grid.ColumnWidth j ]
    let before = widths ()
    Assert.Equal(Tokens.Width.columnMax, before.[1])
    Assert.True(before.[0] < before.[2], $"the narrow column is not narrower than the notes: {before}")
    Assert.True(before |> List.forall (fun w -> w >= Tokens.Width.columnMin && w <= Tokens.Width.columnMax), $"a width out of bounds: {before}")

    // A drag on the first column's edge, 50 px to the right.
    let edge = grid.ColumnEdge 0
    let y = 10.0
    window.MouseMove(Avalonia.Point(edge, y))
    window.MouseDown(Avalonia.Point(edge, y), Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
    window.MouseMove(Avalonia.Point(edge + 50.0, y), Avalonia.Input.RawInputModifiers.LeftMouseButton)
    window.MouseUp(Avalonia.Point(edge + 50.0, y), Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
    Dispatcher.UIThread.RunJobs()
    let dragged = widths ()
    Assert.Equal(before.[0] + 50.0, dragged.[0], 3)
    Assert.Equal<float list>(before.Tail, dragged.Tail)

    // An edit makes the column anew: its width stays.
    grid.Table <- Some { wide with Columns = { wide.Columns.[0] with Values = Ints [| 1; 2; 4 |] } :: wide.Columns.Tail }
    Assert.Equal(dragged.[0], grid.ColumnWidth 0, 3)

    // A double-click on the edge fits it again.
    grid.AutoFit 0
    Assert.Equal(before.[0], grid.ColumnWidth 0, 3)
    Assert.Equal<float list>(before.Tail, (widths ()).Tail)
    window.Close()

/// Gate 16 (b), hiding and collapsing: a hidden column is nowhere on screen, a collapsed one keeps
/// its place as a strip that a click opens again, the last column shown cannot be hidden, and the
/// first column shown stays at the left however far the grid scrolls.
[<AvaloniaFact>]
let ``hidden columns go, collapsed ones keep their place, and the first stays while the rest scroll`` () =
    // Twice what the window holds, so the grid scrolls in any font: twelve fitted in 900 px under
    // Windows' Segoe UI, as they do in Inter.
    let many =
        { Name = "many"
          Columns = [ for j in 0..23 ->{ Name = $"column {j}"; Unit = "m"; Values = Floats [| float j; 2.0 * float j |] } ] }

    let grid, window, _, _ = shown many
    let told = ResizeArray<GridLayout>()
    grid.LayoutChanged.Add told.Add
    let header (x: float) = grid.ColumnAt(Avalonia.Point(x, 10.0))
    let across () = [ for x in 0.0 .. 4.0 .. 880.0 -> header x ] |> List.choose id |> List.distinct

    grid.Hide 2
    Assert.DoesNotContain(2, across ())
    Assert.Equal<Set<string>>(set [ "column 2" ], (Seq.last told).Hidden)

    grid.ToggleCollapsed 3
    Assert.Equal(Tokens.Space.s3, grid.ColumnWidth 3)
    Assert.Contains(3, across ())
    let strip = grid.ColumnEdge 3 - grid.ColumnWidth 3 / 2.0
    window.MouseDown(Avalonia.Point(strip, 10.0), Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
    window.MouseUp(Avalonia.Point(strip, 10.0), Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
    Dispatcher.UIThread.RunJobs()
    Assert.True(grid.ColumnWidth 3 > Tokens.Space.s3, "a click on the strip did not open the column")

    // Scrolled to the end: the first column still where it was, the scrolled ones after it.
    let firstEdge = grid.ColumnEdge 0
    grid.Left <- 11
    Dispatcher.UIThread.RunJobs()
    Assert.Equal(firstEdge, grid.ColumnEdge 0, 3)
    Assert.Equal(Some 0, header (firstEdge - 5.0))
    Assert.True(grid.Left > 1, $"the grid did not scroll: {grid.Left}")
    Assert.Equal(Some(grid.Left + 1), header (firstEdge + 5.0)) // one hidden column before it

    grid.ShowAll()
    Assert.True((Seq.last told).Hidden.IsEmpty && (Seq.last told).Collapsed.IsEmpty)

    // The last column shown stays.
    let two = { Name = "two"; Columns = many.Columns |> List.truncate 2 }
    grid.Table <- Some two
    grid.Hide 0
    grid.Hide 1
    Assert.Equal<int list>([ 1 ], across ())
    window.Close()
