/// The project's history: typing grouped as an editor groups it, a GUI action one entry, and every
/// change taken back and done again exactly.
module Tambour.Tests.HistoryTests

open System
open System.IO
open FsCheck
open FsCheck.FSharp
open Tambour.Edits.Edits
open Tambour.History
open Xunit

let private t0 = DateTime(2026, 10, 26, 12, 0, 0, DateTimeKind.Utc)

/// The history's state holding a script alone.
let private st (script: string) = { Script = script; Tables = Map.empty }

/// Types `text` one character at a time from `offset`, `gap` apart, into `h`.
let private typeIn (start: DateTime) (gap: TimeSpan) (offset: int) (text: string) (h: History) =
    text
    |> Seq.indexed
    |> Seq.fold (fun h (i, c) -> record (start + gap * float i) true { Offset = offset + i; Removed = ""; Inserted = string c } h) h

let private inserted (h: History) =
    h.Done |> List.rev |> List.map (function Script edits -> edits |> List.map (fun e -> e.Inserted) |> String.concat "" | Tables _ -> "")

[<Fact>]
let ``typing is grouped by word, and a line break and a pause end a group`` () =
    let fast = TimeSpan.FromMilliseconds 100.0
    let h = empty |> typeIn t0 fast 0 "let x = 1\nlet"
    Assert.Equal<string list>([ "let "; "x "; "= "; "1\n"; "let" ], inserted h)
    let later = t0 + TimeSpan.FromSeconds 10.0
    let h = h |> typeIn later fast 13 "ter"
    Assert.Equal<string list>([ "let "; "x "; "= "; "1\n"; "let"; "ter" ], inserted h)

[<Fact>]
let ``backspaces are one entry, and a GUI action is one of its own`` () =
    let fast = TimeSpan.FromMilliseconds 100.0
    let text = "abcdef"

    let h =
        [ 5; 4; 3 ]
        |> List.indexed
        |> List.fold (fun h (i, at) -> record (t0 + fast * float i) true { Offset = at; Removed = string text.[at]; Inserted = "" } h) empty

    Assert.Equal(1, h.Done.Length)
    Assert.Equal("abc", (Entry.redo (st text) h.Done.Head).Script)
    Assert.Equal(text, (Entry.undo (st "abc") h.Done.Head).Script)
    let h = record (t0 + fast * 4.0) false (TextEdit.between "abc" "abcX") h
    let h = record (t0 + fast * 5.0) true { Offset = 4; Removed = ""; Inserted = "Y" } h
    Assert.Equal(3, h.Done.Length)

[<Fact>]
let ``undo and redo walk the history, and something new forgets what was undone`` () =
    let h = empty |> record t0 false (TextEdit.between "a" "ab") |> record t0 false (TextEdit.between "ab" "abc")
    let e1, h = (undo h).Value
    Assert.Equal("ab", (Entry.undo (st "abc") e1).Script)
    let e2, h = (redo h).Value
    Assert.Equal("abc", (Entry.redo (st "ab") e2).Script)
    let _, h = (undo h).Value
    let h = record t0 false (TextEdit.between "ab" "abd") h
    Assert.True((redo h).IsNone)

// ---- Gate: random interleavings, undone and redone ------------------------------------------------

/// One step of a session: a character typed or deleted at a place (as a fraction of the text), a
/// pause, or a GUI action.
type Step =
    | Type of at: float * char
    | Backspace of at: float
    /// A word typed at one place, a key at a time, as a person types.
    | Word of at: float * string
    /// Backspace held at one place.
    | Backspaces of at: float * count: int
    | Pause
    | Act of index: int

let private scripts = Path.Combine(AppContext.BaseDirectory, "scripts", "edits")

let private actions (column: string) =
    [| Title "Ringdown"
       XLabel "elapsed"
       YLabel "height"
       YLog
       YDomain(0.0, 2.0)
       ThemeIs "presentation"
       Legend "LegendTopLeft"
       ColorBy column
       Facet column
       Fit
       AddLayer "GeomLine" |]

let private steps =
    let fraction = Gen.choose (0, 1000) |> Gen.map (fun n -> float n / 1000.0)
    let character = Gen.elements [ 'a'; 'x'; '1'; ' '; ' '; '\n'; '('; ')'; '.'; '"' ]

    let word = Gen.choose (1, 8) |> Gen.bind (fun n -> Gen.arrayOfLength n character) |> Gen.map String

    Gen.frequency
        [ 4, Gen.map2 (fun at c -> Type(at, c)) fraction character
          2, Gen.map Backspace fraction
          4, Gen.map2 (fun at w -> Word(at, w)) fraction word
          3, Gen.map2 (fun at n -> Backspaces(at, n)) fraction (Gen.choose (1, 6))
          2, Gen.constant Pause
          2, Gen.choose (0, 10) |> Gen.map Act ]
    |> Gen.listOf

let private scriptNames = Directory.GetFiles(scripts, "*.fsx") |> Array.sort

/// Plays a session on a script as the window would, then undoes it to the start and redoes it to
/// the end. Every text undo or redo stops at is the text that was there at that point of the
/// session, byte for byte, so a text that compiled then compiles again.
let private replay (script: int) (session: Step list) =
    let file = scriptNames.[script % scriptNames.Length]
    let original = File.ReadAllText file
    let column = if Path.GetFileName(file).StartsWith "4" then "Run" else "Sample"
    let all = actions column

    // The text after each entry, aligned with the history's `Done`, the latest first.
    let mutable text = original
    let mutable history = empty
    let mutable after: string list = []
    let mutable clock = t0

    let note (h: History) =
        if h.Done.Length > history.Done.Length then after <- text :: after
        elif h.Done.Length = history.Done.Length && h.Done.Length > 0 then after <- text :: List.tail after
        history <- h

    let typeAt (offset: int) (c: char) =
        clock <- clock + TimeSpan.FromMilliseconds 120.0
        let edit = { Offset = offset; Removed = ""; Inserted = string c }
        text <- TextEdit.apply text edit
        note (record clock true edit history)

    let backspaceAt (offset: int) =
        clock <- clock + TimeSpan.FromMilliseconds 120.0

        if offset > 0 then
            let edit = { Offset = offset - 1; Removed = string text.[offset - 1]; Inserted = "" }
            text <- TextEdit.apply text edit
            note (record clock true edit history)

    for step in session do
        match step with
        | Type(at, c) -> typeAt (int (at * float text.Length)) c
        | Backspace at -> backspaceAt (int (at * float text.Length))
        | Word(at, w) ->
            let start = int (at * float text.Length)
            w |> String.iteri (fun i c -> typeAt (start + i) c)
        | Backspaces(at, n) ->
            let start = int (at * float text.Length)

            for i in 0 .. n - 1 do
                backspaceAt (start - i)
        | Pause -> clock <- clock + TimeSpan.FromSeconds 2.0
        | Act i ->
            clock <- clock + TimeSpan.FromMilliseconds 500.0
            let edited, outcome = apply file "fig" all.[i] text

            match outcome with
            | Refused _ -> ()
            | _ when edited = text -> ()
            | _ ->
                let edit = TextEdit.between text edited
                text <- edited
                note (record clock false edit history)

    let final = text
    let expectedBack =
        match after with
        | [] -> []
        | _ :: older -> older @ [ original ]

    // Undone to the start, one entry at a time.
    let mutable h = history
    let mutable t = final

    for expected in expectedBack do
        let entry, h' = (undo h).Value
        t <- (Entry.undo (st t) entry).Script
        h <- h'

        if t <> expected then
            failwith $"an undo gave a text that was not there:\n{t}\n---\n{expected}"

    if t <> original then failwith "undone to the start, the text is not the original"
    if (undo h).IsSome then failwith "the history has more than was done"

    // Redone to the end.
    for expected in List.rev after do
        let entry, h' = (redo h).Value
        t <- (Entry.redo (st t) entry).Script
        h <- h'

        if t <> expected then
            failwith $"a redo gave a text that was not there:\n{t}\n---\n{expected}"

    if t <> final then failwith "redone to the end, the text is not the final one"
    true

[<Fact>]
let ``any interleaving of typing and actions, undone, gives the original byte for byte, and redone the final`` () =
    let property =
        Prop.forAll (Arb.fromGen (Gen.zip (Gen.choose (0, 4)) steps)) (fun (script, session) -> replay script session)

    Check.One(Config.QuickThrowOnFailure.WithMaxTest(300).WithEndSize(80), property)

// ---- Gate: table edits on the same history ----------------------------------------------------------

/// A step of a session that edits the table as well as the script.
type TableStep =
    | Text of Step
    | Cell of column: float * row: float * seed: int
    | AddColumn of at: float * kind: int
    | RemoveColumn of at: float
    | Rename of at: float * suffix: int
    | AddRows of at: float * count: int
    | RemoveRows of at: float * count: int
    /// A grid command on a selection, by fractions of the table's width and length.
    | GridCommand of kind: int * c0: float * c1: float * r0: float * r1: float

/// The grid's commands that edit without a clipboard or a form.
let private gridKinds =
    [| Tambour.GridCommand.Cut
       Tambour.GridCommand.DuplicateRows
       Tambour.GridCommand.InsertRowAbove
       Tambour.GridCommand.InsertRowBelow
       Tambour.GridCommand.DeleteRows
       Tambour.GridCommand.FillDown
       Tambour.GridCommand.DeleteColumns |]

let private tableSteps =
    let fraction = Gen.choose (0, 1000) |> Gen.map (fun n -> float n / 1000.0)

    let textStep =
        let character = Gen.elements [ 'a'; ' '; '\n'; '(' ]
        Gen.oneof [ Gen.map2 (fun at c -> Type(at, c)) fraction character; Gen.map Backspace fraction; Gen.constant Pause ]

    Gen.frequency
        [ 3, Gen.map Text textStep
          5, Gen.map3 (fun c r s -> Cell(c, r, s)) fraction fraction (Gen.choose (-1000, 1000))
          1, Gen.map2 (fun at k -> AddColumn(at, k)) fraction (Gen.choose (0, 4))
          1, Gen.map RemoveColumn fraction
          1, Gen.map2 (fun at n -> Rename(at, n)) fraction (Gen.choose (0, 99))
          2, Gen.map2 (fun at n -> AddRows(at, n)) fraction (Gen.choose (1, 5))
          2, Gen.map2 (fun at n -> RemoveRows(at, n)) fraction (Gen.choose (1, 5))
          3,
          Gen.map3 (fun k (a, b) (c, d) -> GridCommand(k, a, b, c, d)) (Gen.choose (0, gridKinds.Length - 1)) (Gen.zip fraction fraction) (Gen.zip fraction fraction) ]
    |> Gen.listOf

let private trials =
    { Tambour.Data.Table.Name = "trials"
      Tambour.Data.Table.Columns =
        [ { Name = "Time"; Unit = "s"; Values = Tambour.Data.Floats(Array.init 20 (fun i -> float i * 0.1)) }
          { Name = "Displacement"; Unit = "m"; Values = Tambour.Data.Floats(Array.init 20 (fun i -> float (i * i))) }
          { Name = "Sample"; Unit = null; Values = Tambour.Data.Texts(Array.init 20 (fun i -> $"S{i % 3}")) } ] }

/// A value of a column's type from a seed.
let private valueOf (kind: string) (seed: int) =
    match kind with
    | "float" -> Tambour.Data.Floats [| (if seed % 7 = 0 then nan else float seed / 8.0) |]
    | "int" -> Tambour.Data.Ints [| seed |]
    | "string" -> Tambour.Data.Texts [| $"v{seed}" |]
    | "bool" -> Tambour.Data.Bools [| seed % 2 = 0 |]
    | _ -> Tambour.Data.Dates [| DateTime.UnixEpoch.AddSeconds(float seed) |]

let private replayTables (session: TableStep list) =
    let original = { Script = File.ReadAllText(scriptNames.[0]); Tables = Map.ofList [ "trials", trials ] }
    let mutable state = original
    let mutable history = empty
    let mutable after: State list = []
    let mutable clock = t0
    let kinds = [| "float"; "int"; "string"; "bool"; "date" |]

    let note (h: History) =
        if h.Done.Length > history.Done.Length then after <- state :: after
        elif h.Done.Length = history.Done.Length && h.Done.Length > 0 then after <- state :: List.tail after
        history <- h

    let edit (e: Tambour.Data.TableEdit) =
        state <- { state with Tables = Tambour.Data.TableEdit.apply state.Tables e }
        note (recordTables [ e ] history)

    for step in session do
        let t = state.Tables.["trials"]
        let width = t.Columns.Length
        let pick (at: float) n = min (n - 1) (int (at * float n))

        match step with
        | Text(Type(at, c)) ->
            clock <- clock + TimeSpan.FromMilliseconds 120.0
            let e = { Offset = int (at * float state.Script.Length); Removed = ""; Inserted = string c }
            state <- { state with Script = TextEdit.apply state.Script e }
            note (record clock true e history)
        | Text(Backspace at) ->
            clock <- clock + TimeSpan.FromMilliseconds 120.0
            let offset = int (at * float state.Script.Length)

            if offset > 0 then
                let e = { Offset = offset - 1; Removed = string state.Script.[offset - 1]; Inserted = "" }
                state <- { state with Script = TextEdit.apply state.Script e }
                note (record clock true e history)
        | Text _ -> clock <- clock + TimeSpan.FromSeconds 2.0
        | Cell(c, r, seed) when width > 0 && t.Rows > 0 ->
            let column = pick c width
            edit (Tambour.Data.TableEdit.setCells t column (pick r t.Rows) (valueOf t.Columns.[column].Values.Kind seed))
        | AddColumn(at, k) -> edit (Tambour.Data.TableEdit.addColumn t (int (at * float width)) $"new{history.Done.Length}" kinds.[k] null)
        | RemoveColumn at when width > 1 -> edit (Tambour.Data.TableEdit.removeColumn t (pick at width))
        | Rename(at, n) when width > 0 ->
            let i = pick at width
            edit (Tambour.Data.TableEdit.changeColumn t i { t.Columns.[i] with Name = $"{t.Columns.[i].Name}_{n}" })
        | AddRows(at, n) -> edit (Tambour.Data.TableEdit.addRows t (int (at * float t.Rows)) n)
        | RemoveRows(at, n) when t.Rows > 0 ->
            let row = pick at t.Rows
            edit (Tambour.Data.TableEdit.removeRows t row (min n (t.Rows - row)))
        | GridCommand(k, a, b, c, d) when width > 0 && t.Rows > 0 ->
            let c0, c1, r0, r1 = pick a width, pick b width, pick c t.Rows, pick d t.Rows
            let s: Tambour.GridCommand.Selection = { C0 = min c0 c1; C1 = max c0 c1; R0 = min r0 r1; R1 = max r0 r1 }

            match Tambour.GridCommand.edits gridKinds.[k] t s with
            | [] -> ()
            | edits ->
                // One entry for the command, however many edits it holds.
                state <- { state with Tables = edits |> List.fold Tambour.Data.TableEdit.apply state.Tables }
                note (recordTables edits history)
        | _ -> ()

    let final = state
    let expectedBack = match after with [] -> [] | _ :: older -> older @ [ original ]
    let mutable h = history
    let mutable s = final

    for expected in expectedBack do
        let entry, h' = (undo h).Value
        s <- Entry.undo s entry
        h <- h'
        if s <> expected then failwithf "an undo gave a state that was not there:\n%A\n---\n%A" s expected

    if s <> original then failwith "undone to the start, the script and table are not the original"

    for expected in List.rev after do
        let entry, h' = (redo h).Value
        s <- Entry.redo s entry
        h <- h'
        if s <> expected then failwithf "a redo gave a state that was not there:\n%A\n---\n%A" s expected

    if s <> final then failwith "redone to the end, the script and table are not the final ones"
    true

[<Fact>]
let ``any interleaving of table edits and typing, undone, gives the original script and table, and redone the final`` () =
    Check.One(Config.QuickThrowOnFailure.WithMaxTest(300).WithEndSize(60), Prop.forAll (Arb.fromGen tableSteps) replayTables)
