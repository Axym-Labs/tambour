/// Typing in the script editor: Enter, Tab, Shift+Tab and Backspace indent as F# is indented, and
/// brackets and quotes close as they open; each case through the keyboard, checked for the text
/// and the caret it leaves, and taken back by the app's history to the text before.
[<Xunit.Collection("workers")>]
module Tambour.Tests.TypingTests

open System
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Headless.XUnit
open Avalonia.Input
open Avalonia.Threading
open Tambour
open Xunit

/// What is pressed or typed.
type Step =
    | Enter
    | Tab
    | ShiftTab
    | Backspace
    | Type of string

/// A case: the text before, with `‸` for the caret or `«…»` for a selection; what is done; the
/// text after, marked the same way.
let private cases: (string * string * Step list * string) list =
    [ // Enter keeps the line's indentation, one level deeper after a line that opens one.
      "after =", "let funcA (x) =‸", [ Enter ], "let funcA (x) =\n    ‸"
      "after ->", "xs |> List.map (fun x ->‸", [ Enter ], "xs |> List.map (fun x ->\n    ‸"
      "after <-", "    total <-‸", [ Enter ], "    total <-\n        ‸"
      "after (", "let p = f (‸", [ Enter ], "let p = f (\n    ‸"
      "after [", "let xs = [‸", [ Enter ], "let xs = [\n    ‸"
      "after [|", "let xs = [|‸", [ Enter ], "let xs = [|\n    ‸"
      "after {", "let r = {‸", [ Enter ], "let r = {\n    ‸"
      "after {|", "let r = {|‸", [ Enter ], "let r = {|\n    ‸"
      "after do", "for x in xs do‸", [ Enter ], "for x in xs do\n    ‸"
      "after then", "    if x > 0 then‸", [ Enter ], "    if x > 0 then\n        ‸"
      "after else", "    else‸", [ Enter ], "    else\n        ‸"
      "after a record's with", "let s = { r with‸", [ Enter ], "let s = { r with\n    ‸"
      "after try", "try‸", [ Enter ], "try\n    ‸"
      "after finally", "finally‸", [ Enter ], "finally\n    ‸"
      "after a match's with", "    match x with‸", [ Enter ], "    match x with\n    ‸"
      "after a try's with", "with‸", [ Enter ], "with\n‸"
      "a computation expression", "let fig = plot trials {‸", [ Enter ], "let fig = plot trials {\n    ‸"
      "a match case's arrow", "    | Some x ->‸", [ Enter ], "    | Some x ->\n        ‸"
      "a match case with its value", "    | Some x -> x‸", [ Enter ], "    | Some x -> x\n    ‸"
      "a line that opens nothing", "    let y = 2‸", [ Enter ], "    let y = 2\n    ‸"
      "a comment after the opener", "let x = // the answer‸", [ Enter ], "let x = // the answer\n    ‸"
      "mid-line", "    foo bar‸ baz", [ Enter ], "    foo bar\n    ‸baz"
      "spaces either side", "let a = 1   ‸   + 2", [ Enter ], "let a = 1\n‸+ 2"
      "between ( and )", "let p = f (‸)", [ Enter ], "let p = f (\n    ‸\n)"
      "between [| and |]", "    let xs = [|‸|]", [ Enter ], "    let xs = [|\n        ‸\n    |]"
      "between a CE's braces", "let fig = plot trials {‸}", [ Enter ], "let fig = plot trials {\n    ‸\n}"
      "in a string", "let s = \"ab‸cd\"", [ Enter ], "let s = \"ab\n‸cd\""
      "over a selection", "let a = «1 + 2»", [ Enter ], "let a =\n    ‸"

      // Tab and Shift+Tab.
      "Tab at a line's start", "‸let x = 1", [ Tab ], "    ‸let x = 1"
      "Tab to the next level", "  ‸x", [ Tab ], "    ‸x"
      "Tab on selected lines", "«let a = 1\n\nlet b = 2»", [ Tab ], "«    let a = 1\n\n    let b = 2»"
      "Shift+Tab", "    ‸let x = 1", [ ShiftTab ], "‸let x = 1"
      "Shift+Tab from 6", "      let‸ x", [ ShiftTab ], "    let‸ x"
      "Shift+Tab from 2", "  x‸", [ ShiftTab ], "x‸"
      "Shift+Tab on selected lines", "«        a\n    b»", [ ShiftTab ], "«    a\nb»"

      // Backspace in indentation takes it back to the level before.
      "Backspace in 4", "    ‸", [ Backspace ], "‸"
      "Backspace in 6", "      ‸", [ Backspace ], "    ‸"
      "Backspace in 8", "        ‸", [ Backspace ], "    ‸"
      "Backspace in 8, twice", "        ‸x", [ Backspace; Backspace ], "‸x"
      "Backspace after text", "    let x‸", [ Backspace ], "    let ‸"
      "Backspace after spaces after text", "let x =   ‸", [ Backspace ], "let x =  ‸"
      "Backspace on the next line", "let x =\n    ‸", [ Backspace ], "let x =\n‸"

      // Auto-closing.
      "( closes", "f ‸", [ Type "(" ], "f (‸)"
      "[ closes", "‸", [ Type "[" ], "[‸]"
      "{ closes", "‸", [ Type "{" ], "{‸}"
      "[| closes", "‸", [ Type "["; Type "|" ], "[|‸|]"
      "{| closes", "‸", [ Type "{"; Type "|" ], "{|‸|}"
      "\" closes", "let s = ‸", [ Type "\"" ], "let s = \"‸\""
      "< does not", "let xs: List‸", [ Type "<" ], "let xs: List<‸"
      "' does not", "let f (x: ‸", [ Type "'" ], "let f (x: '‸"
      "a closer steps over", "f ‸", [ Type "("; Type "x"; Type ")" ], "f (x)‸"
      "|] steps over", "‸", [ Type "["; Type "|"; Type "1"; Type "|"; Type "]" ], "[|1|]‸"
      "a quote steps over", "‸", [ Type "\""; Type "a"; Type "\"" ], "\"a\"‸"
      "a closer not put in is typed", "f (x‸)", [ Type ")" ], "f (x)‸)"
      "Backspace after (", "f ‸", [ Type "("; Backspace ], "f ‸"
      "Backspace after [|", "‸", [ Type "["; Type "|"; Backspace ], "[‸]"
      "Backspace after [| twice", "‸", [ Type "["; Type "|"; Backspace; Backspace ], "‸"
      "Backspace after a quote", "‸", [ Type "\""; Backspace ], "‸"
      "( wraps a selection", "let y = «a + b»", [ Type "(" ], "let y = («a + b»)"
      "\" wraps a selection", "let s = «abc»", [ Type "\"" ], "let s = \"«abc»\""
      "a quote in a string", "let s = \"ab‸cd\"", [ Type "\"" ], "let s = \"ab\"‸cd\""
      "a bracket before an identifier", "‸foo", [ Type "(" ], "(‸foo"
      "a bracket in a comment", "// see ‸", [ Type "(" ], "// see (‸"
      "Enter between brackets put in", "let p = f ‸", [ Type "("; Enter ], "let p = f (\n    ‸\n)" ]

/// The text without its marks, and the selection the marks stand for.
let private unmark (marked: string) =
    match marked.IndexOf '‸' with
    | -1 ->
        let s = marked.IndexOf '«'
        let e = marked.IndexOf '»' - 1
        marked.Replace("«", "").Replace("»", ""), s, e
    | c -> marked.Replace("‸", ""), c, c

let private mark (text: string) (s: int) (e: int) =
    if s = e then text.Insert(s, "‸") else text.Insert(e, "»").Insert(s, "«")

let private press (window: Window) (key: Key) (modifiers: RawInputModifiers) =
    window.KeyPress(key, modifiers, PhysicalKey.None, null)
    window.KeyRelease(key, modifiers, PhysicalKey.None, null)
    Dispatcher.UIThread.RunJobs()

let private run (window: Window) (step: Step) =
    match step with
    | Enter -> press window Key.Enter RawInputModifiers.None
    | Tab -> press window Key.Tab RawInputModifiers.None
    | ShiftTab -> press window Key.Tab RawInputModifiers.Shift
    | Backspace -> press window Key.Back RawInputModifiers.None
    | Type text ->
        window.KeyTextInput text
        Dispatcher.UIThread.RunJobs()

/// One case in a fresh editor with no language service, the typing alone: what is wrong with it,
/// if anything.
let private tryCase (before: string) (steps: Step list) (after: string) : string option =
    let editor = ScriptEditor()
    let window = Window(Width = 800.0, Height = 400.0, Content = editor)
    window.Show()
    let text, s, e = unmark before
    editor.Text <- text
    editor.Editor.Select(s, e - s)
    editor.Editor.TextArea.Focus() |> ignore
    Dispatcher.UIThread.RunJobs()

    let mutable history = History.empty
    use _ = editor.Edited.Subscribe(fun (edit, _) -> history <- History.record DateTime.UtcNow true edit history)

    for step in steps do
        run window step

    let area = editor.Editor.TextArea

    let s, e =
        if area.Selection.IsEmpty then editor.Editor.CaretOffset, editor.Editor.CaretOffset
        else area.Selection.SurroundingSegment.Offset, area.Selection.SurroundingSegment.EndOffset

    let typed = mark editor.Text s e

    // Every step of it taken back gives the text before; done again, the text after.
    let mutable state: History.State = { Script = editor.Text; Tables = Map.empty }
    let mutable h = history

    while (match History.undo h with
           | Some(entry, rest) ->
               state <- History.Entry.undo state entry
               h <- rest
               true
           | None -> false) do
        ()

    let undone = state.Script

    while (match History.redo h with
           | Some(entry, rest) ->
               state <- History.Entry.redo state entry
               h <- rest
               true
           | None -> false) do
        ()

    let redone = state.Script
    window.Close()

    if typed <> after then Some $"gave {typed}"
    elif undone <> text then Some $"undid to {undone}"
    elif redone <> editor.Text then Some $"redid to {redone}"
    else None

/// Gate: every case of the table, each in a fresh editor.
[<AvaloniaFact>]
let ``typing does what the table says, and the history takes it back`` () =
    let failures =
        [ for name, before, steps, after in cases do
              match tryCase before steps after with
              | Some why -> $"{name}: {why.Replace('\n', '⏎')} (wanted {after.Replace('\n', '⏎')})"
              | None -> () ]

    TestContext.Current.TestOutputHelper.WriteLine $"{cases.Length} cases, {failures.Length} failed"
    Assert.True(failures.IsEmpty, String.Join("\n", failures))

/// An opener and its closer are one edit, and an Enter's indentation is part of its line break.
[<AvaloniaFact>]
let ``an opener with its closer, and a line break with its indentation, are one entry each`` () =
    let editor = ScriptEditor()
    let window = Window(Width = 800.0, Height = 400.0, Content = editor)
    window.Show()
    editor.Text <- "let p = f "
    editor.Editor.CaretOffset <- editor.Text.Length
    editor.Editor.TextArea.Focus() |> ignore
    let edits = ResizeArray<History.TextEdit>()
    use _ = editor.Edited.Subscribe(fun (edit, _) -> edits.Add edit)
    run window (Type "(")
    run window Enter
    Assert.Equal(2, edits.Count)
    Assert.Equal("()", edits.[0].Inserted)
    Assert.Equal("\n    \n", edits.[1].Inserted)
    window.Close()

/// Backspaces in indentation join the group being deleted, as single backspaces do.
[<Fact>]
let ``backspaces in indentation group as backspaces do`` () =
    let now = DateTime.UtcNow
    let back offset (removed: string) : History.TextEdit = { Offset = offset; Removed = removed; Inserted = "" }

    let h =
        History.empty
        |> History.record now true (back 12 "x")
        |> History.record now true (back 8 "    ")
        |> History.record now true (back 4 "    ")

    Assert.Equal(1, h.Done.Length)

/// Completion comes by itself where a name is being used, not where one is being made, nor in a
/// comment or a string. The caret is at ‸.
[<Fact>]
let ``completion comes by itself where a name is used`` () =
    let at (marked: string) = Typing.completesAsTyped (marked.Replace("‸", "")) (marked.IndexOf '‸')

    for used in [ "let a = pre‸"; "x |> Array.map (fun r -> r.m‸"; "let b = f (p‸"; "  pre‸"; "let a = _x‸" ] do
        Assert.True(at used, used)

    for quiet in [ "let p‸"; "let mutable p‸"; "fun r‸"; "type R‸"; "// pre‸"; "let s = \"pre‸"; "let n = 1e‸"; "let a = ‸" ] do
        Assert.False(at quiet, quiet)

/// The call the caret is in: the nearest parenthesis before it not closed before it, past lists
/// and records; none outside every call, or in a string or comment only.
[<Fact>]
let ``the call the caret is in is found by its opening parenthesis`` () =
    let at (marked: string) = Typing.enclosingCall (marked.Replace("‸", "")) (marked.IndexOf '‸')

    Assert.Equal(Some 12, at "Array.sortBy(‸")
    Assert.Equal(Some 12, at "Array.sortBy(fun r -> r.x‸")
    Assert.Equal(Some 12, at "Array.sortBy(f (g x) [1; 2]‸")
    Assert.Equal(Some 12, at "Array.sortBy(xs, [1; ‸")
    Assert.Equal(Some 14, at "Array.sortBy(f(‸")
    Assert.Equal(None, at "Array.sortBy(f)‸")
    Assert.Equal(None, at "let a = 1\nlet b = ‸")
    Assert.Equal(None, at "let s = \"(‸")
    Assert.Equal(None, at "// (‸")
