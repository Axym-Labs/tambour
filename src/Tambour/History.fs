/// The project's one history, the app's: every change to the script, typed or made by a GUI
/// action, in order, each held as what it replaced, so it can be taken back and done again. The
/// table's changes join the same history, as entries of their own.
module Tambour.History

open System

/// A change to a text: at `Offset`, `Removed` gave way to `Inserted`.
type TextEdit =
    { Offset: int
      Removed: string
      Inserted: string }

module TextEdit =

    let apply (text: string) (e: TextEdit) =
        text.Substring(0, e.Offset) + e.Inserted + text.Substring(e.Offset + e.Removed.Length)

    let invert (e: TextEdit) = { e with Removed = e.Inserted; Inserted = e.Removed }

    /// The one edit that turns `before` into `after`: what lies between their common start and
    /// their common end.
    let between (before: string) (after: string) : TextEdit =
        let mutable prefix = 0

        while prefix < before.Length && prefix < after.Length && before.[prefix] = after.[prefix] do
            prefix <- prefix + 1

        let mutable suffix = 0

        while suffix < before.Length - prefix
              && suffix < after.Length - prefix
              && before.[before.Length - 1 - suffix] = after.[after.Length - 1 - suffix] do
            suffix <- suffix + 1

        { Offset = prefix
          Removed = before.Substring(prefix, before.Length - prefix - suffix)
          Inserted = after.Substring(prefix, after.Length - prefix - suffix) }

/// What the history changes: the script and the project's tables.
type State =
    { Script: string
      Tables: Map<string, Tambour.Data.Table> }

/// A change to a project's scripts on disk, made when it is done and taken back by its opposite.
type FileEdit =
    /// A script moved to another name.
    | Moved of source: string * target: string
    /// A script copied, and the bytes the copy was made with.
    | Copied of source: string * target: string * bytes: byte[]

/// One step of the history.
type Entry =
    /// Edits to the script, in the order they were made.
    | Script of TextEdit list
    /// Edits to the tables, in the order they were made.
    | Tables of Tambour.Data.TableEdit list
    /// Changes to the project's scripts on disk, which the history's state does not hold: whoever
    /// does or undoes the entry makes them.
    | Files of FileEdit list
    /// Entries done together and taken back together, as one.
    | Group of Entry list

module Entry =

    /// The state after the entry is done.
    let rec redo (state: State) (entry: Entry) =
        match entry with
        | Script edits -> { state with Script = edits |> List.fold TextEdit.apply state.Script }
        | Tables edits -> { state with Tables = edits |> List.fold Tambour.Data.TableEdit.apply state.Tables }
        | Files _ -> state
        | Group entries -> entries |> List.fold redo state

    /// The state after the entry is taken back.
    let rec undo (state: State) (entry: Entry) =
        match entry with
        | Script edits -> { state with Script = edits |> List.rev |> List.map TextEdit.invert |> List.fold TextEdit.apply state.Script }
        | Tables edits ->
            { state with Tables = edits |> List.rev |> List.map Tambour.Data.TableEdit.invert |> List.fold Tambour.Data.TableEdit.apply state.Tables }
        | Files _ -> state
        | Group entries -> entries |> List.rev |> List.fold undo state

    /// Whether doing or undoing the entry changes a table's columns, and so the prelude.
    let rec changesSchema (entry: Entry) =
        match entry with
        | Script _
        | Files _ -> false
        | Tables edits -> edits |> List.exists Tambour.Data.TableEdit.changesSchema
        | Group entries -> entries |> List.exists changesSchema

    /// The changes to files on disk the entry makes, in the order it makes them.
    let rec files (entry: Entry) =
        match entry with
        | Files edits -> edits
        | Group entries -> entries |> List.collect files
        | Script _
        | Tables _ -> []

/// How long a pause in typing ends the group being typed.
let pause = TimeSpan.FromSeconds 1.0

type History =
    { /// What was done, the latest first.
      Done: Entry list
      /// What was taken back, the latest first, until something new is done.
      Undone: Entry list
      /// When the latest entry was last typed into, while typing may still join it.
      Typing: DateTime option }

let empty =
    { Done = []
      Undone = []
      Typing = None }

let private isSpace (c: char) = Char.IsWhiteSpace c

/// Whether typed `next` joins the group `last` was typed into, as an editor groups typing: one
/// character at a time, each where the last left off, the same way (typing or deleting), no line
/// break before it, and no new word after a space. A backspace in indentation, which takes the
/// spaces back to the level before, counts as one.
let private joins (last: TextEdit) (next: TextEdit) =
    match last, next with
    | { Removed = "" }, { Removed = ""; Inserted = c } when c.Length = 1 && last.Inserted.Length > 0 ->
        let previous = last.Inserted.[last.Inserted.Length - 1]

        next.Offset = last.Offset + last.Inserted.Length
        && previous <> '\n'
        && not (isSpace previous && not (isSpace c.[0]))
    | { Inserted = "" }, { Inserted = ""; Removed = c } when (c.Length = 1 || c.Trim(' ') = "") && last.Removed.Length > 0 ->
        c <> "\n"
        // A backspace, just before the last; or a delete, at the same place.
        && (next.Offset + c.Length = last.Offset || next.Offset = last.Offset)
    | _ -> false

let private merge (last: TextEdit) (next: TextEdit) =
    if next.Removed = "" then { last with Inserted = last.Inserted + next.Inserted }
    elif next.Offset < last.Offset then { last with Offset = next.Offset; Removed = next.Removed + last.Removed }
    else { last with Removed = last.Removed + next.Removed }

/// Records an edit to the script. A typed one may join the group being typed; any other is an entry
/// of its own, as a GUI action is. Either way, what was taken back can no longer be done again.
let record (now: DateTime) (typed: bool) (edit: TextEdit) (h: History) : History =
    if edit.Removed = "" && edit.Inserted = "" then
        h
    else
        match h.Done, h.Typing with
        | Script [ last ] :: rest, Some since when typed && now - since < pause && joins last edit ->
            { Done = Script [ merge last edit ] :: rest
              Undone = []
              Typing = Some now }
        | _ ->
            { Done = Script [ edit ] :: h.Done
              Undone = []
              Typing = if typed then Some now else None }

/// Records an entry of its own, as a GUI action's or a change to files is.
let recordEntry (entry: Entry) (h: History) : History =
    { Done = entry :: h.Done
      Undone = []
      Typing = None }

/// Records edits to the tables, as one entry of their own.
let recordTables (edits: Tambour.Data.TableEdit list) (h: History) : History =
    if edits.IsEmpty then h else recordEntry (Tables edits) h

/// Takes the latest entry back: the entry, to be undone, and the history after.
let undo (h: History) : (Entry * History) option =
    match h.Done with
    | entry :: rest ->
        Some(
            entry,
            { Done = rest
              Undone = entry :: h.Undone
              Typing = None }
        )
    | [] -> None

/// Does the latest entry taken back again: the entry, to be redone, and the history after.
let redo (h: History) : (Entry * History) option =
    match h.Undone with
    | entry :: rest ->
        Some(
            entry,
            { Done = entry :: h.Done
              Undone = rest
              Typing = None }
        )
    | [] -> None
