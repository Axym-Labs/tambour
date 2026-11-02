/// The window: a menu bar and a toolbar over the script on the left and its figure (or a table) on
/// the right, the problems and the output in a panel below the script, a status bar at the foot. An Elmish loop over FuncUI, whose messages include
/// everything the worker says.
module Tambour.Shell

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open Elmish
open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Types
open Avalonia.Input
open Avalonia.Input.Platform
open Avalonia.Interactivity
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Platform.Storage
open Avalonia.Threading
open Avalonia.VisualTree
open Tambour.Protocol

/// The figure pane in FuncUI's terms.
module Pane =

    let create (attrs: IAttr<FigurePane> list) : IView<FigurePane> = ViewBuilder.Create<FigurePane>(attrs)

    let picture (p: Picture) : IAttr<FigurePane> =
        AttrBuilder<FigurePane>.CreateProperty<Picture>(
            "Picture",
            p,
            ValueSome(fun (v: FigurePane) -> v.Picture),
            ValueSome(fun (v: FigurePane, p) -> v.Picture <- p),
            ValueSome(fun (a: obj, b: obj) -> obj.ReferenceEquals(a, b))
        )

    let readout (r: ReadoutState option) : IAttr<FigurePane> =
        AttrBuilder<FigurePane>.CreateProperty<ReadoutState option>(
            "Readout",
            r,
            ValueSome(fun (v: FigurePane) -> v.Readout),
            ValueSome(fun (v: FigurePane, r) -> v.Readout <- r),
            ValueNone
        )

    let marks (points: Point[]) : IAttr<FigurePane> =
        AttrBuilder<FigurePane>.CreateProperty<Point[]>(
            "Marks",
            points,
            ValueSome(fun (v: FigurePane) -> v.Marks),
            ValueSome(fun (v: FigurePane, p) -> v.Marks <- p),
            ValueSome(fun (a: obj, b: obj) -> obj.ReferenceEquals(a, b))
        )

    let onPicked (f: Rect -> unit) : IAttr<FigurePane> =
        let subscribe (view: AvaloniaObject, handler: Rect -> unit, token: CancellationToken) =
            let d = (view :?> FigurePane).Picked.Subscribe handler
            token.Register(fun () -> d.Dispose()) |> ignore

        AttrBuilder<FigurePane>.CreateSubscription<Rect>("Picked", subscribe, f)

    let onInput (f: LiveInput -> unit) : IAttr<FigurePane> =
        let subscribe (view: AvaloniaObject, handler: LiveInput -> unit, token: CancellationToken) =
            let d = (view :?> FigurePane).Input.Subscribe handler
            token.Register(fun () -> d.Dispose()) |> ignore

        AttrBuilder<FigurePane>.CreateSubscription<LiveInput>("Input", subscribe, f)

/// The script editor in FuncUI's terms.
module Editor =

    let create (attrs: IAttr<ScriptEditor> list) : IView<ScriptEditor> = ViewBuilder.Create<ScriptEditor>(attrs)

    let text (t: string) : IAttr<ScriptEditor> =
        AttrBuilder<ScriptEditor>.CreateProperty<string>(
            "Text",
            t,
            ValueSome(fun (v: ScriptEditor) -> v.Text),
            ValueSome(fun (v: ScriptEditor, t) -> v.Text <- t),
            ValueNone
        )

    let language (l: LanguageService) : IAttr<ScriptEditor> =
        AttrBuilder<ScriptEditor>.CreateProperty<LanguageService>(
            "Language",
            l,
            ValueSome(fun (v: ScriptEditor) -> v.Language),
            ValueSome(fun (v: ScriptEditor, l) -> v.Language <- l),
            ValueSome(fun (a: obj, b: obj) -> obj.ReferenceEquals(a, b))
        )

    /// Moves the caret when the request's serial changes.
    let goTo (request: (int * int * int) option) : IAttr<ScriptEditor> =
        AttrBuilder<ScriptEditor>.CreateProperty<(int * int * int) option>(
            "GoTo",
            request,
            ValueNone,
            ValueSome(fun (v: ScriptEditor, r) ->
                match r with
                | Some(_, line, column) -> v.GoTo(line, column)
                | None -> ()),
            ValueNone
        )

    let private subscription name (events: ScriptEditor -> IObservable<'a>) (f: 'a -> unit) : IAttr<ScriptEditor> =
        let subscribe (view: AvaloniaObject, handler: 'a -> unit, token: CancellationToken) =
            let d = (events (view :?> ScriptEditor)).Subscribe handler
            token.Register(fun () -> d.Dispose()) |> ignore

        AttrBuilder<ScriptEditor>.CreateSubscription<'a>(name, subscribe, f)

    let path (p: string) : IAttr<ScriptEditor> =
        AttrBuilder<ScriptEditor>.CreateProperty<string>(
            "Path",
            p,
            ValueSome(fun (v: ScriptEditor) -> v.Path),
            ValueSome(fun (v: ScriptEditor, p) -> v.Path <- p),
            ValueNone
        )

    let onEdited f = subscription "Edited" (fun e -> e.Edited) f
    let onProblemsChanged f = subscription "ProblemsChanged" (fun e -> e.ProblemsChanged) f
    let onCaretMoved f = subscription "CaretMoved" (fun e -> e.CaretMoved) f

/// The table grid in FuncUI's terms.
module Sheet =

    let create (attrs: IAttr<TableGrid> list) : IView<TableGrid> = ViewBuilder.Create<TableGrid>(attrs)

    let table (t: Tambour.Data.Table) : IAttr<TableGrid> =
        AttrBuilder<TableGrid>.CreateProperty<Tambour.Data.Table option>(
            "Table",
            Some t,
            ValueSome(fun (v: TableGrid) -> v.Table),
            ValueSome(fun (v: TableGrid, t) -> v.Table <- t),
            ValueSome(fun (a: obj, b: obj) ->
                match a, b with
                | (:? (Tambour.Data.Table option) as x), (:? (Tambour.Data.Table option) as y) ->
                    match x, y with
                    | Some x, Some y -> obj.ReferenceEquals(x, y)
                    | None, None -> true
                    | _ -> false
                | _ -> false)
        )

    let private subscription name (events: TableGrid -> IObservable<'a>) (f: 'a -> unit) : IAttr<TableGrid> =
        let subscribe (view: AvaloniaObject, handler: 'a -> unit, token: CancellationToken) =
            let d = (events (view :?> TableGrid)).Subscribe handler
            token.Register(fun () -> d.Dispose()) |> ignore

        AttrBuilder<TableGrid>.CreateSubscription<'a>(name, subscribe, f)

    let marked (rows: Set<int>) : IAttr<TableGrid> =
        AttrBuilder<TableGrid>.CreateProperty<Set<int>>(
            "Marked",
            rows,
            ValueSome(fun (v: TableGrid) -> v.Marked),
            ValueSome(fun (v: TableGrid, r) -> v.Marked <- r),
            ValueNone
        )

    let layout (l: GridLayout) : IAttr<TableGrid> =
        AttrBuilder<TableGrid>.CreateProperty<GridLayout>("Layout", l, ValueSome(fun (v: TableGrid) -> v.Layout), ValueSome(fun (v: TableGrid, l) -> v.Layout <- l), ValueNone)

    let onLayoutChanged f = subscription "LayoutChanged" (fun g -> g.LayoutChanged) f
    let onEdited f = subscription "Edited" (fun g -> g.Edited) f
    let onSelectionChanged f = subscription "SelectionChanged" (fun g -> g.SelectionChanged) f
    let onRefused f = subscription "Refused" (fun g -> g.Refused) f
    let onHeaderActivated f = subscription "HeaderActivated" (fun g -> g.HeaderActivated) f
    let onInsertColumnAt f = subscription "InsertColumnAt" (fun g -> g.InsertColumnAt) f

/// The script the window opens with.
let example =
    """// Ctrl+Enter (or F5) runs the script; Escape cancels a run.
let t = Array.init 100 (fun i -> float i * 0.05<s>)

let fall =
    interact {
        let! h = Control.slider U.metre "height" 10.0<m> 100.0<m> 50.0<m>
        return
            Chart.lineIn U.second U.metre t (t |> Array.map (fun t -> max 0.0<m> (h - 0.5 * 9.81<m/s^2> * t * t)))
            |> Chart.withTitle "A dropped ball"
    }
"""

/// What every script may take for granted.
let prelude =
    "open Marey\nopen Marey.UnitsOfMeasure\nopen Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols\n"

/// What a workspace asks of the window that holds it, whose tabs are workspaces.
type Tabs =
    { /// A new tab, with an empty script.
      New: unit -> unit
      /// The tab of a script or a project directory: the one showing it already, or a new one.
      Open: string -> unit
      /// Closes the workspace's own tab, asking first about what is unsaved in it.
      Close: unit -> unit
      /// Shows the next tab, or with -1 the one before, round the ends.
      Cycle: int -> unit
      /// A script or project opened or saved under a new name, for the recent list.
      Opened: string -> unit
      /// The recent list emptied, or with `true` only of what is no longer on disk.
      ForgetRecent: bool -> unit
      /// Saves every tab with unsaved work.
      SaveAll: unit -> unit
      /// Closes every tab of a project directory, asking first about what is unsaved.
      CloseProject: string -> unit
      /// Asks for a name, under a title, the current one given; none where it was cancelled.
      AskName: string -> string -> Task<string option>
      /// Asks to go ahead with deleting the files named; Cancel is the default.
      ConfirmDelete: string list -> Task<bool>
      /// Asks a question with two answers, the yes and the no named; true for yes.
      Confirm: string -> string -> string -> Task<bool>
      /// A script moved: the tab showing it follows it.
      Renamed: string -> string -> unit
      /// Files deleted: a tab showing one of them closes, as there is nothing left to save.
      Deleted: string list -> unit
      /// Moves the workspace's tab into a window of its own.
      Detach: unit -> unit
      /// Opens the Preferences dialog.
      Preferences: unit -> unit
      /// The tables of the other tabs, each with its tab's name.
      OtherTables: unit -> (string * Tambour.Data.Table) list
      /// Asks which of these tables to take; none where it was cancelled.
      PickTable: (string * Tambour.Data.Table) list -> Task<Tambour.Data.Table option> }

/// What a workspace runs with: its workers, started when first asked, the language service it
/// shares with the other workspaces, the control it is drawn in, and the window's tabs.
type Env =
    { Workers: LazyWorkers
      Language: LanguageService
      Host: Control
      Tabs: Tabs }

/// The window the workspace is in, whose pickers ask where a file is.
let private topOf (env: Env) : TopLevel = TopLevel.GetTopLevel env.Host

/// The most output kept, in characters; older text goes first.
[<Literal>]
let private MaxOutput = 200_000

type Model =
    { Script: string
      Figures: FigureInfo[]
      Selected: int
      Picture: Picture
      Readout: ReadoutState option
      Controls: ControlInfo[]
      /// The controls whose time is playing.
      Playing: Set<int>
      Output: string
      Diagnostics: Diagnostic[]
      /// What the language service finds in the script as it is typed.
      Problems: Diagnostic[]
      /// A place to move the caret to, with a serial so the same place can be asked for twice.
      GoTo: (int * int * int) option
      /// What each GUI action would come to for the figure on screen, and its rows' columns.
      Actions: ActionOutcome[]
      Columns: string[]
      /// What is typed into the actions panel, by field.
      Inputs: Map<string, string>
      Running: bool
      Status: string
      /// What the last GUI action did, shown until the script is next edited.
      ActionNote: string
      /// Every change to the script, to take back and do again.
      History: History.History
      /// Where the script is on disk, if it is.
      Path: string option
      /// The project directory the script belongs to, if any, and its settings.
      Project: string option
      Settings: Project.Settings option
      /// The `.tambour` the project directory was unpacked from, if it was: written back from there.
      Bundle: string option
      /// The script as last read or written, to tell what is unsaved, and our own writes from
      /// someone else's.
      Saved: string
      /// Where the export being made goes.
      Exporting: string option
      /// The project's tables, and the files they are kept in.
      Tables: Tambour.Data.Prelude.Source list
      /// The tables as last read or written, to tell what is unsaved.
      SavedTables: Map<string, Tambour.Data.Table>
      /// The tables' files when the project was opened or last saved, so a table removed since
      /// has its file removed on the next save.
      TableFiles: string list
      /// The tab on the right: the figure's, then one per table.
      Tab: int
      /// The column whose header is being edited, by table and place; a new column as -1 - the
      /// place it goes.
      Header: (string * int) option
      /// The columns selected in a table's grid.
      TableSelection: (string * int list) option
      /// A figure to show once a run binds it, as one just written for the selected columns.
      ShowFigure: string option
      /// A pick on its way to the worker: what the figure plots, and where it was made.
      Picking: (PlottedFields * Rect) option
      /// Rows picked in the figure, by table, marked in its grid; and their points in the figure.
      Marked: (string * Set<int>) option
      Marks: Point[]
      /// The theme chosen in View → Theme, and the look the window is drawn in by it.
      Theme: Preferences.Theme
      Look: Palette.Look
      /// Where the caret is, for the status bar.
      Caret: int * int
      /// Where the divider between the script and the right side is, as a fraction of the width.
      Split: float
      /// The panel under the script: whether it is open, its tab (problems, output), its height.
      BottomOpen: bool
      BottomTab: int
      Bottom: float
      /// Whether the figure's actions are shown.
      ActionsOpen: bool
      /// How each table's columns are shown in its grid, by table name.
      Layouts: Map<string, GridLayout>
      /// Whether the right side takes the whole window.
      FullView: bool
      /// The scripts and projects opened, most recent first, as the window keeps them.
      Recent: string list
      /// The project's scripts on disk, as the file list shows them, and whether it is shown.
      Files: string list
      FilesOpen: bool
      /// The scripts open in the window's tabs, and whether each has unsaved work.
      OpenTabs: Map<string, bool> }

type Msg =
    | Edited of History.TextEdit * string
    | Undo
    | Redo
    | Run
    | Cancel
    | Replied of Reply
    | Select of int
    | Input of LiveInput
    | SetControl of index: int * value: float
    | TogglePlay of int
    | Tick of seconds: float
    | Problems of Diagnostic[]
    | GoTo of line: int * column: int
    | ActionsLoaded of script: string * ActionsFor
    | Typed of field: string * text: string
    | Act of ActionSpec
    | Acted of Acted
    | OpenPath of string
    | ChooseOpen of folder: bool
    | Save
    | SaveAs of string
    | DiskChanged
    | ChooseExport
    | ExportTo of path: string * format: string * width: float * height: float
    /// A picker closed without a choice.
    | Dismissed
    | SelectTab of int
    | TableEdited of Tambour.Data.TableEdit list
    /// A column's header form opened: a column there is, or a new one at -1 - its place.
    | EditHeader of table: string * column: int
    | ApplyHeader
    | CloseHeader
    | NewTable
    | ChooseImport
    | Import of string
    | RemoveTable of string
    /// A table renamed: its new name asked for, then whether the script's uses follow, then done.
    | RenameTable of string
    | RenameTableTo of table: string * name: string
    | RenameTableNow of table: string * name: string * uses: bool
    | DuplicateTable of string
    /// The shown table renamed or duplicated, from the Table menu.
    | RenameShownTable
    | DuplicateShownTable
    /// Asks for a table of another tab, to copy into this workspace.
    | ChooseCopyTable
    | CopyTable of Tambour.Data.Table option
    /// The project's text settings changed in Preferences: kept in its project file.
    | SetSkia of Tambour.Protocol.SkiaSettings
    | TableSelected of table: string * columns: int list
    | PlotSelected
    | Pick of Rect
    | PickFor of PlottedFields * Rect
    | ChooseTheme of Preferences.Theme
    | LookChanged of Palette.Look
    | Caret of line: int * column: int
    | ToggleBottom
    | ShowBottom of tab: int
    | ToggleActions
    | ToggleFullView
    | OpenPreferences
    /// The preferences changed: drawn again with them (the menus' shortcuts).
    | PreferencesChanged
    /// A table's grid laid its columns out anew.
    | LaidOut of table: string * GridLayout
    /// A divider dragged: where the script's side ends, as a fraction, and the bottom panel's height.
    | Resized of split: float * bottom: float
    /// A grid command from the Table menu or its key, for the table shown.
    | Grid of GridCommand.Kind
    | About
    | Quit
    | NewScript
    /// A script or a project directory, opened in a tab of its own.
    | OpenTab of string
    | ChooseSaveAs
    | SaveAll
    | CloseProject
    | ForgetRecent of missingOnly: bool
    | RecentChanged of string list
    /// The project directory's scripts read again.
    | FilesChanged
    | ToggleFiles
    | TabsChanged of Map<string, bool>
    | RenameFile of string
    | RenameTo of path: string * name: string
    | DuplicateFile of string
    | DeleteFile of string
    | DeleteNow of string list
    | RevealFile of string
    /// The shown script's file: renamed, duplicated, deleted or revealed, from the File menu.
    | RenameScript
    | DuplicateScript
    | DeleteScript
    | RevealScript
    /// The workspace written out as a plain folder, into a new one under the folder chosen.
    | ChooseExportFolder
    | ExportFolder of parent: string
    /// This workspace's script was moved to a new path.
    | Renamed of string
    | CloseTab
    | CycleTab of int
    | DetachTab
    /// Something to tell, shown where a GUI action's outcome is.
    | Note of string
    | Failed of exn
    /// The workspace disposed: its loop stops and its subscriptions with it, so a closed tab no
    /// longer watches its file or the preferences.
    | Ended

/// The bottom panel's height where a project has none.
let defaultBottom = 170.0

/// The user's preferences, or the defaults where they are kept nowhere (a test's run).
let private preferences () =
    match App.SettingsDirectory with
    | null -> Preferences.defaults
    | dir -> Preferences.read dir

/// Which panes are open, kept for the next tab and the next start.
let private keepPanes (model: Model) =
    match App.SettingsDirectory with
    | null -> ()
    | dir ->
        try
            Preferences.write dir { Preferences.read dir with FilesOpen = model.FilesOpen; BottomOpen = model.BottomOpen; ActionsOpen = model.ActionsOpen }
        with _ ->
            ()

let init (script: string) () : Model * Cmd<Msg> =
    let panes = preferences ()

    { Script = script
      Figures = [||]
      Selected = -1
      Picture = null
      Readout = None
      Controls = [||]
      Playing = Set.empty
      Output = ""
      Diagnostics = [||]
      Problems = [||]
      GoTo = None
      Actions = [||]
      Columns = [||]
      Inputs = Map.empty
      Running = false
      Status = "starting a worker…"
      ActionNote = ""
      History = History.empty
      Path = None
      Project = None
      Bundle = None
      Settings = None
      Saved = script
      Exporting = None
      Tables = []
      SavedTables = Map.empty
      TableFiles = []
      Tab = 0
      Header = None
      TableSelection = None
      ShowFigure = None
      Picking = None
      Marked = None
      Marks = [||]
      Theme = App.Theme
      Look = Palette.light
      Caret = (1, 1)
      Split = 0.5
      BottomOpen = panes.BottomOpen
      BottomTab = 0
      Bottom = defaultBottom
      ActionsOpen = panes.ActionsOpen
      Layouts = Map.empty
      FullView = false
      Recent = []
      Files = []
      FilesOpen = panes.FilesOpen
      OpenTabs = Map.empty },
    Cmd.none

let private send (env: Env) (request: Request) : Cmd<Msg> =
    let go (r: Request) : Task<unit> =
        task { do! env.Workers.SendAsync r }

    Cmd.OfTask.attempt go request Failed

/// The figure on screen, by the name the worker gave it, when it is one the actions can edit.
let private editable (model: Model) =
    match Array.tryItem model.Selected model.Figures with
    | Some f when f.Controls = 0 -> Some f.Name
    | _ -> None

/// Asks what the actions would do to the figure on screen.
let private loadActions (env: Env) (model: Model) : Cmd<Msg> =
    match editable model with
    | Some name ->
        let script = model.Script
        Cmd.OfTask.either (fun () -> env.Language.ActionsAsync(script, null, name)) () (fun a -> ActionsLoaded(script, a)) Failed
    | None -> Cmd.none

let private appendOutput (model: Model) (text: string) =
    let all = model.Output + text
    if all.Length > MaxOutput then all.Substring(all.Length - MaxOutput) else all

let private diagnosticLine (d: Diagnostic) =
    $"{d.StartLine}:{d.StartColumn + 1} {d.Severity} {d.Code}: {d.Message}"

/// The export formats, and what their files are called.
let exportFormats = [| "png"; "pdf"; "svg" |]

let private pickOpen (window: TopLevel) (folder: bool) : Task<string option> =
    task {
        if folder then
            let! picked = window.StorageProvider.OpenFolderPickerAsync(FolderPickerOpenOptions(Title = "Open a project folder"))
            return picked |> Seq.tryHead |> Option.bind (fun f -> Option.ofObj (f.TryGetLocalPath()))
        else
            let both = FilePickerFileType("Workspaces and scripts", Patterns = [| "*" + Bundle.Extension; "*.fsx" |])
            let bundles = FilePickerFileType("Tambour workspaces", Patterns = [| "*" + Bundle.Extension |])
            let scripts = FilePickerFileType("F# scripts", Patterns = [| "*.fsx" |])

            let! picked =
                window.StorageProvider.OpenFilePickerAsync(FilePickerOpenOptions(Title = "Open a workspace or a script", FileTypeFilter = [| both; bundles; scripts |]))
            return picked |> Seq.tryHead |> Option.bind (fun f -> Option.ofObj (f.TryGetLocalPath()))
    }

let private pickSave (window: TopLevel) (title: string) (name: string) (extension: string) : Task<string option> =
    task {
        let! picked =
            window.StorageProvider.SaveFilePickerAsync(
                FilePickerSaveOptions(Title = title, SuggestedFileName = name, DefaultExtension = extension, ShowOverwritePrompt = true)
            )

        return Option.ofObj picked |> Option.bind (fun f -> Option.ofObj (f.TryGetLocalPath()))
    }

/// The project's text settings, or the app's.
let private skiaOf (model: Model) =
    match model.Settings with
    | Some s -> s.Skia
    | None -> (Project.defaults "").Skia

let private writeProject (model: Model) =
    match model.Project, model.Path with
    | Some dir, Some path when Path.GetDirectoryName path = dir ->
        let settings = model.Settings |> Option.defaultValue (Project.defaults (Path.GetFileName path))
        Project.write
            dir
            { settings with
                Script = Path.GetFileName path
                Marey = Project.mareyVersion
                Split = model.Split
                Bottom = model.Bottom
                Columns =
                    [| for KeyValue(name, l) in model.Layouts ->
                           { Table = name
                             Widths = [| for KeyValue(c, w) in l.Widths -> { Column = c; Width = w } |]
                             Hidden = Array.ofSeq l.Hidden
                             Collapsed = Array.ofSeq l.Collapsed } : Project.TableColumns |] }
    | _ -> ()

/// The tables by name, as the history holds them.
let private tablesOf (model: Model) =
    model.Tables |> List.map (fun s -> s.Table.Name, s.Table) |> Map.ofList

/// The model with the tables the history gives: a table keeps its file, a new one is kept as CSV
/// until it is saved, and a removed one goes.
let private withTables (model: Model) (tables: Map<string, Tambour.Data.Table>) =
    let names = model.Tables |> List.map (fun s -> s.Table.Name) |> Set.ofList

    // A table renamed is the same columns under a name that was not there: it keeps its place, and
    // its file is named so on the next save.
    let renamed (s: Tambour.Data.Prelude.Source) =
        tables
        |> Map.tryPick (fun name t ->
            if not (names.Contains name) && obj.ReferenceEquals(t.Columns, s.Table.Columns) then
                let slash = s.File.LastIndexOf '/'
                Some { s with Table = t; File = s.File.Substring(0, slash + 1) + name + Path.GetExtension s.File }
            else
                None)

    let kept =
        model.Tables
        |> List.choose (fun s ->
            match tables.TryFind s.Table.Name with
            | Some t -> Some { s with Table = t }
            | None -> renamed s)

    let added =
        tables
        |> Map.toList
        |> List.filter (fun (name, _) -> not (kept |> List.exists (fun s -> s.Table.Name = name)))
        |> List.map (fun (name, t) -> { Tambour.Data.Prelude.Table = t; Tambour.Data.Prelude.File = name + ".csv" })

    // A renamed table's column layout goes with it to the new name.
    let layouts =
        model.Tables
        |> List.fold
            (fun (m: Map<string, GridLayout>) s ->
                match tables.ContainsKey s.Table.Name, m.TryFind s.Table.Name, renamed s with
                | false, Some l, Some r -> m.Remove(s.Table.Name).Add(r.Table.Name, l)
                | _ -> m)
            model.Layouts

    { model with Tables = kept @ added; Layouts = layouts }

/// The model and what follows once the script and the tables are `state`: a table that changed
/// is sent to the worker, the prelude is written again when the columns changed, and the figure
/// runs again.
let private toState (env: Env) (model: Model) (state: History.State) (schema: bool) : Model * Cmd<Msg> =
    let before = tablesOf model
    let next = withTables { model with Script = state.Script } state.Tables

    let changed =
        next.Tables
        |> List.filter (fun s ->
            match before.TryFind s.Table.Name with
            | Some t -> not (obj.ReferenceEquals(t, s.Table))
            | None -> true)

    if schema then
        match next.Project with
        | Some dir -> Tables.writePrelude dir next.Tables |> ignore
        | None -> ()

    if not (obj.ReferenceEquals(next.Layouts, model.Layouts)) then writeProject next

    next,
    Cmd.OfTask.either
        (fun () ->
            task {
                for s in changed do
                    do! env.Workers.SendAsync(Tables.request s.Table)
            })
        ()
        (fun () -> Run)
        Failed

/// A column's values read as another type, through their text: a float that does not read is
/// missing, any value reads as a string, and an int, bool or date that does not read is refused.
let private retype (values: Tambour.Data.Values) (kind: string) : Result<Tambour.Data.Values, string> =
    if values.Kind = kind then
        Ok values
    else
        let read =
            [| for i in 0 .. values.Length - 1 ->
                   match CellText.parse kind (CellText.exact values i), kind with
                   | Ok v, _ -> Ok v
                   | Error _, "float" -> Ok(Tambour.Data.Floats [| nan |])
                   | Error why, _ -> Error $"row {i + 1}: {why}" |]

        match read |> Array.tryPick (function Error e -> Some e | Ok _ -> None) with
        | Some why -> Error why
        | None ->
            Ok(
                read
                |> Array.fold
                    (fun (acc: Tambour.Data.Values) r ->
                        match r with
                        | Ok v -> Tambour.Data.Values.insert acc.Length v acc
                        | Error _ -> acc)
                    (Tambour.Data.Values.blank kind 0)
            )

/// A name no table has yet: `stem`, or `stem2`, `stem3`, ….
let private freshName (model: Model) (stem: string) =
    let taken = model.Tables |> List.map (fun s -> s.Table.Name) |> Set.ofList
    Seq.initInfinite (fun i -> if i = 0 then stem else $"{stem}{i + 1}") |> Seq.find (taken.Contains >> not)

/// The tables saved: each to `<name>.csv`, or `<name>.arrow` above Arrow's threshold, in
/// `tables/` in a workspace; a file of a table that moved between the two or was removed taken
/// away, and the prelude written again.
let private saveTables (model: Model) =
    match model.Project with
    | None -> model
    | Some dir ->
        let saved =
            model.Tables
            |> List.map (fun s ->
                let file = Project.tableFile dir (s.Table.Name + (if s.Table.Rows > Tambour.Data.Arrow.Threshold then ".arrow" else ".csv"))
                let path = Path.Combine(dir, file)
                Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore

                if file.EndsWith ".arrow" then Tambour.Data.Arrow.writeFile path s.Table
                else Tambour.Data.Csv.writeFile path s.Table

                if s.File <> file && File.Exists(Path.Combine(dir, s.File)) then File.Delete(Path.Combine(dir, s.File))
                { s with File = file })

        let files = saved |> List.map (fun s -> s.File)

        for gone in model.TableFiles |> List.filter (fun f -> not (List.contains f files)) do
            let path = Path.Combine(dir, gone)
            if File.Exists path then File.Delete path

        if not saved.IsEmpty then Tables.writePrelude dir saved |> ignore

        { model with
            Tables = saved
            SavedTables = saved |> List.map (fun s -> s.Table.Name, s.Table) |> Map.ofList
            TableFiles = files }

/// The figure shown, kept in a bundle as its thumbnail for the recent list and a file manager: as
/// the export draws it, on its own background, at most 512 px wide, drawn here rather than by a
/// worker.
let private writeThumbnail (dir: string) (picture: Picture) =
    let png = Marey.SkiaBackend.renderPng picture.Extent picture.Node
    let path = Path.Combine(dir, Bundle.Thumbnail)

    if float picture.Extent.Width <= 512.0 then
        File.WriteAllBytes(path, png)
    else
        use image = SkiaSharp.SKBitmap.Decode png
        let scale = 512.0 / float image.Width
        let info = SkiaSharp.SKImageInfo(512, max 1 (int (Math.Round(float image.Height * scale))))
        use smaller = image.Resize(info, SkiaSharp.SKSamplingOptions(SkiaSharp.SKCubicResampler.Mitchell))
        use data = smaller.Encode(SkiaSharp.SKEncodedImageFormat.Png, 100)
        File.WriteAllBytes(path, data.ToArray())

/// Makes changes to the project's scripts on disk, or with `back` takes them back, the last first: a
/// move made the other way, a copy deleted while it is as it was made. Stops at the first that
/// cannot be made, before making it. A moved script's settings and tab follow it.
let private changeFiles (env: Env) (model: Model) (back: bool) (edits: History.FileEdit list) : Result<unit, string> =
    let move (source: string) (target: string) =
        if File.Exists target then failwith $"{Path.GetFileName target} is there already"
        File.Move(source, target)

        // The project's settings name the open script by its file.
        let dir = Path.GetDirectoryName source

        match Project.read dir with
        | Some s when s.Script = Path.GetFileName source -> Project.write dir { s with Script = Path.GetFileName target }
        | _ -> ()

        env.Tabs.Renamed source target

    try
        for e in (if back then List.rev edits else edits) do
            match e with
            | History.Moved(source, target) -> if back then move target source else move source target
            | History.Copied(_, target, bytes) when back ->
                if model.OpenTabs.TryFind target = Some true then failwith $"{Path.GetFileName target} has unsaved changes"

                if File.Exists target then
                    if File.ReadAllBytes target <> bytes then failwith $"{Path.GetFileName target} has changed since it was made"
                    File.Delete target

                env.Tabs.Deleted [ target ]
            | History.Copied(_, target, bytes) ->
                if File.Exists target then failwith $"{Path.GetFileName target} is there already"
                File.WriteAllBytes(target, bytes)

        Ok()
    with e ->
        Error e.Message

/// This app's version, as its package names it.
let tambourVersion =
    let assembly = typeof<Env>.Assembly

    match assembly.GetCustomAttributes(typeof<Reflection.AssemblyInformationalVersionAttribute>, false) with
    | [| :? Reflection.AssemblyInformationalVersionAttribute as v |] -> v.InformationalVersion.Split('+').[0]
    | _ -> string (assembly.GetName().Version)

/// Help → About: what this is, which Marey it runs, and where its licences are.
let private aboutWindow (look: Palette.Look) =
    let close = Button(Content = "Close", HorizontalAlignment = HorizontalAlignment.Right, IsDefault = true, IsCancel = true)

    let text =
        TextBlock(
            Text =
                $"Tambour {tambourVersion}\nScripts run on Marey {Project.mareyVersion}.\n\nMPL-2.0. The libraries it carries, and their licences, are listed in THIRD-PARTY-NOTICES.md beside it.",
            TextWrapping = TextWrapping.Wrap,
            Foreground = SolidColorBrush look.Text
        )

    let panel = StackPanel(Spacing = Tokens.Space.s3, Margin = Thickness Tokens.Space.s4)
    panel.Children.Add text
    panel.Children.Add close

    let window =
        Window(Title = "About Tambour", Width = 420.0, SizeToContent = SizeToContent.Height, CanResize = false, Content = panel, WindowStartupLocation = WindowStartupLocation.CenterOwner)

    close.Click.Add(fun _ -> window.Close())
    window

let private tableEdited (env: Env) (edits: Tambour.Data.TableEdit list) (model: Model) : Model * Cmd<Msg> =
    let tables = edits |> List.fold Tambour.Data.TableEdit.apply (tablesOf model)
    let schema = edits |> List.exists Tambour.Data.TableEdit.changesSchema

    toState
        env
        { model with History = History.recordTables edits model.History; ActionNote = "" }
        { Script = model.Script; Tables = tables }
        schema

/// A table added and shown. Its tab is chosen once the table is there: a tab asked for before it
/// is drawn leaves the pane on the figure.
let private addShown (env: Env) (table: Tambour.Data.Table) (model: Model) : Model * Cmd<Msg> =
    let next, cmd = tableEdited env [ Tambour.Data.AddTable table ] model

    match next.Tables |> List.tryFindIndex (fun s -> s.Table.Name = table.Name) with
    | Some i -> { next with Tab = i + 1; Header = None }, cmd
    | None -> next, cmd

let update (env: Env) (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | Edited(edit, text) ->
        { model with
            Script = text
            ActionNote = ""
            History = History.record DateTime.UtcNow true edit model.History },
        Cmd.none
    | Undo ->
        match History.undo model.History with
        | Some(entry, history) ->
            let files = History.Entry.files entry

            match changeFiles env model true files with
            | Error why -> { model with ActionNote = $"not undone: {why}" }, Cmd.none
            | Ok() ->
                let state = History.Entry.undo { Script = model.Script; Tables = tablesOf model } entry
                let next, cmd = toState env { model with History = history; ActionNote = "undone" } state (History.Entry.changesSchema entry)
                next, (if files.IsEmpty then cmd else Cmd.batch [ cmd; Cmd.ofMsg FilesChanged ])
        | None -> model, Cmd.none
    | Redo ->
        match History.redo model.History with
        | Some(entry, history) ->
            let files = History.Entry.files entry

            match changeFiles env model false files with
            | Error why -> { model with ActionNote = $"not redone: {why}" }, Cmd.none
            | Ok() ->
                let state = History.Entry.redo { Script = model.Script; Tables = tablesOf model } entry
                let next, cmd = toState env { model with History = history; ActionNote = "redone" } state (History.Entry.changesSchema entry)
                next, (if files.IsEmpty then cmd else Cmd.batch [ cmd; Cmd.ofMsg FilesChanged ])
        | None -> model, Cmd.none
    | Run ->
        { model with Running = true; Status = "running…"; Playing = Set.empty },
        send env (Request.Evaluate { Text = model.Script; Path = Option.toObj model.Path })
    // An export ends as a run does, with the worker doing it; its reply, if one comes, is let go.
    | Cancel when model.Exporting.IsSome ->
        { model with Exporting = None; Status = "export cancelled" }, Cmd.OfTask.attempt env.Workers.CancelAsync () Failed
    | Cancel when model.Running ->
        { model with Running = false; Status = "cancelled" }, Cmd.OfTask.attempt env.Workers.CancelAsync () Failed
    // With nothing running, Escape leaves the full view.
    | Cancel when model.FullView -> { model with FullView = false }, Cmd.none
    | Cancel -> model, Cmd.none
    | Replied reply ->
        match reply with
        | Reply.Ready _ -> model, Cmd.none
        | Reply.Evaluated e ->
            let status =
                match e.Ok, e.Exception with
                | true, _ -> $"ran in {e.Timings.Evaluate:F0} ms"
                | false, null -> "the script does not compile"
                | false, ex -> $"the script threw: {ex}"

            // A project from another Marey is run on this one, and what does not compile is said
            // to be that, rather than migrated.
            let status =
                match model.Settings with
                | Some s when not e.Ok && s.Marey <> Project.mareyVersion ->
                    $"{status}: the project was last run on Marey {s.Marey}, and this is {Project.mareyVersion}"
                | _ -> status

            let next =
                { model with
                    Running = false
                    Figures = e.Figures
                    Selected = e.Selected
                    Picture = (if e.Selected < 0 then null else model.Picture)
                    Controls = (if e.Selected < 0 then [||] else model.Controls)
                    Diagnostics = e.Diagnostics
                    Actions = (if e.Selected < 0 then [||] else model.Actions)
                    Status = status }

            // A figure just written for the selected columns is the one to show.
            match next.ShowFigure |> Option.bind (fun name -> e.Figures |> Array.tryFindIndex (fun f -> f.Name = name)) with
            | Some i when e.Ok ->
                { next with ShowFigure = None }, (if i <> e.Selected then Cmd.ofMsg (Select i) else loadActions env next)
            | _ -> next, (if e.Ok then loadActions env next else Cmd.none)
        | Reply.Scene frame ->
            match Picture.Decode frame with
            | Ok p -> { model with Picture = p; Readout = None; Marks = [||] }, Cmd.none
            | Error e -> { model with Status = $"a scene did not decode: {e}" }, Cmd.none
        | Reply.Patch frame when not (isNull model.Picture) && frame.Figure = model.Picture.Figure ->
            match model.Picture.Apply frame with
            | Ok p -> { model with Picture = p }, Cmd.none
            | Error e -> { model with Status = $"a patch did not decode: {e}" }, Cmd.none
        | Reply.Patch _ -> model, Cmd.none
        | Reply.Controls c -> { model with Controls = c.Controls }, Cmd.none
        | Reply.Readout r -> { model with Readout = Some r }, Cmd.none
        | Reply.Output o -> { model with Output = appendOutput model o.Text }, Cmd.none
        | Reply.Exported frame ->
            match model.Exporting with
            | Some path ->
                try
                    File.WriteAllBytes(path, frame.Bytes)
                    { model with Exporting = None; Status = $"exported {Path.GetFileName path}" }, Cmd.none
                with e ->
                    { model with Exporting = None; Status = $"the export was not written: {e.Message}" }, Cmd.none
            | None -> model, Cmd.none
        | Reply.Failed message -> { model with Running = false; Exporting = None; Status = message }, Cmd.none
        | Reply.Picked located ->
            match model.Picking with
            | Some(plotted, area) ->
                match model.Tables |> List.tryFind (fun s -> Tambour.Data.Prelude.identifier s.Table.Name = plotted.Rows || s.Table.Name = plotted.Rows) with
                | None -> { model with Picking = None; ActionNote = $"no table binds {plotted.Rows}" }, Cmd.none
                | Some source ->
                    let rows =
                        if area.Width = 0.0 && area.Height = 0.0 then
                            Plotting.nearest source.Table plotted.X plotted.Y [| located.Values.[0], located.Values.[1] |]
                            |> Array.choose id
                            |> List.ofArray
                        elif located.Values |> Array.exists Double.IsNaN then
                            []
                        else
                            Plotting.inside source.Table plotted.X plotted.Y (located.Values.[0], located.Values.[1]) (located.Values.[2], located.Values.[3])

                    let note =
                        match rows with
                        | [] -> "no row picked there"
                        | [ r ] -> $"picked row {r + 1} of {source.Table.Name}"
                        | rs -> $"picked {rs.Length} rows of {source.Table.Name}"

                    let data = Plotting.dataOf source.Table plotted.X plotted.Y (List.truncate 10_000 rows) |> List.collect (fun (x, y) -> [ x; y ]) |> Array.ofList

                    { model with Picking = None; Marked = Some(source.Table.Name, Set.ofList rows); ActionNote = note },
                    (if data.Length > 0 then send env (Request.Place { Values = data; Clamp = false }) else Cmd.none)
            | None -> model, Cmd.none
        | Reply.Placed located ->
            { model with Marks = Array.init (located.Values.Length / 2) (fun i -> Point(located.Values.[2 * i], located.Values.[2 * i + 1])) }, Cmd.none
    | Select index when index <> model.Selected && index >= 0 ->
        let next = { model with Selected = index; Playing = Set.empty; Actions = [||] }
        next, Cmd.batch [ send env (Request.Select index); loadActions env next ]
    | Select _ -> model, Cmd.none
    | Input input -> model, send env (Request.Live input)
    | SetControl(index, value) ->
        match Array.tryItem index model.Controls with
        | Some c when c.Value <> value ->
            let controls = Array.copy model.Controls
            controls.[index] <- { c with Value = value }
            { model with Controls = controls }, send env (Request.Live { Kind = "set"; Args = [| float index; value |] })
        | _ -> model, Cmd.none
    | TogglePlay index ->
        let playing = if model.Playing.Contains index then model.Playing.Remove index else model.Playing.Add index
        { model with Playing = playing }, Cmd.none
    | Tick seconds ->
        let mutable playing = model.Playing

        let moves =
            [ for index in model.Playing do
                  match Array.tryItem index model.Controls with
                  | Some c when c.Kind = "time" ->
                      let next = c.Value + seconds

                      if next <= c.Hi then
                          yield SetControl(index, next)
                      elif c.Loop then
                          yield SetControl(index, c.Lo)
                      else
                          playing <- playing.Remove index
                          yield SetControl(index, c.Hi)
                  | _ -> playing <- playing.Remove index ]

        { model with Playing = playing }, moves |> List.map Cmd.ofMsg |> Cmd.batch
    | Problems problems -> { model with Problems = problems }, Cmd.none
    | GoTo(line, column) ->
        let serial = match model.GoTo with Some(s, _, _) -> s + 1 | None -> 0
        { model with GoTo = Some(serial, line, column) }, Cmd.none
    | ActionsLoaded(script, a) when script = model.Script -> { model with Actions = a.Outcomes; Columns = a.Columns }, Cmd.none
    | ActionsLoaded _ -> model, Cmd.none
    | Typed(field, text) -> { model with Inputs = model.Inputs.Add(field, text) }, Cmd.none
    | Act spec ->
        match editable model with
        | Some name ->
            let script = model.Script
            model, Cmd.OfTask.either (fun () -> env.Language.ActAsync(script, null, name, spec)) () Acted Failed
        | None -> model, Cmd.none
    | Acted a when a.Outcome.Outcome = "refused" -> { model with ActionNote = $"not done: {a.Outcome.Note}" }, Cmd.none
    | Acted a ->
        let note =
            if a.Outcome.Outcome = "fallback" then
                $"{a.Outcome.Note}: the stage applies to the whole expression above it"
            else
                a.Outcome.Note

        { model with
            Script = a.Text
            ActionNote = note
            History = History.record DateTime.UtcNow false (History.TextEdit.between model.Script a.Text) model.History },
        Cmd.ofMsg Run
    | OpenPath path ->
        try
            // A `.tambour` opens from its cache, a workspace directory like any other; a script there
            // stays in its bundle.
            let path = Path.GetFullPath path

            let bundle, path, writtenBack =
                if Bundle.isBundle path then
                    match Bundle.openBundle path with
                    | Ok o ->
                        Project.makeWorkspace o.Directory
                        Some path, o.Directory, o.WrittenBack
                    | Error why -> failwith why
                else
                    Bundle.ofDirectory (if Directory.Exists path then path else Path.GetDirectoryName path), path, false

            match model.Bundle with
            | Some before when Some before <> bundle -> Bundle.close before
            | _ -> ()

            let opened = Project.resolve path
            env.Tabs.Opened(bundle |> Option.defaultValue path)

            // The project's tables, their prelude written if it changed, and their columns sent.
            let tables, unread =
                match opened.Project with
                | Some dir ->
                    let tables, unread = Tables.load dir
                    if not tables.IsEmpty then Tables.writePrelude dir tables |> ignore
                    tables, unread
                | None -> [], []

            // A new script begins empty: its project's tables are in reach without a line of it.
            let text = if File.Exists opened.Script then File.ReadAllText opened.Script else ""

            let note =
                match opened.Settings, unread with
                | _, why :: _ -> $"a table did not read: {why}"
                | _ when writtenBack -> "what was saved here last time is written back"
                | Some s, [] when s.Marey <> Project.mareyVersion -> $"last run on Marey {s.Marey}; this is {Project.mareyVersion}"
                | _ -> ""

            { model with
                Script = text
                Saved = text
                Path = Some opened.Script
                Project = opened.Project
                Settings = opened.Settings
                Bundle = bundle
                Files = (match opened.Project with Some dir -> Project.scripts dir |> Array.map (fun n -> Path.Combine(dir, n)) |> List.ofArray | None -> [])
                Split = (match opened.Settings with Some s when s.Split > 0.05 && s.Split < 0.95 -> s.Split | _ -> model.Split)
                Bottom = (match opened.Settings with Some s when s.Bottom > 0.0 -> s.Bottom | _ -> model.Bottom)
                Layouts =
                    match opened.Settings with
                    | Some s ->
                        let orEmpty (a: 'a[]) = if isNull a then [||] else a
                        Map.ofList
                            [ for c in s.Columns do
                                  c.Table,
                                  { Widths = Map.ofList [ for w in orEmpty c.Widths -> w.Column, w.Width ]
                                    Hidden = Set.ofArray (orEmpty c.Hidden)
                                    Collapsed = Set.ofArray (orEmpty c.Collapsed) } ]
                    | None -> Map.empty
                History = History.empty
                Figures = [||]
                Selected = -1
                Picture = null
                Readout = None
                Controls = [||]
                Actions = [||]
                Diagnostics = [||]
                Output = ""
                ActionNote = note
                Tables = tables
                SavedTables = tables |> List.map (fun s -> s.Table.Name, s.Table) |> Map.ofList
                TableFiles = tables |> List.map (fun s -> s.File)
                Tab = 0
                Header = None },
            // In order, then the run, which needs them.
            Cmd.OfTask.either
                (fun () ->
                    task {
                        for t in tables do
                            do! env.Workers.SendAsync(Tables.request t.Table)
                    })
                ()
                (fun () -> Run)
                Failed
        with e ->
            { model with Status = $"{path} did not open: {e.Message}" }, Cmd.none
    | ChooseOpen folder -> model, Cmd.OfTask.either (fun () -> pickOpen (topOf env) folder) () (Option.map OpenTab >> Option.defaultValue Dismissed) Failed
    | Save ->
        match model.Path with
        | Some path ->
            try
                // A lone script given tables of its own: its directory becomes its workspace, where
                // the tables are kept.
                let model =
                    if model.Project.IsNone && not model.Tables.IsEmpty then
                        let dir = Path.GetDirectoryName path
                        Project.makeWorkspace dir
                        { model with Project = Some dir }
                    else
                        model

                File.WriteAllText(path, model.Script)
                writeProject model
                let model = saveTables model

                match model.Bundle, model.Project with
                | Some file, Some dir ->
                    if not (isNull model.Picture) then writeThumbnail dir model.Picture
                    Bundle.sync file |> ignore
                | _ -> ()

                let name = model.Bundle |> Option.defaultValue path |> Path.GetFileName
                { model with Saved = model.Script; Status = $"saved {name}" },
                (if Preferences.Live.Current.RunOnSave then Cmd.ofMsg Run else Cmd.none)
            with e ->
                { model with Status = $"not saved: {e.Message}" }, Cmd.none
        | None ->
            model, Cmd.OfTask.either (fun () -> pickSave (topOf env) "Save the workspace" ("figure" + Bundle.Extension) "tambour") () (Option.map SaveAs >> Option.defaultValue Dismissed) Failed
    | SaveAs path when Bundle.isBundle path ->
        // A new workspace is one file, its script named as it: unpacked into an empty cache, which
        // the save then fills and writes out.
        try
            let path = Path.GetFullPath path

            if model.Bundle = Some path then model, Cmd.ofMsg Save else

            match Bundle.create path with
            | Error why -> { model with Status = $"not saved: {why}" }, Cmd.none
            | Ok dir ->
                Project.makeWorkspace dir

                match model.Bundle with
                | Some before when before <> path -> Bundle.close before
                | _ -> ()

                env.Tabs.Opened path

                { model with
                    Path = Some(Path.Combine(dir, Path.GetFileNameWithoutExtension path + ".fsx"))
                    Project = Some dir
                    Bundle = Some path },
                Cmd.batch [ Cmd.ofMsg Save; Cmd.ofMsg FilesChanged ]
        with e ->
            { model with Status = $"not saved: {e.Message}" }, Cmd.none
    | SaveAs path ->
        // Saved as a script: a workspace directory of its own, named as it, with the script, its
        // tables in `tables/`, and `workspace.json`.
        try
            let name = Path.GetFileNameWithoutExtension path
            let dir = Path.Combine(Path.GetDirectoryName path, name)
            Directory.CreateDirectory dir |> ignore
            Project.makeWorkspace dir
            env.Tabs.Opened dir
            { model with Path = Some(Path.Combine(dir, name + ".fsx")); Project = Some dir }, Cmd.batch [ Cmd.ofMsg Save; Cmd.ofMsg FilesChanged ]
        with e ->
            { model with Status = $"not saved: {e.Message}" }, Cmd.none
    | DiskChanged ->
        match model.Path with
        | Some path when File.Exists path ->
            let text = try File.ReadAllText path with _ -> model.Saved

            if text = model.Saved || text = model.Script then
                { model with Saved = text }, Cmd.none
            else
                // Someone else's change, as one entry of the history, so it can be taken back.
                { model with
                    Script = text
                    Saved = text
                    ActionNote = "reloaded from disk"
                    History = History.record DateTime.UtcNow false (History.TextEdit.between model.Script text) model.History },
                Cmd.ofMsg Run
        | _ -> model, Cmd.none
    | ChooseExport ->
        let format = model.Inputs.TryFind "export format" |> Option.defaultValue Preferences.Live.Current.ExportFormat
        let name = (model.Path |> Option.map Path.GetFileNameWithoutExtension |> Option.defaultValue "figure") + "." + format

        let size field fallback =
            match Double.TryParse(model.Inputs.TryFind field |> Option.defaultValue "", Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
            | true, v when v > 0.0 -> v
            | _ -> fallback

        let width, height =
            match model.Picture with
            | null -> size "export width" 800.0, size "export height" 500.0
            | p -> size "export width" (float p.Extent.Width), size "export height" (float p.Extent.Height)

        model,
        Cmd.OfTask.either
            (fun () -> pickSave (topOf env) "Export the figure" name format)
            ()
            (Option.map (fun path -> ExportTo(path, format, width, height)) >> Option.defaultValue Dismissed)
            Failed
    | ExportTo(path, format, width, height) when not (isNull model.Picture) ->
        { model with Exporting = Some path; Status = $"exporting {Path.GetFileName path}…" },
        send env (Request.Export { Format = format; Width = width; Height = height; Skia = skiaOf model })
    | ExportTo _ -> { model with Status = "there is no figure to export" }, Cmd.none
    | Dismissed -> model, Cmd.none
    | SelectTab tab -> { model with Tab = tab; Header = None }, Cmd.none
    | TableEdited edits -> tableEdited env edits model
    | EditHeader(table, column) ->
        let header =
            model.Tables
            |> List.tryFind (fun s -> s.Table.Name = table)
            |> Option.bind (fun s -> List.tryItem column s.Table.Columns)

        let inputs =
            match header with
            | Some c ->
                model.Inputs
                    .Add("header name", c.Name)
                    .Add("header unit", (if isNull c.Unit then "" else c.Unit))
                    .Add("header type", c.Values.Kind)
            | None -> model.Inputs.Add("header name", "").Add("header unit", "").Add("header type", "float")

        { model with Header = Some(table, column); Inputs = inputs }, Cmd.none
    | CloseHeader -> { model with Header = None }, Cmd.none
    | ApplyHeader ->
        match model.Header |> Option.bind (fun (name, i) -> model.Tables |> List.tryFind (fun s -> s.Table.Name = name) |> Option.map (fun s -> s.Table, i)) with
        | None -> { model with Header = None }, Cmd.none
        | Some(t, i) ->
            let field f = model.Inputs.TryFind f |> Option.defaultValue ""
            let kind = field "header type"
            let unit = if kind = "float" && field "header unit" <> "" then field "header unit" else null

            let edit =
                if i < 0 then
                    Ok(Tambour.Data.TableEdit.addColumn t (min (-1 - i) t.Columns.Length) (field "header name") kind unit)
                else
                    retype t.Columns.[i].Values kind
                    |> Result.map (fun v -> Tambour.Data.TableEdit.changeColumn t i { Name = field "header name"; Unit = unit; Values = v })

            match edit with
            | Error why -> { model with ActionNote = $"not done: {why}" }, Cmd.none
            | Ok e ->
                let after = (Tambour.Data.TableEdit.apply (Map.ofList [ t.Name, t ]) e).[t.Name]

                match Tambour.Data.Table.create after.Name after.Columns with
                | Error why -> { model with ActionNote = $"not done: {why}" }, Cmd.none
                | Ok _ -> { model with Header = None }, Cmd.ofMsg (TableEdited [ e ])
    | NewTable ->
        let blank = Tambour.Data.Values.blank "float" 10
        let t: Tambour.Data.Table = { Name = freshName model "table"; Columns = [ { Name = "x"; Unit = null; Values = blank }; { Name = "y"; Unit = null; Values = blank } ] }
        addShown env t model
    | ChooseImport ->
        let pick () =
            task {
                let types = FilePickerFileType("Tables", Patterns = [| "*.csv"; "*.arrow" |])
                let! picked = (topOf env).StorageProvider.OpenFilePickerAsync(FilePickerOpenOptions(Title = "Import a table", FileTypeFilter = [| types |]))
                return picked |> Seq.tryHead |> Option.bind (fun f -> Option.ofObj (f.TryGetLocalPath()))
            }

        model, Cmd.OfTask.either pick () (Option.map Import >> Option.defaultValue Dismissed) Failed
    | Import path ->
        let read = if path.EndsWith ".arrow" then Tambour.Data.Arrow.readFile path else Tambour.Data.Csv.readFile path

        match read with
        | Error why -> { model with ActionNote = $"{Path.GetFileName path} did not import: {why}" }, Cmd.none
        | Ok t ->
            let named = { t with Name = freshName model (Tambour.Data.Prelude.identifier t.Name |> fun n -> n.Trim('`')) }
            addShown env named model
    | RemoveTable name ->
        match model.Tables |> List.tryFind (fun s -> s.Table.Name = name) with
        | Some s -> { model with Tab = 0 }, Cmd.ofMsg (TableEdited [ Tambour.Data.RemoveTable s.Table ])
        | None -> model, Cmd.none
    | RenameTable name ->
        model,
        Cmd.OfTask.either
            (fun () -> env.Tabs.AskName "Rename the table" name)
            ()
            (function
            | Some next -> RenameTableTo(name, next)
            | None -> Dismissed)
            Failed
    | RenameTableTo(name, next) ->
        let next = next.Trim()

        if next = name then
            model, Cmd.none
        elif next = "" || next.IndexOfAny(Path.GetInvalidFileNameChars()) >= 0 then
            { model with ActionNote = $"not renamed: \"{next}\" is not a table's name" }, Cmd.none
        elif model.Tables |> List.exists (fun s -> s.Table.Name = next) then
            { model with ActionNote = $"not renamed: there is a table {next} already" }, Cmd.none
        else
            match Tables.renameUses model.Script name next with
            | _, 0 -> model, Cmd.ofMsg (RenameTableNow(name, next, false))
            | _, uses ->
                let question = $"The script uses {name} in {uses} place(s). Rename them to {next} as well?"
                model, Cmd.OfTask.either (fun () -> env.Tabs.Confirm question "Rename them" "Leave them") () (fun yes -> RenameTableNow(name, next, yes)) Failed
    | RenameTableNow(name, next, uses) when model.Tables |> List.exists (fun s -> s.Table.Name = name) ->
        // The table and the script's uses of it, as one entry, so one undo takes back both.
        let edit = Tambour.Data.RenameTable(name, next)
        let script = if uses then fst (Tables.renameUses model.Script name next) else model.Script

        let entry =
            if script = model.Script then History.Tables [ edit ]
            else History.Group [ History.Tables [ edit ]; History.Script [ History.TextEdit.between model.Script script ] ]

        toState
            env
            { model with
                History = History.recordEntry entry model.History
                ActionNote = $"renamed {name} to {next}"
                Header = None
                TableSelection = None
                Marked = None }
            { Script = script; Tables = Tambour.Data.TableEdit.apply (tablesOf model) edit }
            true
    | RenameTableNow _ -> model, Cmd.none
    | DuplicateTable name ->
        match model.Tables |> List.tryFind (fun s -> s.Table.Name = name) with
        | Some s ->
            let copy = { s.Table with Name = freshName model (name + "_copy") }
            addShown env copy model
        | None -> model, Cmd.none
    | RenameShownTable ->
        model, (match List.tryItem (model.Tab - 1) model.Tables with Some s -> Cmd.ofMsg (RenameTable s.Table.Name) | None -> Cmd.none)
    | DuplicateShownTable ->
        model, (match List.tryItem (model.Tab - 1) model.Tables with Some s -> Cmd.ofMsg (DuplicateTable s.Table.Name) | None -> Cmd.none)
    | ChooseCopyTable ->
        match env.Tabs.OtherTables() with
        | [] -> { model with ActionNote = "no other tab has a table to copy" }, Cmd.none
        | others -> model, Cmd.OfTask.either env.Tabs.PickTable others CopyTable Failed
    | CopyTable None -> model, Cmd.none
    | SetSkia skia ->
        let settings = model.Settings |> Option.defaultValue (Project.defaults (model.Path |> Option.map Path.GetFileName |> Option.defaultValue ""))
        let next = { model with Settings = Some { settings with Skia = skia } }
        writeProject next
        next, Cmd.none
    | CopyTable(Some t) ->
        let copy = { t with Name = freshName model t.Name }
        let next, cmd = addShown env copy model
        { next with ActionNote = $"copied {t.Name} into this workspace" }, cmd
    | TableSelected(table, columns) -> { model with TableSelection = Some(table, columns) }, Cmd.none
    | PlotSelected ->
        match model.TableSelection |> Option.bind (fun (name, columns) -> model.Tables |> List.tryFind (fun s -> s.Table.Name = name) |> Option.map (fun s -> s.Table, columns)) with
        | None -> { model with ActionNote = "select two columns of a table first, the x and then the y" }, Cmd.none
        | Some(table, columns) ->
            match Plotting.plotColumns model.Script table columns with
            | Error why -> { model with ActionNote = $"not done: {why}" }, Cmd.none
            | Ok(text, name) ->
                { model with
                    Script = text
                    Tab = 0
                    ShowFigure = Some name
                    ActionNote = $"wrote {name}, a plot of {table.Name}"
                    History = History.record DateTime.UtcNow false (History.TextEdit.between model.Script text) model.History },
                Cmd.ofMsg Run
    | Pick area ->
        match editable model with
        | None -> { model with ActionNote = "picking reads a plot { } block of a table; this figure is not one" }, Cmd.none
        | Some name ->
            let script, path = model.Script, Option.toObj model.Path
            model, Cmd.OfTask.either (fun () -> env.Language.PlottedAsync(script, path, name)) () (fun p -> PickFor(p, area)) Failed
    | PickFor(plotted, _) when plotted.Rows = "" -> { model with ActionNote = "picking reads a plot { } block of a table; this figure is not one" }, Cmd.none
    | PickFor(plotted, area) ->
        let points =
            if area.Width = 0.0 && area.Height = 0.0 then [| area.X; area.Y |]
            else [| area.X; area.Y; area.Right; area.Bottom |]

        { model with Picking = Some(plotted, area) }, send env (Request.Pick { Values = points; Clamp = points.Length > 2 })
    | ChooseTheme theme ->
        App.Choose theme
        { model with Theme = theme }, Cmd.none
    | LookChanged look -> { model with Look = look }, Cmd.none
    | Caret(line, column) -> { model with Caret = (line, column) }, Cmd.none
    | ToggleBottom ->
        let next = { model with BottomOpen = not model.BottomOpen }
        keepPanes next
        next, Cmd.none
    | ShowBottom tab ->
        let next = { model with BottomOpen = true; BottomTab = tab }
        if not model.BottomOpen then keepPanes next
        next, Cmd.none
    | ToggleActions ->
        let next = { model with ActionsOpen = not model.ActionsOpen }
        keepPanes next
        next, Cmd.none
    | ToggleFullView -> { model with FullView = not model.FullView }, Cmd.none
    | OpenPreferences ->
        env.Tabs.Preferences()
        model, Cmd.none
    | PreferencesChanged -> model, Cmd.none
    | LaidOut(table, layout) ->
        let next = { model with Layouts = model.Layouts.Add(table, layout) }
        writeProject next
        next, Cmd.none
    | Grid kind ->
        // The grid of the table shown, the only one in the window's tree.
        env.Host.GetVisualDescendants() |> Seq.tryPick (function :? TableGrid as g -> Some g | _ -> None) |> Option.iter (fun g -> g.Run kind)
        model, Cmd.none
    | Resized(split, bottom) ->
        let next = { model with Split = split; Bottom = bottom }
        writeProject next
        next, Cmd.none
    | About ->
        match topOf env with
        | :? Window as owner -> model, Cmd.OfTask.attempt (fun () -> (aboutWindow model.Look).ShowDialog owner) () Failed
        | _ -> model, Cmd.none
    | Quit ->
        match topOf env with
        | :? Window as w -> w.Close()
        | _ -> ()

        model, Cmd.none
    | NewScript ->
        env.Tabs.New()
        model, Cmd.none
    | ChooseSaveAs ->
        let name = (model.Bundle |> Option.orElse model.Path |> Option.map Path.GetFileNameWithoutExtension |> Option.defaultValue "figure") + Bundle.Extension
        model, Cmd.OfTask.either (fun () -> pickSave (topOf env) "Save the workspace as" name "tambour") () (Option.map SaveAs >> Option.defaultValue Dismissed) Failed
    | SaveAll ->
        env.Tabs.SaveAll()
        model, Cmd.none
    | CloseProject ->
        match model.Project with
        | Some dir -> env.Tabs.CloseProject dir
        | None -> env.Tabs.Close()

        model, Cmd.none
    | ForgetRecent missingOnly ->
        env.Tabs.ForgetRecent missingOnly
        model, Cmd.none
    | RecentChanged recent -> { model with Recent = recent }, Cmd.none
    | FilesChanged ->
        let files =
            match model.Project with
            | Some dir when Directory.Exists dir -> Project.scripts dir |> Array.map (fun n -> Path.Combine(dir, n)) |> List.ofArray
            | _ -> []

        // Every file action reads the list again after it: a bundle takes what it changed.
        try
            model.Bundle |> Option.iter (Bundle.sync >> ignore)
            { model with Files = files }, Cmd.none
        with e ->
            { model with Files = files; Status = $"not written back: {e.Message}" }, Cmd.none
    | ToggleFiles ->
        let next = { model with FilesOpen = not model.FilesOpen }
        keepPanes next
        next, Cmd.none
    | TabsChanged tabs -> { model with OpenTabs = tabs }, Cmd.none
    // Taken by the loop's termination before it reaches here.
    | Ended -> model, Cmd.none
    | RenameFile path ->
        model,
        Cmd.OfTask.either
            (fun () -> env.Tabs.AskName "Rename the script" (Path.GetFileNameWithoutExtension path))
            ()
            (function
            | Some name -> RenameTo(path, name)
            | None -> Dismissed)
            Failed
    | RenameTo(path, name) ->
        let name = (if name.EndsWith ".fsx" then name.Substring(0, name.Length - 4) else name).Trim()
        let target = Path.Combine(Path.GetDirectoryName path, name + ".fsx")

        if name = "" || name.IndexOfAny(Path.GetInvalidFileNameChars()) >= 0 then
            { model with Status = $"not renamed: {name} is not a file's name" }, Cmd.none
        elif File.Exists target then
            { model with Status = $"not renamed: {Path.GetFileName target} is there already" }, Cmd.none
        else
            let edit = History.Moved(path, target)

            match changeFiles env model false [ edit ] with
            | Ok() ->
                { model with
                    Status = $"renamed {Path.GetFileName path} to {Path.GetFileName target}"
                    History = History.recordEntry (History.Files [ edit ]) model.History },
                Cmd.ofMsg FilesChanged
            | Error why -> { model with Status = $"not renamed: {why}" }, Cmd.none
    | Renamed target ->
        let settings = model.Settings |> Option.map (fun s -> { s with Script = Path.GetFileName target })
        { model with Path = Some target; Settings = settings }, Cmd.ofMsg FilesChanged
    | DuplicateFile path ->
        let dir = Path.GetDirectoryName path
        let stem = Path.GetFileNameWithoutExtension path

        let target =
            Seq.initInfinite (fun i -> Path.Combine(dir, (if i = 0 then $"{stem} copy.fsx" else $"{stem} copy {i + 1}.fsx")))
            |> Seq.find (File.Exists >> not)

        let edit = try Ok(History.Copied(path, target, File.ReadAllBytes path)) with e -> Error e.Message

        match edit |> Result.bind (fun e -> changeFiles env model false [ e ] |> Result.map (fun () -> e)) with
        | Ok e ->
            { model with
                Status = $"duplicated as {Path.GetFileName target}"
                History = History.recordEntry (History.Files [ e ]) model.History },
            Cmd.ofMsg FilesChanged
        | Error why -> { model with Status = $"not duplicated: {why}" }, Cmd.none
    | DeleteFile path ->
        // A workspace's only script takes its workspace with it: tables, settings and prelude.
        let dir = Path.GetDirectoryName path

        let files =
            if Project.isWorkspace dir && Project.scripts dir = [| Path.GetFileName path |] then
                [ path
                  yield! Directory.GetFiles(Path.Combine(dir, Project.TablesDirectory)) |> Array.sort
                  for f in [ Project.WorkspaceFile; Tambour.Data.Prelude.FileName ] do
                      let full = Path.Combine(dir, f)
                      if File.Exists full then full ]
            else
                [ path ]

        model, Cmd.OfTask.either (fun () -> env.Tabs.ConfirmDelete files) () (fun ok -> if ok then DeleteNow files else Dismissed) Failed
    | DeleteNow files ->
        try
            for f in files do
                if File.Exists f then File.Delete f

            env.Tabs.Deleted files
            { model with Status = $"deleted {files.Length} file(s)" }, Cmd.ofMsg FilesChanged
        with e ->
            { model with Status = $"not all deleted: {e.Message}" }, Cmd.ofMsg FilesChanged
    | RevealFile path ->
        try
            if OperatingSystem.IsWindows() then
                Diagnostics.Process.Start("explorer.exe", $"/select,\"{path}\"") |> ignore
            elif OperatingSystem.IsMacOS() then
                Diagnostics.Process.Start("open", [| "-R"; path |]) |> ignore
            else
                Diagnostics.Process.Start("xdg-open", [| Path.GetDirectoryName path |]) |> ignore

            model, Cmd.none
        with e ->
            { model with Status = $"not shown: {e.Message}" }, Cmd.none
    | RenameScript -> model, (match model.Path with Some p -> Cmd.ofMsg (RenameFile p) | None -> Cmd.none)
    | DuplicateScript -> model, (match model.Path with Some p -> Cmd.ofMsg (DuplicateFile p) | None -> Cmd.none)
    | DeleteScript -> model, (match model.Path with Some p -> Cmd.ofMsg (DeleteFile p) | None -> Cmd.none)
    // A script in a bundle is shown as the bundle, not as its place in the cache.
    | RevealScript -> model, (match model.Bundle |> Option.orElse model.Path with Some p -> Cmd.ofMsg (RevealFile p) | None -> Cmd.none)
    | ChooseExportFolder ->
        model,
        Cmd.OfTask.either
            (fun () ->
                task {
                    let! picked = (topOf env).StorageProvider.OpenFolderPickerAsync(FolderPickerOpenOptions(Title = "Export the workspace into a folder"))
                    return picked |> Seq.tryHead |> Option.bind (fun f -> Option.ofObj (f.TryGetLocalPath()))
                })
            ()
            (Option.map ExportFolder >> Option.defaultValue Dismissed)
            Failed
    | ExportFolder parent ->
        // Into a new folder named as the workspace, of what is saved: plain scripts and tables.
        match model.Project with
        | None -> model, Cmd.none
        | Some dir ->
            let name = model.Bundle |> Option.map Path.GetFileNameWithoutExtension |> Option.defaultValue (Path.GetFileName dir)
            let target = Path.Combine(parent, name)

            try
                if Directory.Exists target || File.Exists target then failwith $"{name} is there already"
                Bundle.export dir target
                let changed =
                    model.Script <> model.Saved
                    || model.Tables |> List.exists (fun t -> not (model.SavedTables.TryFind t.Table.Name |> Option.exists (fun s -> obj.ReferenceEquals(s, t.Table))))

                let left = if changed then "; what is not saved is not in it" else ""
                { model with ActionNote = $"exported to {target}{left}" }, Cmd.none
            with e ->
                { model with ActionNote = $"not exported: {e.Message}" }, Cmd.none
    | OpenTab path ->
        env.Tabs.Open path
        model, Cmd.none
    | CloseTab ->
        env.Tabs.Close()
        model, Cmd.none
    | CycleTab step ->
        env.Tabs.Cycle step
        model, Cmd.none
    | DetachTab ->
        env.Tabs.Detach()
        model, Cmd.none
    | Note text -> { model with ActionNote = text }, Cmd.none
    | Failed e -> { model with Status = e.Message }, Cmd.none

let private label (text: string) : IView =
    TextBlock.create [ TextBlock.text text; TextBlock.verticalAlignment VerticalAlignment.Center; TextBlock.margin (Thickness(0.0, 0.0, Tokens.Space.s2, 0.0)) ]

let private controlView (dispatch: Msg -> unit) (index: int) (c: ControlInfo) (playing: bool) : IView =
    let row (children: IView list) : IView =
        DockPanel.create [ DockPanel.margin (Thickness(Tokens.Space.s2, Tokens.Space.s1)); DockPanel.children children ]

    let shown = TextBlock.create [ TextBlock.dock Dock.Right; TextBlock.text c.Shown; TextBlock.width 90.0; TextBlock.margin (Thickness(Tokens.Space.s3, 0.0, 0.0, 0.0)); TextBlock.verticalAlignment VerticalAlignment.Center ]

    let slider =
        Slider.create [
            Slider.minimum c.Lo
            Slider.maximum c.Hi
            Slider.value c.Value
            Slider.onValueChanged (fun v -> dispatch (SetControl(index, v)))
        ]

    match c.Kind with
    | "slider" -> row [ TextBlock.create [ TextBlock.dock Dock.Left; TextBlock.text c.Label; TextBlock.width 120.0; TextBlock.verticalAlignment VerticalAlignment.Center ]; shown; slider ]
    | "time" ->
        row [
            Button.create [ Button.dock Dock.Left; Button.content (if playing then "Pause" else "Play"); Button.onClick (fun _ -> dispatch (TogglePlay index)) ]
            TextBlock.create [ TextBlock.dock Dock.Left; TextBlock.text c.Label; TextBlock.margin (Thickness(Tokens.Space.s2, 0.0)); TextBlock.verticalAlignment VerticalAlignment.Center ]
            shown
            slider
        ]
    | "select" ->
        row [
            TextBlock.create [ TextBlock.dock Dock.Left; TextBlock.text c.Label; TextBlock.width 120.0; TextBlock.verticalAlignment VerticalAlignment.Center ]
            ComboBox.create [
                ComboBox.dataItems c.Options
                ComboBox.selectedIndex (int c.Value)
                ComboBox.onSelectedIndexChanged (fun i -> if i >= 0 then dispatch (SetControl(index, float i)))
            ]
        ]
    | _ ->
        row [
            CheckBox.create [
                CheckBox.content c.Label
                CheckBox.isChecked (c.Value <> 0.0)
                CheckBox.onIsCheckedChanged (fun e ->
                    match e.Source with
                    | :? CheckBox as box -> dispatch (SetControl(index, (if box.IsChecked = Nullable true then 1.0 else 0.0)))
                    | _ -> ())
            ]
        ]

/// The themes, legend placements and geoms the panel offers.
let private themes = [| "publication"; "modern"; "presentation"; "notebook"; "dark"; "tikz" |]

let private placements =
    [| "LegendTopRight"; "LegendTopLeft"; "LegendBottomRight"; "LegendBottomLeft"; "LegendOutsideRight"; "LegendOutsideBottom"; "LegendBest"; "LegendNone" |]

let private geoms = [| "GeomPoint"; "GeomLine" |]

/// The GUI actions for the figure on screen. One the figure's shape refuses is greyed out with its
/// reason; one that would fall back says so.
let private actionsView (model: Model) (dispatch: Msg -> unit) : IView =
    let outcome kind = model.Actions |> Array.tryFind (fun o -> o.Kind = kind)
    let enabled kind = match outcome kind with Some o -> o.Outcome <> "refused" | None -> false

    let tip kind =
        match outcome kind with
        | Some o when o.Outcome = "refused" -> o.Note
        | Some o when o.Outcome = "fallback" -> o.Note + "; it will apply to the whole expression"
        | Some _ -> "writes the script"
        | None -> ""

    let spec kind (text: string) = Some { Kind = kind; Text = text; Lo = 0.0; Hi = 0.0 }

    /// A field that writes its action into the script on Enter.
    let input kind field (watermark: string) (apply: string -> ActionSpec option) : IView =
        TextBox.create [
            TextBox.placeHolderText watermark
            TextBox.verticalContentAlignment VerticalAlignment.Center
            TextBox.isEnabled (enabled kind)
            TextBox.tip (tip kind)
            TextBox.text (model.Inputs.TryFind field |> Option.defaultValue "")
            TextBox.onTextChanged (fun t -> if Some t <> model.Inputs.TryFind field then dispatch (Typed(field, t)))
            TextBox.onKeyDown (
                (fun e ->
                    if e.Key = Key.Enter then
                        match e.Source with
                        | :? TextBox as box ->
                            apply (Option.ofObj box.Text |> Option.defaultValue "") |> Option.iter (Act >> dispatch)
                            e.Handled <- true
                        | _ -> ()),
                SubPatchOptions.Always
            )
        ]

    let button kind (label: string) (spec: unit -> ActionSpec option) : IView =
        Button.create [
            Button.content label
            Button.isEnabled (enabled kind)
            Button.tip (tip kind)
            Button.verticalAlignment VerticalAlignment.Center
            // Every render's handler, since it reads what is typed now.
            Button.onClick ((fun _ -> spec () |> Option.iter (Act >> dispatch)), SubPatchOptions.Always)
        ]

    /// A menu that acts on a choice and then shows its label again.
    let menu kind (label: string) (items: string[]) : IView =
        ComboBox.create [
            ComboBox.placeholderText label
            ComboBox.minWidth 120.0
            ComboBox.verticalAlignment VerticalAlignment.Center
            ComboBox.dataItems items
            ComboBox.selectedIndex -1
            ComboBox.isEnabled (enabled kind && items.Length > 0)
            ComboBox.tip (tip kind)
            ComboBox.onSelectedIndexChanged (
                (fun i ->
                    if i >= 0 && i < items.Length then
                        dispatch (Act { Kind = kind; Text = items.[i]; Lo = 0.0; Hi = 0.0 })),
                SubPatchOptions.Always
            )
        ]

    /// The actions the bar has no room for, as one menu: each a submenu of its choices, or one entry.
    let more : IView =
        let choices kind (header: string) (items: string[]) : IView =
            MenuItem.create [
                MenuItem.header header
                MenuItem.isEnabled (enabled kind && items.Length > 0)
                MenuItem.tip (tip kind)
                MenuItem.viewItems [
                    for item in items do
                        MenuItem.create [ MenuItem.header item; MenuItem.onClick ((fun _ -> spec kind item |> Option.iter (Act >> dispatch)), SubPatchOptions.Always) ]
                ]
            ]

        Button.create [
            Button.name "more actions"
            Button.width Tokens.Height.control
            Button.height Tokens.Height.control
            Button.padding (Thickness 0.0)
            Button.verticalAlignment VerticalAlignment.Center
            Button.horizontalContentAlignment HorizontalAlignment.Center
            Button.verticalContentAlignment VerticalAlignment.Center
            Button.background Brushes.Transparent
            Button.tip "More: legend, colour by, facet by, fit"
            Button.contextMenu (
                ContextMenu.create [
                    ContextMenu.viewItems [
                        choices "legend" "_Legend" placements
                        choices "colour by" "_Colour by" model.Columns
                        choices "facet" "_Facet by" model.Columns
                        MenuItem.create [
                            MenuItem.header "_Fit"
                            MenuItem.isEnabled (enabled "fit")
                            MenuItem.tip (tip "fit")
                            MenuItem.onClick ((fun _ -> spec "fit" "" |> Option.iter (Act >> dispatch)), SubPatchOptions.Always)
                        ]
                    ]
                ]
            )
            Button.onClick (
                (fun e ->
                    match e.Source with
                    | :? Control as c when not (isNull c.ContextMenu) -> c.ContextMenu.Open c
                    | _ -> ()),
                SubPatchOptions.Always
            )
            Button.content (
                Avalonia.FuncUI.DSL.Path.create [
                    Shapes.Path.data (Geometry.Parse "M2.25,8 A1.25,1.25 0 1 1 2.26,8 Z M6.75,8 A1.25,1.25 0 1 1 6.76,8 Z M11.25,8 A1.25,1.25 0 1 1 11.26,8 Z")
                    Shapes.Path.fill (SolidColorBrush model.Look.Text)
                    Shapes.Path.width Tokens.Height.icon
                    Shapes.Path.height Tokens.Height.icon
                ]
            )
        ]

    let domain (_: string) =
        let parse field =
            match Double.TryParse(model.Inputs.TryFind field |> Option.defaultValue "", Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
            | true, v -> Some v
            | _ -> None

        match parse "lo", parse "hi" with
        | Some lo, Some hi -> Some { Kind = "y domain"; Text = ""; Lo = lo; Hi = hi }
        | _ ->
            dispatch (Failed(FormatException "a y domain wants two numbers"))
            None

    let note (text: string) : IView =
        TextBlock.create [ TextBlock.margin (Thickness Tokens.Space.s3); TextBlock.textWrapping TextWrapping.Wrap; TextBlock.foreground (SolidColorBrush model.Look.Dim); TextBlock.text text ]

    /// A row's label, in the column of labels.
    let rowLabel (text: string) : IView =
        TextBlock.create [
            TextBlock.width Tokens.Width.label
            TextBlock.verticalAlignment VerticalAlignment.Center
            TextBlock.foreground (SolidColorBrush model.Look.Dim)
            TextBlock.text text
        ]

    let labelled (text: string) (field: IView) : IView =
        DockPanel.create [
            DockPanel.children [
                Border.create [ Border.dock Dock.Left; Border.child (rowLabel text) ]
                field
            ]
        ]

    match Array.tryItem model.Selected model.Figures with
    | Some f when f.Controls > 0 ->
        note "This figure is inside interact { }: it is shown and driven, but the actions edit plot { } blocks and Chart.* pipelines only."
    | Some _ ->
        let small (width: float) (child: IView) : IView = Border.create [ Border.width width; Border.child child ]

        StackPanel.create [
            StackPanel.margin (Thickness Tokens.Space.s3)
            StackPanel.spacing Tokens.Space.s2
            StackPanel.children [
                labelled "Title" (input "title" "title" "title" (spec "title"))
                Grid.create [
                    Grid.columnDefinitions $"*,{Tokens.Space.s3},*"
                    Grid.children [
                        Border.create [ Grid.column 0; Border.child (labelled "x label" (input "x label" "x label" "x label" (spec "x label"))) ]
                        Border.create [ Grid.column 2; Border.child (labelled "y label" (input "y label" "y label" "y label" (spec "y label"))) ]
                    ]
                ]
                DockPanel.create [
                    DockPanel.children [
                        StackPanel.create [
                            StackPanel.dock Dock.Right
                            StackPanel.orientation Orientation.Horizontal
                            StackPanel.spacing Tokens.Space.s2
                            StackPanel.children [ menu "theme" "Theme" themes; menu "layer" "Add layer" geoms; more ]
                        ]
                        StackPanel.create [
                            StackPanel.orientation Orientation.Horizontal
                            StackPanel.spacing Tokens.Space.s2
                            StackPanel.children [
                                rowLabel "y axis"
                                button "log y" "Log" (fun () -> spec "log y" "")
                                small (2.0 * Tokens.Height.control) (input "y domain" "lo" "from" domain)
                                TextBlock.create [ TextBlock.text "–"; TextBlock.verticalAlignment VerticalAlignment.Center; TextBlock.foreground (SolidColorBrush model.Look.Dim) ]
                                small (2.0 * Tokens.Height.control) (input "y domain" "hi" "to" domain)
                            ]
                        ]
                    ]
                ]
            ]
        ]
    | None -> note "Run a script that binds a figure to edit it here."

/// Puts a text on the clipboard of the window a control is in.
let private copyFrom (source: obj) (text: string) =
    match source with
    | :? Visual as v ->
        match TopLevel.GetTopLevel v with
        | null -> ()
        | top -> top.Clipboard |> Option.ofObj |> Option.iter (fun board -> board.SetTextAsync text |> ignore)
    | _ -> ()

/// A problem in the Problems panel: a click goes to it; Ctrl+C or its menu copies it, or them all.
let private problemView (look: Palette.Look) (dispatch: Msg -> unit) (all: Diagnostic[]) (d: Diagnostic) : IView =
    let everything = String.Join("\n", all |> Array.map diagnosticLine)

    Button.create [
        Button.background Brushes.Transparent
        Button.horizontalAlignment HorizontalAlignment.Stretch
        Button.horizontalContentAlignment HorizontalAlignment.Left
        Button.padding (Thickness(Tokens.Space.s2, Tokens.border))
        Button.onClick ((fun _ -> dispatch (GoTo(d.StartLine, d.StartColumn))), SubPatchOptions.Always)
        Button.onKeyDown (
            (fun e ->
                if e.Key = Key.C && e.KeyModifiers = KeyModifiers.Control then
                    copyFrom e.Source (diagnosticLine d)
                    e.Handled <- true),
            SubPatchOptions.Always
        )
        Button.contextMenu (
            ContextMenu.create [
                ContextMenu.viewItems [
                    MenuItem.create [
                        MenuItem.header "Copy"
                        MenuItem.inputGesture (KeyGesture(Key.C, KeyModifiers.Control))
                        MenuItem.onClick ((fun e -> copyFrom e.Source (diagnosticLine d)), SubPatchOptions.Always)
                    ]
                    MenuItem.create [
                        MenuItem.header "Copy all problems"
                        MenuItem.onClick ((fun e -> copyFrom e.Source everything), SubPatchOptions.Always)
                    ]
                ]
            ]
        )
        Button.tip (diagnosticLine d)
        Button.content (
            TextBlock.create [
                // One line each; the whole message is the tip.
                TextBlock.text (Text.RegularExpressions.Regex.Replace(diagnosticLine d, @"\s+", " "))
                TextBlock.textWrapping TextWrapping.NoWrap
                TextBlock.textTrimming TextTrimming.CharacterEllipsis
                TextBlock.foreground (SolidColorBrush(if d.Severity = "error" then look.Error else look.Warning))
            ]
        )
    ]

/// The types a column can be.
let private columnTypes = [| "float"; "int"; "string"; "bool"; "date" |]

/// The form a column's header opens: its name, its unit (a float column's only) and its type, for
/// a column there is or one to add.
let rec private headerView (model: Model) (dispatch: Msg -> unit) (column: int) (shown: bool) : IView =
    let field f = model.Inputs.TryFind f |> Option.defaultValue ""

    let input name (watermark: string) (width: float) : IView =
        TextBox.create [
            TextBox.placeHolderText watermark
            TextBox.width width
            TextBox.text (field name)
            TextBox.onTextChanged ((fun t -> if t <> field name then dispatch (Typed(name, t))), SubPatchOptions.Always)
        ]

    StackPanel.create [
        StackPanel.dock Dock.Top
        StackPanel.isVisible shown
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing Tokens.Space.s2
        StackPanel.margin (Thickness(Tokens.Space.s2, Tokens.Space.s1))
        StackPanel.children [
            label (if column < 0 then "New column" else "Column")
            input "header name" "name" 140.0
            ComboBox.create [
                ComboBox.dataItems columnTypes
                ComboBox.selectedIndex (columnTypes |> Array.tryFindIndex ((=) (field "header type")) |> Option.defaultValue 0)
                ComboBox.onSelectedIndexChanged ((fun i -> if i >= 0 then dispatch (Typed("header type", columnTypes.[i]))), SubPatchOptions.Always)
            ]
            if field "header type" = "float" then input "header unit" "unit (V/m, km, ppm…)" 150.0
            Button.create [ Button.content "Apply"; Button.onClick (fun _ -> dispatch ApplyHeader) ]
            if column >= 0 then
                Button.create [
                    Button.content "Remove column"
                    Button.onClick ((fun _ ->
                        match model.Header with
                        | Some(name, i) ->
                            match model.Tables |> List.tryFind (fun s -> s.Table.Name = name) with
                            | Some s when s.Table.Columns.Length > 1 ->
                                dispatch CloseHeader
                                dispatch (TableEdited [ Tambour.Data.TableEdit.removeColumn s.Table i ])
                            | _ -> ()
                        | None -> ()), SubPatchOptions.Always)
                ]
            Button.create [ Button.content "Cancel"; Button.onClick (fun _ -> dispatch CloseHeader) ]
        ]
    ]

/// A table's tab: what can be done to it, the header's form when one is open, and its grid.
and private tableView (model: Model) (dispatch: Msg -> unit) (t: Tambour.Data.Table) : IView =
    let look = model.Look

    let glyph (data: string) (colour: Color) : IView =
        Avalonia.FuncUI.DSL.Path.create [
            Shapes.Path.data (Geometry.Parse data)
            Shapes.Path.width Tokens.Height.icon
            Shapes.Path.height Tokens.Height.icon
            Shapes.Path.verticalAlignment VerticalAlignment.Center
            Shapes.Path.stroke (SolidColorBrush colour)
            Shapes.Path.strokeThickness 1.5
            Shapes.Path.strokeLineCap PenLineCap.Round
            Shapes.Path.strokeJoin PenLineJoin.Round
        ]

    let plus = "M8,3.5 V12.5 M3.5,8 H12.5"
    let chevron = "M4.5,6.5 L8,10 L11.5,6.5"

    /// A button of the bar: an icon, a word, and a chevron where it opens a menu.
    let barButton (icon: string option) (word: string) (primary: bool) (menu: (string * Msg) list) (act: Msg option) : IView =
        let fore = if primary then look.Roles.OnAccent else look.Text

        Button.create [
            Button.height Tokens.Height.control
            Button.padding (Thickness(Tokens.Space.s2, 0.0, Tokens.Space.s3, 0.0))
            Button.verticalAlignment VerticalAlignment.Center
            Button.verticalContentAlignment VerticalAlignment.Center
            if primary then
                Button.background (SolidColorBrush look.Roles.AccentFill)
                Button.foreground (SolidColorBrush fore)
            if not menu.IsEmpty then
                Button.contextMenu (
                    ContextMenu.create [
                        ContextMenu.viewItems [
                            for header, msg in menu do
                                MenuItem.create [ MenuItem.header header; MenuItem.onClick ((fun _ -> dispatch msg), SubPatchOptions.Always) ]
                        ]
                    ]
                )
            Button.onClick (
                (fun e ->
                    match act, e.Source with
                    | Some msg, _ -> dispatch msg
                    | None, (:? Control as c) when not (isNull c.ContextMenu) -> c.ContextMenu.Open c
                    | _ -> ()),
                SubPatchOptions.Always
            )
            Button.content (
                StackPanel.create [
                    StackPanel.orientation Orientation.Horizontal
                    StackPanel.spacing Tokens.Space.s1
                    StackPanel.children [
                        match icon with
                        | Some data -> glyph data fore
                        | None -> ()
                        TextBlock.create [ TextBlock.text word; TextBlock.verticalAlignment VerticalAlignment.Center; TextBlock.foreground (SolidColorBrush fore) ]
                        if not menu.IsEmpty && act.IsNone then glyph chevron look.Dim
                    ]
                ]
            )
        ]

    let selected =
        match model.TableSelection with
        | Some(name, columns) when name = t.Name && not columns.IsEmpty ->
            let names = columns |> List.choose (fun c -> List.tryItem c t.Columns) |> List.map (fun c -> c.Name)
            " · " + String.Join(", ", names) + " selected"
        | _ -> ""

    let layout = model.Layouts.TryFind t.Name |> Option.defaultValue GridLayout.Empty
    let shownCount = t.Columns |> List.filter (fun c -> not (layout.Hidden.Contains c.Name)) |> List.length
    let filter = model.Inputs.TryFind "column filter" |> Option.defaultValue ""

    let laidOut (next: GridLayout) = dispatch (LaidOut(t.Name, next))

    // Every column with a check to show or hide it, filtered by name, and collapsed ones marked.
    let chooser : IView =
        Button.create [
            Button.dock Dock.Right
            Button.height Tokens.Height.control
            Button.margin (Thickness(0.0, 0.0, Tokens.Space.s1, 0.0))
            Button.padding (Thickness(Tokens.Space.s2, 0.0, Tokens.Space.s1, 0.0))
            Button.verticalContentAlignment VerticalAlignment.Center
            Button.tip "Choose the columns shown"
            Button.flyout (
                Flyout.create [
                    Flyout.content (
                        StackPanel.create [
                            StackPanel.width 280.0
                            StackPanel.spacing Tokens.Space.s2
                            StackPanel.children [
                                TextBox.create [
                                    TextBox.placeHolderText "Filter columns"
                                    TextBox.text filter
                                    TextBox.onTextChanged ((fun f -> if f <> filter then dispatch (Typed("column filter", f))), SubPatchOptions.Always)
                                ]
                                ScrollViewer.create [
                                    ScrollViewer.maxHeight (12.0 * Tokens.Height.row)
                                    ScrollViewer.content (
                                        StackPanel.create [
                                            StackPanel.children [
                                                for c in t.Columns do
                                                    if filter = "" || c.Name.Contains(filter, StringComparison.OrdinalIgnoreCase) then
                                                        let shown = not (layout.Hidden.Contains c.Name)

                                                        CheckBox.create [
                                                            CheckBox.isChecked shown
                                                            CheckBox.isEnabled (not shown || shownCount > 1)
                                                            CheckBox.onClick (
                                                                (fun _ ->
                                                                    laidOut { layout with Hidden = (if shown then layout.Hidden.Add c.Name else layout.Hidden.Remove c.Name) }),
                                                                SubPatchOptions.Always
                                                            )
                                                            CheckBox.content (
                                                                StackPanel.create [
                                                                    StackPanel.orientation Orientation.Horizontal
                                                                    StackPanel.spacing Tokens.Space.s2
                                                                    StackPanel.children [
                                                                        TextBlock.create [ TextBlock.text c.Name ]
                                                                        if not (String.IsNullOrEmpty c.Unit) then
                                                                            TextBlock.create [ TextBlock.text c.Unit; TextBlock.fontSize Tokens.Type.small; TextBlock.verticalAlignment VerticalAlignment.Center; TextBlock.foreground (SolidColorBrush look.Dim) ]
                                                                        if layout.Collapsed.Contains c.Name then
                                                                            TextBlock.create [ TextBlock.text "collapsed"; TextBlock.fontSize Tokens.Type.small; TextBlock.verticalAlignment VerticalAlignment.Center; TextBlock.foreground (SolidColorBrush look.Dim) ]
                                                                    ]
                                                                ]
                                                            )
                                                        ]
                                            ]
                                        ]
                                    )
                                ]
                                DockPanel.create [
                                    DockPanel.children [
                                        Button.create [
                                            Button.dock Dock.Right
                                            Button.content "Show all"
                                            Button.background Brushes.Transparent
                                            Button.onClick ((fun _ -> laidOut { layout with Hidden = Set.empty; Collapsed = Set.empty }), SubPatchOptions.Always)
                                        ]
                                        TextBlock.create [
                                            TextBlock.text $"{shownCount} of {t.Columns.Length} shown"
                                            TextBlock.verticalAlignment VerticalAlignment.Center
                                            TextBlock.fontSize Tokens.Type.small
                                            TextBlock.foreground (SolidColorBrush look.Dim)
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    )
                ]
            )
            Button.content (
                StackPanel.create [
                    StackPanel.orientation Orientation.Horizontal
                    StackPanel.spacing Tokens.Space.s1
                    StackPanel.children [
                        glyph "M2.5,3 H13.5 V13 H2.5 Z M6.17,3 V13 M9.83,3 V13" look.Text
                        TextBlock.create [ TextBlock.text "Columns"; TextBlock.verticalAlignment VerticalAlignment.Center ]
                        TextBlock.create [ TextBlock.text $"{shownCount} of {t.Columns.Length}"; TextBlock.verticalAlignment VerticalAlignment.Center; TextBlock.foreground (SolidColorBrush look.Dim) ]
                        glyph chevron look.Dim
                    ]
                ]
            )
        ]

    DockPanel.create [
        DockPanel.children [
            Border.create [
                Border.dock Dock.Top
                Border.padding (Thickness(Tokens.Space.s3, Tokens.Space.s2))
                Border.borderBrush (SolidColorBrush look.Line)
                Border.borderThickness (Thickness(0.0, 0.0, 0.0, Tokens.border))
                Border.child (
                    DockPanel.create [
                        DockPanel.children [
                            Button.create [
                                Button.dock Dock.Right
                                Button.width Tokens.Height.control
                                Button.height Tokens.Height.control
                                Button.padding (Thickness 0.0)
                                Button.background Brushes.Transparent
                                Button.horizontalContentAlignment HorizontalAlignment.Center
                                Button.verticalContentAlignment VerticalAlignment.Center
                                Button.tip "More: rename, duplicate, remove the table"
                                Button.contextMenu (
                                    ContextMenu.create [
                                        ContextMenu.viewItems [
                                            MenuItem.create [ MenuItem.header "_Rename…"; MenuItem.onClick ((fun _ -> dispatch (RenameTable t.Name)), SubPatchOptions.Always) ]
                                            MenuItem.create [ MenuItem.header "_Duplicate"; MenuItem.onClick ((fun _ -> dispatch (DuplicateTable t.Name)), SubPatchOptions.Always) ]
                                            MenuItem.create [ MenuItem.header "Re_move table"; MenuItem.onClick ((fun _ -> dispatch (RemoveTable t.Name)), SubPatchOptions.Always) ]
                                        ]
                                    ]
                                )
                                Button.onClick (
                                    (fun e ->
                                        match e.Source with
                                        | :? Control as c when not (isNull c.ContextMenu) -> c.ContextMenu.Open c
                                        | _ -> ()),
                                    SubPatchOptions.Always
                                )
                                Button.content (
                                    Avalonia.FuncUI.DSL.Path.create [
                                        Shapes.Path.data (Geometry.Parse "M2.25,8 A1.25,1.25 0 1 1 2.26,8 Z M6.75,8 A1.25,1.25 0 1 1 6.76,8 Z M11.25,8 A1.25,1.25 0 1 1 11.26,8 Z")
                                        Shapes.Path.fill (SolidColorBrush look.Text)
                                        Shapes.Path.width Tokens.Height.icon
                                        Shapes.Path.height Tokens.Height.icon
                                    ]
                                )
                            ]
                            chooser
                            StackPanel.create [
                                StackPanel.dock Dock.Left
                                StackPanel.orientation Orientation.Horizontal
                                StackPanel.spacing Tokens.Space.s2
                                StackPanel.children [
                                    barButton (Some plus) "Column" false [] (Some(EditHeader(t.Name, -1 - t.Columns.Length)))
                                    barButton
                                        (Some plus)
                                        "Rows"
                                        false
                                        [ for n in [ 1; 10; 100 ] ->
                                              (if n = 1 then "Add 1 row" else $"Add {n} rows"), TableEdited [ Tambour.Data.TableEdit.addRows t t.Rows n ] ]
                                        None
                                    Border.create [
                                        Border.width Tokens.border
                                        Border.height Tokens.Height.icon
                                        Border.verticalAlignment VerticalAlignment.Center
                                        Border.background (SolidColorBrush look.Line)
                                    ]
                                    Border.create [
                                        Border.tip "Plot selected columns (Ctrl+Shift+P)"
                                        Border.child (barButton (Some "M2.5,2.5 V13.5 H13.5 M5,10 L8,6.5 L10,8.5 L13.5,4.5") "Plot" true [] (Some PlotSelected))
                                    ]
                                ]
                            ]
                            // What the table holds and what is selected, in the room left, cut short there.
                            TextBlock.create [
                                TextBlock.text $"{t.Rows} rows · {t.Columns.Length} columns{selected}"
                                TextBlock.verticalAlignment VerticalAlignment.Center
                                TextBlock.margin (Thickness(Tokens.Space.s3, 0.0, Tokens.Space.s2, 0.0))
                                TextBlock.foreground (SolidColorBrush look.Dim)
                                TextBlock.textTrimming TextTrimming.CharacterEllipsis
                                TextBlock.tip "Right-click, or the Table menu, for what can be done to cells, rows and columns; Ctrl+click or Alt+drag in a figure picks rows."
                            ]
                        ]
                    ]
                )
            ]
            // Always there, and hidden when no header is being edited, so the grid after it keeps its
            // place among the children and is not made again.
            match model.Header with
            | Some(name, column) when name = t.Name -> headerView model dispatch column true
            | _ -> headerView model dispatch -1 false
            Sheet.create [
                Sheet.table t
                Sheet.marked (match model.Marked with Some(name, rows) when name = t.Name -> rows | _ -> Set.empty)
                Sheet.onSelectionChanged (fun columns -> dispatch (TableSelected(t.Name, columns)))
                Sheet.onEdited (TableEdited >> dispatch)
                Sheet.onRefused (fun why -> dispatch (Failed(FormatException why)))
                Sheet.onHeaderActivated (fun column -> dispatch (EditHeader(t.Name, column)))
                Sheet.onInsertColumnAt (fun at -> dispatch (EditHeader(t.Name, -1 - at)))
                Sheet.layout (model.Layouts.TryFind t.Name |> Option.defaultValue GridLayout.Empty)
                Sheet.onLayoutChanged (fun l -> dispatch (LaidOut(t.Name, l)))
            ]
        ]
    ]

/// An action of the window's: its menu and its name there, its shortcut and any other keys that do
/// the same, the message it sends, and whether it can be taken now. The menus, the toolbar and
/// the keys are all made from these, so a shortcut always does what its menu entry does.
type Command =
    { Menu: string
      Header: string
      Gesture: KeyGesture option
      Also: KeyGesture list
      Msg: Msg
      Enabled: Model -> bool
      /// Whether its keys act only while a table's grid has the focus (Ctrl+C there copies cells,
      /// and in the editor text).
      InGrid: bool }

/// The menus, in the menu bar's order.
let menus = [ "_File"; "_Edit"; "_Run"; "_Table"; "_View"; "_Help" ]

let commands: Command list =
    let always (_: Model) = true

    let c menu header (keys: string) (also: string list) msg enabled =
        { Menu = menu
          Header = header
          Gesture = (if keys = "" then None else Some(KeyGesture.Parse keys))
          Also = also |> List.map KeyGesture.Parse
          Msg = msg
          Enabled = enabled
          InGrid = false }

    [ c "_File" "_New script" "Ctrl+N" [] NewScript always
      c "_File" "_Open…" "Ctrl+O" [] (ChooseOpen false) always
      c "_File" "Open _folder…" "Ctrl+Shift+O" [] (ChooseOpen true) always
      c "_File" "_Save" "Ctrl+S" [] Save always
      c "_File" "Save _as…" "Ctrl+Shift+S" [] ChooseSaveAs always
      c "_File" "Save a_ll" "Ctrl+Alt+S" [] SaveAll always
      c "_File" "_Close tab" "Ctrl+W" [] CloseTab always
      c "_File" "Close _project" "" [] CloseProject (fun m -> m.Project.IsSome)
      c "_File" "Rena_me script…" "" [] RenameScript (fun m -> m.Path.IsSome)
      c "_File" "_Duplicate script" "" [] DuplicateScript (fun m -> m.Path.IsSome)
      c "_File" "Dele_te script…" "" [] DeleteScript (fun m -> m.Path.IsSome)
      c "_File" "Reveal in file _manager" "" [] RevealScript (fun m -> m.Path.IsSome)
      c "_File" "_Export figure…" "Ctrl+E" [] ChooseExport (fun m -> not (isNull m.Picture))
      c "_File" "Export as fo_lder…" "" [] ChooseExportFolder (fun m -> m.Project.IsSome)
      c "_File" "_Quit" "Ctrl+Q" [] Quit always
      c "_Edit" "_Undo" "Ctrl+Z" [] Undo (fun m -> not m.History.Done.IsEmpty)
      c "_Edit" "_Redo" "Ctrl+Y" [ "Ctrl+Shift+Z" ] Redo (fun m -> not m.History.Undone.IsEmpty)
      c "_Edit" "_Preferences…" "Ctrl+OemComma" [] OpenPreferences always
      c "_Run" "_Run" "F5" [ "Ctrl+Enter" ] Run always
      c "_Run" "_Cancel" "Escape" [] Cancel (fun m -> m.Running || m.Exporting.IsSome || m.FullView)
      c "_Table" "_New table" "Ctrl+T" [] NewTable always
      c "_Table" "_Import table…" "Ctrl+Shift+I" [] ChooseImport always
      c "_Table" "_Copy a table from another tab…" "" [] ChooseCopyTable always
      c "_Table" "_Rename table…" "" [] RenameShownTable (fun m -> m.Tab > 0)
      c "_Table" "D_uplicate table" "" [] DuplicateShownTable (fun m -> m.Tab > 0)
      c "_Table" "_Plot selected columns" "Ctrl+Shift+P" [] PlotSelected (fun m -> m.TableSelection.IsSome)
      for kind, header, keys in GridCommand.all do
          { c "_Table" header keys [] (Grid kind) (fun m -> m.Tab > 0) with InGrid = true }
      c "_View" "_Problems" "Ctrl+Shift+M" [] (ShowBottom 0) always
      c "_View" "_Output" "Ctrl+Shift+U" [] (ShowBottom 1) always
      c "_View" "_Bottom panel" "Ctrl+J" [] ToggleBottom always
      c "_View" "_Actions" "Ctrl+Shift+A" [] ToggleActions always
      c "_View" "_Files" "Ctrl+Shift+E" [] ToggleFiles (fun m -> m.Project.IsSome)
      c "_View" "F_ull view" "Ctrl+Shift+F" [] ToggleFullView always
      c "_View" "_Next tab" "Ctrl+Tab" [] (CycleTab 1) always
      c "_View" "Pre_vious tab" "Ctrl+Shift+Tab" [] (CycleTab -1) always
      c "_View" "Move tab to a _new window" "" [] DetachTab always
      c "_Help" "_About Tambour" "" [] About always ]

/// A command's keys now: as the user changed them in the preferences, or its own.
let gesturesOf (c: Command) =
    match Preferences.Live.Current.Keys.TryFind c.Header with
    | Some "" -> []
    | Some k -> (try [ KeyGesture.Parse k ] with _ -> [])
    | None -> Option.toList c.Gesture @ c.Also

/// The command a key and its modifiers take, if one does.
let commandFor (key: Key) (modifiers: KeyModifiers) =
    commands
    |> List.tryFind (fun c -> gesturesOf c |> List.exists (fun g -> g.Key = key && g.KeyModifiers = modifiers))

let private commandNamed (header: string) = commands |> List.find (fun c -> c.Header = header)

/// A small drawn icon: stroked in a colour, or a solid shape filled with it; faint while it
/// cannot be taken.
let private icon (look: Palette.Look) (data: string) (solid: bool) (colour: Color) (enabled: bool) : IView =
    let colour = if enabled then colour else look.Roles.Faint

    Avalonia.FuncUI.DSL.Path.create [
        Shapes.Path.data (Geometry.Parse data)
        Shapes.Path.width Tokens.Height.icon
        Shapes.Path.height Tokens.Height.icon
        Shapes.Path.verticalAlignment VerticalAlignment.Center
        if solid then
            Shapes.Path.fill (SolidColorBrush colour)
        else
            Shapes.Path.stroke (SolidColorBrush colour)
            Shapes.Path.strokeThickness 1.5
            Shapes.Path.strokeLineCap PenLineCap.Round
            Shapes.Path.strokeJoin PenLineJoin.Round
    ]

let private icons =
    Map.ofList
        [ "_Run", ("M4.5,2.5 L13.5,8 L4.5,13.5 Z", true)
          "_Cancel", ("M3.5,3.5 H12.5 V12.5 H3.5 Z", true)
          "_Undo", ("M5.5,3 L2,6.5 L5.5,10 M2,6.5 H10 A3.75,3.75 0 0 1 10,14 H7", false)
          "_Redo", ("M10.5,3 L14,6.5 L10.5,10 M14,6.5 H6 A3.75,3.75 0 0 0 6,14 H9", false)
          "_Export figure…", ("M8,2 V10 M4.5,5.5 L8,2 L11.5,5.5 M2.5,10 V13.5 H13.5 V10", false) ]

/// The script's name, as its tab and the window's title show it.
let scriptName (model: Model) = model.Path |> Option.map Path.GetFileName |> Option.defaultValue "untitled"

/// What a workspace holds that is not on disk, each named as the question before dropping it
/// names it: the script, and each table changed, added or removed since it was last saved.
let unsaved (model: Model) : string list =
    [ if model.Script <> model.Saved then scriptName model

      for s in model.Tables do
          match model.SavedTables.TryFind s.Table.Name with
          | Some t when obj.ReferenceEquals(t, s.Table) -> ()
          | _ -> $"the table {s.Table.Name}"

      for name in model.SavedTables.Keys do
          if not (model.Tables |> List.exists (fun s -> s.Table.Name = name)) then
              $"the removal of the table {name}" ]

/// What its tab is called: the bundle's name where it is one, the script's otherwise.
let workspaceName (model: Model) =
    match model.Bundle with
    | Some file -> Path.GetFileNameWithoutExtension file
    | None -> scriptName model

/// The window's title: the script's name, and its bundle's, marked while anything is unsaved.
let title (model: Model) =
    let mark = if (unsaved model).IsEmpty then "" else " •"

    match model.Bundle with
    | Some file -> $"{scriptName model} in {Path.GetFileName file}{mark} — Tambour"
    | None -> $"{scriptName model}{mark} — Tambour"

let view (env: Env) (model: Model) (dispatch: Msg -> unit) : IView =
    let look = model.Look
    let brush (c: Color) = SolidColorBrush c

    let output =
        String.Join("\n", model.Diagnostics |> Array.map diagnosticLine)
        + (if model.Diagnostics.Length > 0 && model.Output.Length > 0 then "\n" else "")
        + model.Output

    let menuItem (command: Command) : IView =
        MenuItem.create [
            MenuItem.header command.Header
            MenuItem.inputGesture (gesturesOf command |> List.tryHead |> Option.toObj)
            MenuItem.isEnabled (command.Enabled model)
            MenuItem.onClick ((fun _ -> dispatch command.Msg), SubPatchOptions.Always)
        ]

    let themeItem (theme: Preferences.Theme) (header: string) : IView =
        MenuItem.create [
            MenuItem.header header
            MenuItem.toggleType MenuItemToggleType.Radio
            MenuItem.groupName "theme"
            MenuItem.isChecked (model.Theme = theme)
            MenuItem.onClick ((fun _ -> dispatch (ChooseTheme theme)), SubPatchOptions.Always)
        ]

    let menuBar =
        Menu.create [
            Menu.verticalAlignment VerticalAlignment.Center
            Menu.viewItems [
                for name in menus do
                    MenuItem.create [
                        MenuItem.header name
                        MenuItem.viewItems [
                            for command in commands |> List.filter (fun c -> c.Menu = name) do
                                match command.Msg with
                                | Grid kind when kind = GridCommand.Copy || GridCommand.startsGroup kind -> Separator.create []
                                | _ -> ()

                                menuItem command

                                if command.Msg = ChooseOpen true then
                                    MenuItem.create [
                                        MenuItem.header "Open _recent"
                                        MenuItem.isEnabled (not model.Recent.IsEmpty)
                                        MenuItem.viewItems [
                                            for path in model.Recent do
                                                MenuItem.create [
                                                    MenuItem.header path
                                                    // Gone from disk: shown, not taken.
                                                    MenuItem.isEnabled (File.Exists path || Directory.Exists path)
                                                    MenuItem.onClick ((fun _ -> dispatch (OpenTab path)), SubPatchOptions.Always)
                                                ]
                                            Separator.create []
                                            MenuItem.create [
                                                MenuItem.header "Remove _missing"
                                                MenuItem.onClick ((fun _ -> dispatch (ForgetRecent true)), SubPatchOptions.Always)
                                            ]
                                            MenuItem.create [
                                                MenuItem.header "_Clear"
                                                MenuItem.onClick ((fun _ -> dispatch (ForgetRecent false)), SubPatchOptions.Always)
                                            ]
                                        ]
                                    ]
                            if name = "_View" then
                                Separator.create []
                                MenuItem.create [
                                    MenuItem.header "_Theme"
                                    MenuItem.viewItems [
                                        themeItem Preferences.Light "_Light"
                                        themeItem Preferences.Dark "_Dark"
                                        themeItem Preferences.FollowSystem "Follow _system"
                                    ]
                                ]
                        ]
                    ]
            ]
        ]

    /// A command's button on the bar: its icon alone, or with a word beside it.
    let tool (header: string) (word: string option) : IView =
        let command = commandNamed header
        let data, solid = icons.[header]
        let enabled = command.Enabled model
        let name = header.Replace("_", "").TrimEnd('…')
        let colour = if header = "_Run" then look.Roles.Success else look.Text

        Button.create [
            match word with
            | None ->
                Button.content (icon look data solid colour enabled)
                Button.width Tokens.Height.control
                Button.padding (Thickness 0.0)
            | Some word ->
                Button.content (
                    StackPanel.create [
                        StackPanel.orientation Orientation.Horizontal
                        StackPanel.spacing Tokens.Space.s1
                        StackPanel.children [
                            icon look data solid colour enabled
                            TextBlock.create [ TextBlock.text word; TextBlock.verticalAlignment VerticalAlignment.Center ]
                        ]
                    ]
                )
                Button.padding (Thickness(Tokens.Space.s2, 0.0, Tokens.Space.s3, 0.0))
            Button.height Tokens.Height.control
            Button.verticalAlignment VerticalAlignment.Center
            Button.horizontalContentAlignment HorizontalAlignment.Center
            Button.verticalContentAlignment VerticalAlignment.Center
            Button.isEnabled enabled
            Button.background Brushes.Transparent
            Button.tip (match gesturesOf command with g :: _ -> $"""{name} ({g.ToString("p", null)})""" | [] -> name)
            Button.onClick ((fun _ -> dispatch command.Msg), SubPatchOptions.Always)
        ]

    let separator () : IView =
        Border.create [
            Border.width Tokens.border
            Border.height Tokens.Height.icon
            Border.margin (Thickness(Tokens.Space.s1, 0.0))
            Border.verticalAlignment VerticalAlignment.Center
            Border.background (brush look.Line)
        ]

    // One bar: the menus, then the commands most taken.
    let toolbar =
        Border.create [
            Border.dock Dock.Top
            Border.height Tokens.Height.toolbar
            Border.background (brush look.Surface)
            Border.borderBrush (brush look.Line)
            Border.borderThickness (Thickness(0.0, 0.0, 0.0, Tokens.border))
            Border.child (
                StackPanel.create [
                    StackPanel.orientation Orientation.Horizontal
                    StackPanel.margin (Thickness(Tokens.Space.s2, 0.0))
                    StackPanel.spacing Tokens.Space.s1
                    StackPanel.children [
                        menuBar
                        separator ()
                        tool "_Run" None
                        tool "_Cancel" None
                        separator ()
                        tool "_Undo" None
                        tool "_Redo" None
                        separator ()
                        tool "_Export figure…" (Some "Export")
                        ComboBox.create [
                            ComboBox.dataItems exportFormats
                            ComboBox.verticalAlignment VerticalAlignment.Center
                            ComboBox.tip "the format Export writes"
                            ComboBox.selectedIndex (
                                let format = model.Inputs.TryFind "export format" |> Option.defaultValue Preferences.Live.Current.ExportFormat
                                exportFormats |> Array.tryFindIndex ((=) format) |> Option.defaultValue 0
                            )
                            ComboBox.onSelectedIndexChanged ((fun i -> if i >= 0 then dispatch (Typed("export format", exportFormats.[i]))), SubPatchOptions.Always)
                        ]
                        ComboBox.create [
                            ComboBox.minWidth 140.0
                            ComboBox.margin (Thickness(Tokens.Space.s2, 0.0, 0.0, 0.0))
                            ComboBox.verticalAlignment VerticalAlignment.Center
                            ComboBox.isVisible (model.Figures.Length > 1)
                            ComboBox.tip "the figure shown"
                            ComboBox.dataItems (model.Figures |> Array.map (fun f -> f.Name))
                            ComboBox.selectedIndex model.Selected
                            ComboBox.onSelectedIndexChanged (fun i -> dispatch (Select i))
                        ]
                        TextBlock.create [
                            TextBlock.text model.ActionNote
                            TextBlock.margin (Thickness(Tokens.Space.s2, 0.0, 0.0, 0.0))
                            TextBlock.verticalAlignment VerticalAlignment.Center
                            TextBlock.foreground (brush (if model.ActionNote.Contains "whole expression" || model.ActionNote.StartsWith "not done" then look.Warning else look.Dim))
                        ]
                    ]
                ]
            )
        ]

    let errors = model.Problems |> Array.filter (fun d -> d.Severity = "error") |> Array.length
    let warnings = model.Problems.Length - errors

    let statusText (text: string) (colour: Color) : IView =
        TextBlock.create [
            TextBlock.text text
            TextBlock.fontSize Tokens.Type.small
            TextBlock.lineHeight Tokens.Type.smallLine
            TextBlock.verticalAlignment VerticalAlignment.Center
            TextBlock.foreground (brush colour)
        ]

    let statusBar =
        Border.create [
            Border.dock Dock.Bottom
            Border.height Tokens.Height.status
            Border.background (brush look.Surface)
            Border.borderBrush (brush look.Line)
            Border.borderThickness (Thickness(0.0, Tokens.border, 0.0, 0.0))
            Border.child (
                DockPanel.create [
                    DockPanel.margin (Thickness(Tokens.Space.s3, 0.0))
                    DockPanel.children [
                        StackPanel.create [
                            StackPanel.dock Dock.Right
                            StackPanel.orientation Orientation.Horizontal
                            StackPanel.spacing Tokens.Space.s4
                            StackPanel.children [
                                Button.create [
                                    Button.padding (Thickness 0.0)
                                    Button.minHeight 0.0
                                    Button.verticalAlignment VerticalAlignment.Center
                                    Button.background Brushes.Transparent
                                    Button.tip "show the problems (Ctrl+Shift+M)"
                                    Button.onClick ((fun _ -> dispatch (ShowBottom 0)), SubPatchOptions.Always)
                                    Button.content (
                                        statusText
                                            (match errors, warnings with
                                             | 0, 0 -> "no problems"
                                             | e, 0 -> $"""{e} error{if e = 1 then "" else "s"}"""
                                             | 0, w -> $"""{w} warning{if w = 1 then "" else "s"}"""
                                             | e, w -> $"""{e} error{if e = 1 then "" else "s"}, {w} warning{if w = 1 then "" else "s"}""")
                                            (if errors > 0 then look.Error elif warnings > 0 then look.Warning else look.Dim)
                                    )
                                ]
                                statusText $"Ln {fst model.Caret}, Col {snd model.Caret}" look.Dim
                                statusText $"Marey {Project.mareyVersion}" look.Dim
                            ]
                        ]
                        // Something under way, a run or an export, which Cancel ends.
                        ProgressBar.create [
                            ProgressBar.dock Dock.Left
                            ProgressBar.isIndeterminate true
                            ProgressBar.isVisible (model.Running || model.Exporting.IsSome)
                            ProgressBar.width Tokens.Width.label
                            ProgressBar.minWidth 0.0
                            ProgressBar.height Tokens.Space.s1
                            ProgressBar.minHeight 0.0
                            ProgressBar.verticalAlignment VerticalAlignment.Center
                            ProgressBar.margin (Thickness(0.0, 0.0, Tokens.Space.s2, 0.0))
                        ]
                        statusText model.Status look.Text
                    ]
                ]
            )
        ]

    /// One of a pane's tabs in its header: the accent under the one shown.
    let paneTab (text: string) (glyph: string option) (count: int option) (selected: bool) (act: unit -> unit) : IView =
        Button.create [
            Button.background Brushes.Transparent
            Button.cornerRadius (CornerRadius 0.0)
            Button.padding (Thickness 0.0)
            Button.minHeight 0.0
            Button.height Tokens.Height.paneHeader
            Button.borderThickness (Thickness(0.0, 0.0, 0.0, 2.0 * Tokens.border))
            Button.borderBrush (if selected then brush look.Accent :> IBrush else Brushes.Transparent)
            Button.verticalContentAlignment VerticalAlignment.Center
            Button.onClick ((fun _ -> act ()), SubPatchOptions.Always)
            Button.content (
                StackPanel.create [
                    StackPanel.orientation Orientation.Horizontal
                    StackPanel.spacing Tokens.Space.s2
                    StackPanel.children [
                        match glyph with
                        | Some data -> icon look data false (if selected then look.Text else look.Dim) true
                        | None -> ()
                        TextBlock.create [
                            TextBlock.text text
                            TextBlock.verticalAlignment VerticalAlignment.Center
                            TextBlock.fontWeight FontWeight.Medium
                            TextBlock.foreground (brush (if selected then look.Text else look.Dim))
                        ]
                        match count with
                        | Some n ->
                            Border.create [
                                Border.minWidth Tokens.Space.s4
                                Border.height Tokens.Space.s4
                                Border.padding (Thickness(Tokens.Space.s1, 0.0))
                                Border.cornerRadius (CornerRadius Tokens.Space.s2)
                                Border.background (brush look.Raised)
                                Border.verticalAlignment VerticalAlignment.Center
                                Border.child (
                                    TextBlock.create [
                                        TextBlock.text (string n)
                                        TextBlock.fontSize Tokens.Type.small
                                        TextBlock.horizontalAlignment HorizontalAlignment.Center
                                        TextBlock.verticalAlignment VerticalAlignment.Center
                                        TextBlock.foreground (brush look.Dim)
                                    ]
                                )
                            ]
                        | None -> ()
                    ]
                ]
            )
        ]

    /// A small icon button of a pane's header, lit while what it shows is shown.
    let paneAction (data: string) (tip: string) (on: bool) (act: unit -> unit) : IView =
        Button.create [
            Button.width Tokens.Height.controlSm
            Button.height Tokens.Height.controlSm
            Button.minHeight 0.0
            Button.padding (Thickness 0.0)
            Button.verticalAlignment VerticalAlignment.Center
            Button.horizontalContentAlignment HorizontalAlignment.Center
            Button.verticalContentAlignment VerticalAlignment.Center
            Button.background (if on then brush look.Roles.Hover :> IBrush else Brushes.Transparent)
            Button.tip tip
            Button.onClick ((fun _ -> act ()), SubPatchOptions.Always)
            Button.content (icon look data false look.Text true)
        ]

    /// A pane's header: its tabs, what follows them, and its actions at the right.
    let paneHeader (tabs: IView list) (after: IView list) (actions: IView list) : IView =
        Border.create [
            Border.dock Dock.Top
            Border.height Tokens.Height.paneHeader
            Border.background (brush look.Surface)
            Border.borderBrush (brush look.Line)
            Border.borderThickness (Thickness(0.0, 0.0, 0.0, Tokens.border))
            Border.padding (Thickness(Tokens.Space.s3, 0.0, Tokens.Space.s2, 0.0))
            Border.child (
                DockPanel.create [
                    DockPanel.children [
                        StackPanel.create [
                            StackPanel.dock Dock.Right
                            StackPanel.orientation Orientation.Horizontal
                            StackPanel.spacing Tokens.Space.s1
                            StackPanel.children actions
                        ]
                        StackPanel.create [
                            StackPanel.orientation Orientation.Horizontal
                            StackPanel.spacing Tokens.Space.s4
                            StackPanel.children (tabs @ after)
                        ]
                    ]
                ]
            )
        ]

    let tableGlyph = "M2.5,3 H13.5 V13 H2.5 Z M2.5,6.5 H13.5 M6.5,6.5 V13"

    let bottomHeader =
        paneHeader
            [ paneTab "Problems" None (Some model.Problems.Length) (model.BottomTab = 0) (fun () -> dispatch (ShowBottom 0))
              paneTab "Output" None None (model.BottomTab = 1) (fun () -> dispatch (ShowBottom 1)) ]
            []
            [ paneAction "M4.5,6.5 L8,10 L11.5,6.5" "Hide the panel (Ctrl+J)" false (fun () -> dispatch ToggleBottom) ]

    let bottomPanel =
        TabControl.create [
            TabControl.classes [ "bare" ]
            TabControl.padding (Thickness 0.0)
            TabControl.isVisible model.BottomOpen
            TabControl.selectedIndex model.BottomTab
            TabControl.onSelectedIndexChanged ((fun i -> if i >= 0 && i <> model.BottomTab then dispatch (ShowBottom i)), SubPatchOptions.Always)
            TabControl.viewItems [
                TabItem.create [
                    TabItem.header (if model.Problems.Length = 0 then "Problems" else $"Problems ({model.Problems.Length})")
                    TabItem.content (
                        ScrollViewer.create [
                            ScrollViewer.content (
                                StackPanel.create [
                                    StackPanel.children [
                                        if model.Problems.Length = 0 then
                                            TextBlock.create [ TextBlock.margin (Thickness Tokens.Space.s3); TextBlock.text "No problems in the script."; TextBlock.foreground (brush look.Dim) ]
                                        for d in model.Problems do
                                            problemView look dispatch model.Problems d
                                    ]
                                ]
                            )
                        ]
                    )
                ]
                TabItem.create [
                    TabItem.header "Output"
                    TabItem.content (
                        TextBox.create [
                            TextBox.text output
                            TextBox.isReadOnly true
                            TextBox.fontFamily Palette.code
                            TextBox.textWrapping TextWrapping.Wrap
                        ]
                    )
                ]
            ]
        ]

    /// Where a divider was let go: the fractions and heights it left, sent to be kept.
    let dragged (e: VectorEventArgs) =
        match e.Source with
        | :? GridSplitter as splitter ->
            match splitter.Parent with
            | :? Grid as grid when grid.ColumnDefinitions.Count = 3 ->
                let a, b = grid.ColumnDefinitions.[0].ActualWidth, grid.ColumnDefinitions.[2].ActualWidth
                if a + b > 0.0 then dispatch (Resized(a / (a + b), model.Bottom))
            | :? Grid as grid when grid.RowDefinitions.Count = 3 ->
                let h = grid.RowDefinitions.[2].ActualHeight
                if h > 0.0 then dispatch (Resized(model.Split, h))
            | _ -> ()
        | _ -> ()

    let split = Math.Clamp(model.Split, 0.05, 0.95)
    let inv (v: float) = v.ToString(Globalization.CultureInfo.InvariantCulture)

    let fileGlyph = "M4,1.75 H9 L12,4.75 V14.25 H4 Z M9,1.75 V4.75 H12"

    /// A row of the project's list: an icon, a name, and what goes at its end.
    let listRow (indent: float) (glyph: string) (name: string) (nameColour: Color) (shown: bool) (fin: IView option) : IView =
        DockPanel.create [
            DockPanel.height Tokens.Height.row
            DockPanel.children [
                match fin with
                | Some v -> Border.create [ Border.dock Dock.Right; Border.margin (Thickness(Tokens.Space.s2, 0.0, 0.0, 0.0)); Border.verticalAlignment VerticalAlignment.Center; Border.child v ]
                | None -> ()
                StackPanel.create [
                    StackPanel.orientation Orientation.Horizontal
                    StackPanel.spacing Tokens.Space.s2
                    StackPanel.margin (Thickness(indent, 0.0, 0.0, 0.0))
                    StackPanel.verticalAlignment VerticalAlignment.Center
                    StackPanel.children [
                        icon look glyph false look.Dim true
                        TextBlock.create [
                            TextBlock.text name
                            TextBlock.verticalAlignment VerticalAlignment.Center
                            TextBlock.fontWeight (if shown then FontWeight.Medium else FontWeight.Normal)
                            TextBlock.foreground (brush nameColour)
                            TextBlock.textTrimming TextTrimming.CharacterEllipsis
                        ]
                    ]
                ]
            ]
        ]

    let rowButton (shown: bool) (tip: string) (onOpen: unit -> unit) (menu: IView list) (content: IView) : IView =
        Button.create [
            Button.background (if shown then brush look.Roles.AccentSubtle :> IBrush else Brushes.Transparent)
            Button.horizontalAlignment HorizontalAlignment.Stretch
            Button.horizontalContentAlignment HorizontalAlignment.Stretch
            Button.padding (Thickness(Tokens.Space.s2, 0.0))
            Button.minHeight 0.0
            Button.cornerRadius (CornerRadius Tokens.Radius.sm)
            Button.onDoubleTapped ((fun _ -> onOpen ()), SubPatchOptions.Always)
            Button.tip tip
            Button.contextMenu (ContextMenu.create [ ContextMenu.viewItems menu ])
            Button.content content
        ]

    let menuItemOf (header: string) (msg: Msg) : IView =
        MenuItem.create [ MenuItem.header header; MenuItem.onClick ((fun _ -> dispatch msg), SubPatchOptions.Always) ]

    // The project's scripts: the shown one on the accent's tint, open ones in the text colour and
    // the rest muted, a dot at the end of each unsaved; the shown one's tables under it.
    let fileItem (path: string) : IView =
        let shown = model.Path = Some path
        let unsaved = if shown then not (unsaved model).IsEmpty else model.OpenTabs.TryFind path = Some true
        let opened = shown || model.OpenTabs.ContainsKey path

        let dot: IView option =
            if unsaved then
                Some(Border.create [ Border.width Tokens.Space.s2; Border.height Tokens.Space.s2; Border.cornerRadius (CornerRadius Tokens.Space.s1); Border.background (brush look.Text); Border.tip "unsaved" ])
            else
                None

        rowButton
            shown
            path
            (fun () -> dispatch (OpenTab path))
            [ menuItemOf "_Open" (OpenTab path)
              menuItemOf "_Rename…" (RenameFile path)
              menuItemOf "_Duplicate" (DuplicateFile path)
              menuItemOf "Dele_te…" (DeleteFile path)
              Separator.create []
              menuItemOf "Reveal in file _manager" (RevealFile path)
              menuItemOf "_New script" NewScript ]
            (listRow 0.0 fileGlyph (Path.GetFileName path) (if opened then look.Text else look.Dim) shown dot)

    // A table of the shown script's workspace, with its rows and columns: a double-click shows it,
    // and its menu renames, duplicates or removes it.
    let tableItem (tab: int) (t: Tambour.Data.Table) : IView =
        let size =
            TextBlock.create [
                TextBlock.text $"{t.Rows} × {t.Columns.Length}"
                TextBlock.fontSize Tokens.Type.small
                TextBlock.foreground (brush look.Dim)
            ]

        rowButton
            false
            t.Name
            (fun () -> dispatch (SelectTab tab))
            [ menuItemOf "_Show" (SelectTab tab)
              menuItemOf "_Rename…" (RenameTable t.Name)
              menuItemOf "_Duplicate" (DuplicateTable t.Name)
              menuItemOf "Re_move" (RemoveTable t.Name) ]
            (listRow Tokens.Space.s5 tableGlyph t.Name look.Text false (Some size))

    let filesPanel (dir: string) : IView =
        Border.create [
            DockPanel.dock Dock.Left
            Border.width Tokens.Width.files
            Border.background (brush look.Surface)
            Border.borderBrush (brush look.Line)
            Border.borderThickness (Thickness(0.0, 0.0, Tokens.border, 0.0))
            Border.child (
                DockPanel.create [
                    DockPanel.children [
                        Border.create [
                            Border.dock Dock.Top
                            Border.height Tokens.Height.paneHeader
                            Border.borderBrush (brush look.Line)
                            Border.borderThickness (Thickness(0.0, 0.0, 0.0, Tokens.border))
                            Border.padding (Thickness(Tokens.Space.s3, 0.0, Tokens.Space.s2, 0.0))
                            Border.child (
                                DockPanel.create [
                                    DockPanel.children [
                                        Border.create [
                                            Border.dock Dock.Right
                                            Border.verticalAlignment VerticalAlignment.Center
                                            Border.child (paneAction "M8,3.5 V12.5 M3.5,8 H12.5" "New script (Ctrl+N)" false (fun () -> dispatch NewScript))
                                        ]
                                        TextBlock.create [
                                            TextBlock.text (Path.GetFileName dir)
                                            TextBlock.fontSize Tokens.Type.title
                                            TextBlock.fontWeight FontWeight.SemiBold
                                            TextBlock.verticalAlignment VerticalAlignment.Center
                                            TextBlock.foreground (brush look.Text)
                                            TextBlock.textTrimming TextTrimming.CharacterEllipsis
                                            TextBlock.tip dir
                                        ]
                                    ]
                                ]
                            )
                        ]
                        ScrollViewer.create [
                            ScrollViewer.content (
                                StackPanel.create [
                                    StackPanel.margin (Thickness Tokens.Space.s2)
                                    StackPanel.children [
                                        for f in model.Files do
                                            fileItem f

                                            if model.Path = Some f then
                                                for i, t in List.indexed model.Tables do
                                                    tableItem (i + 1) t.Table
                                    ]
                                ]
                            )
                        ]
                    ]
                ]
            )
        ]

    // The figure pane's header: Figure and each table, Add for another table, and the figure's
    // actions and the zoom hint at the right.
    let addTable : IView =
        // The Table menu's commands, with their keys as the user has them.
        let item (header: string) (msg: Msg) : IView =
            MenuItem.create [
                MenuItem.header header
                MenuItem.inputGesture (gesturesOf (commandNamed header) |> List.tryHead |> Option.toObj)
                MenuItem.onClick ((fun _ -> dispatch msg), SubPatchOptions.Always)
            ]

        Button.create [
            Button.background Brushes.Transparent
            Button.height Tokens.Height.controlSm
            Button.minHeight 0.0
            Button.padding (Thickness(Tokens.Space.s1, 0.0))
            Button.verticalAlignment VerticalAlignment.Center
            Button.verticalContentAlignment VerticalAlignment.Center
            Button.tip "Add a table to this workspace"
            Button.contextMenu (ContextMenu.create [ ContextMenu.viewItems [ item "_New table" NewTable; item "_Import table…" ChooseImport; item "_Copy a table from another tab…" ChooseCopyTable ] ])
            Button.onClick (
                (fun e ->
                    match e.Source with
                    | :? Control as c when not (isNull c.ContextMenu) -> c.ContextMenu.Open c
                    | _ -> ()),
                SubPatchOptions.Always
            )
            Button.content (
                StackPanel.create [
                    StackPanel.orientation Orientation.Horizontal
                    StackPanel.spacing Tokens.Space.s1
                    StackPanel.children [
                        icon look "M8,3.5 V12.5 M3.5,8 H12.5" false look.Text true
                        TextBlock.create [ TextBlock.text "Add"; TextBlock.verticalAlignment VerticalAlignment.Center ]
                        icon look "M4.5,6.5 L8,10 L11.5,6.5" false look.Dim true
                    ]
                ]
            )
        ]

    let figureHeader =
        paneHeader
            [ paneTab "Figure" None None (model.Tab = 0) (fun () -> dispatch (SelectTab 0))
              for i, source in List.indexed model.Tables do
                  paneTab source.Table.Name (Some tableGlyph) None (model.Tab = i + 1) (fun () -> dispatch (SelectTab(i + 1))) ]
            [ addTable ]
            [ if model.Tab = 0 then
                  paneAction "M2,4.5 H14 M2,11.5 H14 M5.5,2.75 V6.25 M10.5,9.75 V13.25" "Show or hide the figure's actions (Ctrl+Shift+A)" model.ActionsOpen (fun () -> dispatch ToggleActions)
                  paneAction "M8,1.75 A6.25,6.25 0 1 1 7.99,1.75 Z M8,7.25 V11.25 M8,4.8 V4.81" "Drag to zoom · Shift+drag pans · double-click resets" false ignore
              if model.FullView then
                  paneAction "M6,2.5 V6 H2.5 M10,2.5 V6 H13.5 M13.5,10 H10 V13.5 M2.5,10 H6 V13.5" "Leave the full view (Escape, Ctrl+Shift+F)" true (fun () -> dispatch ToggleFullView)
              else
                  paneAction "M2.5,6 V2.5 H6 M10,2.5 H13.5 V6 M13.5,10 V13.5 H10 M6,13.5 H2.5 V10" "Full view (Ctrl+Shift+F)" false (fun () -> dispatch ToggleFullView) ]

    DockPanel.create [
        DockPanel.background (brush look.Surface)
        DockPanel.children [
            toolbar
            statusBar
            // Always in its place, hidden rather than left out: a child left out moves the ones
            // after it, and the editor would be made anew.
            match model.Project with
            | Some dir -> Border.create [ DockPanel.dock Dock.Left; Border.isVisible (model.FilesOpen && not model.FullView); Border.child (filesPanel dir) ]
            | None -> Border.create [ DockPanel.dock Dock.Left; Border.isVisible false ]
            Grid.create [
                // The full view gives the right side the whole width.
                Grid.columnDefinitions (if model.FullView then "0,0,*" else $"{inv split}*,{Tokens.border},{inv (1.0 - split)}*")
                Grid.children [
                    Grid.create [
                        Grid.column 0
                        Grid.rowDefinitions (if model.BottomOpen then $"*,{Tokens.border},{inv model.Bottom}" else "*,0,0")
                        Grid.children [
                            Editor.create [
                                Grid.row 0
                                Editor.text model.Script
                                Editor.path (Option.toObj model.Path)
                                Editor.language env.Language
                                Editor.goTo model.GoTo
                                Editor.onEdited (Edited >> dispatch)
                                Editor.onProblemsChanged (Problems >> dispatch)
                                Editor.onCaretMoved (Caret >> dispatch)
                            ]
                            Border.create [ Grid.row 1; Border.background (brush look.Line) ]
                            GridSplitter.create [
                                Grid.row 1
                                GridSplitter.isVisible model.BottomOpen
                                GridSplitter.resizeDirection GridResizeDirection.Rows
                                GridSplitter.background Brushes.Transparent
                                GridSplitter.margin (Thickness(0.0, -Tokens.Space.s1))
                                Visual.zIndex 1
                                Primitives.Thumb.onDragCompleted dragged
                            ]
                            DockPanel.create [
                                Grid.row 2
                                DockPanel.isVisible model.BottomOpen
                                DockPanel.children [ bottomHeader; bottomPanel ]
                            ]
                        ]
                    ]
                    // A rule of one pixel, grabbed a little either side of it.
                    Border.create [ Grid.column 1; Border.background (brush look.Line) ]
                    GridSplitter.create [
                        Grid.column 1
                        GridSplitter.resizeDirection GridResizeDirection.Columns
                        GridSplitter.background Brushes.Transparent
                        GridSplitter.margin (Thickness(-Tokens.Space.s1, 0.0))
                        Visual.zIndex 1
                        Primitives.Thumb.onDragCompleted dragged
                    ]
                    DockPanel.create [
                      Grid.column 2
                      DockPanel.children [
                        figureHeader
                        TabControl.create [
                        TabControl.name "sheets"
                        TabControl.classes [ "bare" ]
                        TabControl.padding (Thickness 0.0)
                        TabControl.onSelectedIndexChanged ((fun i -> if i >= 0 && i <> model.Tab then dispatch (SelectTab i)), SubPatchOptions.Always)
                        TabControl.viewItems [
                            TabItem.create [
                                TabItem.header "Figure"
                                TabItem.content (
                                    DockPanel.create [
                                        DockPanel.children [
                                            Border.create [
                                                Border.dock Dock.Top
                                                Border.isVisible model.ActionsOpen
                                                Border.borderBrush (brush look.Line)
                                                Border.borderThickness (Thickness(0.0, 0.0, 0.0, Tokens.border))
                                                Border.child (actionsView model dispatch)
                                            ]
                                            StackPanel.create [
                                                StackPanel.dock Dock.Bottom
                                                StackPanel.children (model.Controls |> Array.mapi (fun i c -> controlView dispatch i c (model.Playing.Contains i)) |> List.ofArray)
                                            ]
                                            // The figure as a card, with the same margin on every side.
                                            Border.create [
                                                Border.margin (Thickness Tokens.Space.s4)
                                                Border.borderBrush (brush look.Line)
                                                Border.borderThickness (Thickness Tokens.border)
                                                Border.cornerRadius (CornerRadius Tokens.Radius.md)
                                                Border.clipToBounds true
                                                Border.child (
                                                    Pane.create [
                                                        Pane.picture model.Picture
                                                        Pane.readout model.Readout
                                                        Pane.marks model.Marks
                                                        Pane.onInput (Input >> dispatch)
                                                        Pane.onPicked (Pick >> dispatch)
                                                    ]
                                                )
                                            ]
                                        ]
                                    ]
                                )
                            ]
                            for source in model.Tables do
                                TabItem.create [ TabItem.header source.Table.Name; TabItem.content (tableView model dispatch source.Table) ]
                        ]
                        // After the items: a table's tab chosen as it is added is there to choose.
                        TabControl.selectedIndex model.Tab
                        ]
                      ]
                    ]
                ]
            ]
        ]
    ]


/// The worker's replies, the keys, the clock of a playing time control, the script's file on
/// disk, and the window's title.
let private subscriptions (env: Env) (host: Control) (titled: string -> unit) (expose: (Msg -> unit) -> unit) (model: Model) : Sub<Msg> =
    let replies (dispatch: Msg -> unit) : IDisposable =
        let cts = new CancellationTokenSource()

        task {
            try
                while true do
                    let! reply = env.Workers.Replies.ReadAsync(cts.Token).AsTask()
                    dispatch (Replied reply)
            with _ ->
                ()
        }
        |> ignore

        { new IDisposable with
            member _.Dispose() = cts.Cancel() }

    // The commands' keys, wherever in the window the focus is, while this workspace is the one
    // shown. A command that cannot be taken now leaves its key to the focused control (Escape to a
    // popup, Ctrl+Z to a text box).
    let keys (dispatch: Msg -> unit) : IDisposable =
        let mutable top: TopLevel = null

        let handler =
            EventHandler<KeyEventArgs>(fun _ e ->
                let inGrid () =
                    match top.FocusManager with
                    | null -> false
                    | f ->
                        match f.GetFocusedElement() with
                        | :? Visual as v -> v :? TableGrid || v.GetVisualAncestors() |> Seq.exists (fun a -> a :? TableGrid)
                        | _ -> false

                if host.IsEffectivelyVisible then
                    match commandFor e.Key e.KeyModifiers with
                    | Some c when c.Enabled model && (not c.InGrid || inGrid ()) ->
                        e.Handled <- true
                        dispatch c.Msg
                    | _ -> ())

        let detach () =
            if not (isNull top) then
                top.RemoveHandler(InputElement.KeyDownEvent, handler)
                top <- null

        let attach () =
            detach ()
            top <- TopLevel.GetTopLevel host
            if not (isNull top) then top.AddHandler(InputElement.KeyDownEvent, handler, RoutingStrategies.Tunnel) |> ignore

        let attached = EventHandler<VisualTreeAttachmentEventArgs>(fun _ _ -> attach ())
        let detached = EventHandler<VisualTreeAttachmentEventArgs>(fun _ _ -> detach ())
        host.AttachedToVisualTree.AddHandler attached
        host.DetachedFromVisualTree.AddHandler detached
        attach ()

        { new IDisposable with
            member _.Dispose() =
                host.AttachedToVisualTree.RemoveHandler attached
                host.DetachedFromVisualTree.RemoveHandler detached
                detach () }

    let clock (dispatch: Msg -> unit) : IDisposable =
        let started = Diagnostics.Stopwatch.StartNew()
        let mutable last = 0.0

        let timer =
            DispatcherTimer.Run(
                (fun () ->
                    let now = started.Elapsed.TotalSeconds
                    dispatch (Tick(now - last))
                    last <- now
                    true),
                TimeSpan.FromSeconds(1.0 / 30.0)
            )

        timer

    /// Someone else's change to the script's file, a moment after it settles.
    let watch (path: string) (dispatch: Msg -> unit) : IDisposable =
        let watcher = new FileSystemWatcher(Path.GetDirectoryName path, Path.GetFileName path)
        watcher.NotifyFilter <- NotifyFilters.LastWrite ||| NotifyFilters.Size ||| NotifyFilters.FileName
        let mutable changes = 0

        let changed _ =
            let change = Interlocked.Increment &changes
            Task.Delay(200).ContinueWith(fun (_: Task) -> if change = changes then dispatch DiskChanged) |> ignore

        watcher.Changed.Add changed
        watcher.Created.Add changed
        watcher.Renamed.Add changed
        watcher.EnableRaisingEvents <- true
        watcher :> IDisposable

    /// Scripts added, removed or renamed in the project directory, a moment after they settle.
    let watchFiles (dir: string) (dispatch: Msg -> unit) : IDisposable =
        let watcher = new FileSystemWatcher(dir, "*.fsx")
        watcher.NotifyFilter <- NotifyFilters.FileName
        let mutable changes = 0

        let changed _ =
            let change = Interlocked.Increment &changes
            Task.Delay(200).ContinueWith(fun (_: Task) -> if change = changes then dispatch FilesChanged) |> ignore

        watcher.Created.Add changed
        watcher.Deleted.Add changed
        watcher.Renamed.Add changed
        watcher.EnableRaisingEvents <- true
        watcher :> IDisposable

    let retitle (text: string) (_: Msg -> unit) : IDisposable =
        titled text
        { new IDisposable with member _.Dispose() = () }

    /// The look the workspace is drawn in, now and whenever its theme changes.
    let look (dispatch: Msg -> unit) : IDisposable =
        let changed = EventHandler(fun _ _ -> dispatch (LookChanged(Palette.forVariant host.ActualThemeVariant)))
        host.ActualThemeVariantChanged.AddHandler changed
        dispatch (LookChanged(Palette.forVariant host.ActualThemeVariant))
        { new IDisposable with member _.Dispose() = host.ActualThemeVariantChanged.RemoveHandler changed }

    let exposed (dispatch: Msg -> unit) : IDisposable =
        expose dispatch
        { new IDisposable with member _.Dispose() = () }

    [ [ "replies" ], replies
      [ "preferences" ], (fun dispatch -> Preferences.Live.Changed.Subscribe(fun _ -> dispatch PreferencesChanged))
      // Made again when what can be taken changes, as the keys read it.
      [ "keys"; commands |> List.map (fun c -> if c.Enabled model then "1" else "0") |> String.concat "" ], keys
      [ "post" ], exposed
      [ "look" ], look
      [ "title"; title model ], retitle (title model)
      match model.Path with
      | Some path when Directory.Exists(Path.GetDirectoryName path) -> [ "watch"; path ], watch path
      | _ -> ()
      match model.Project with
      | Some dir when Directory.Exists dir -> [ "files"; dir ], watchFiles dir
      | _ -> ()
      if not model.Playing.IsEmpty then [ "clock" ], clock ]

/// Asks what to do with what is unsaved, by name: Save, Don't save, or Cancel, which keeps it.
let askToSave (owner: Window) (look: Palette.Look) (names: string list) : Task<Unsaved> =
    let answer = TaskCompletionSource<Unsaved>()

    let listed =
        match names with
        | [ one ] -> one
        | _ -> String.Join(", ", names |> List.take (names.Length - 1)) + " and " + List.last names

    let text =
        TextBlock(
            Text = $"Save {listed}?\n\nWhat is not saved is lost.",
            TextWrapping = TextWrapping.Wrap,
            Foreground = SolidColorBrush look.Text
        )

    let save = Button(Content = "Save", IsDefault = true)
    let discard = Button(Content = "Don't save")
    let keep = Button(Content = "Cancel", IsCancel = true)
    let buttons = StackPanel(Orientation = Orientation.Horizontal, Spacing = Tokens.Space.s2, HorizontalAlignment = HorizontalAlignment.Right)
    buttons.Children.Add discard
    buttons.Children.Add keep
    buttons.Children.Add save
    let panel = StackPanel(Spacing = Tokens.Space.s3, Margin = Thickness Tokens.Space.s4)
    panel.Children.Add text
    panel.Children.Add buttons

    let window =
        Window(Title = "Unsaved changes", Width = 440.0, SizeToContent = SizeToContent.Height, CanResize = false, Content = panel, WindowStartupLocation = WindowStartupLocation.CenterOwner)

    let choose (a: Unsaved) =
        answer.TrySetResult a |> ignore
        window.Close()

    save.Click.Add(fun _ -> choose SaveIt)
    discard.Click.Add(fun _ -> choose Discard)
    keep.Click.Add(fun _ -> choose Keep)
    window.Closed.Add(fun _ -> answer.TrySetResult Keep |> ignore)
    window.ShowDialog owner |> ignore
    answer.Task

/// Asks for a name, the current one given and selected: OK takes it, Cancel or closing does not.
let askForName (owner: Window) (title: string) (current: string) : Task<string option> =
    let answer = TaskCompletionSource<string option>()
    let box = TextBox(Text = current, SelectionStart = 0, SelectionEnd = current.Length)
    let ok = Button(Content = "OK", IsDefault = true)
    let cancel = Button(Content = "Cancel", IsCancel = true)
    let buttons = StackPanel(Orientation = Orientation.Horizontal, Spacing = Tokens.Space.s2, HorizontalAlignment = HorizontalAlignment.Right)
    buttons.Children.Add cancel
    buttons.Children.Add ok
    let panel = StackPanel(Spacing = Tokens.Space.s3, Margin = Thickness Tokens.Space.s4)
    panel.Children.Add box
    panel.Children.Add buttons

    let window =
        Window(Title = title, Width = 380.0, SizeToContent = SizeToContent.Height, CanResize = false, Content = panel, WindowStartupLocation = WindowStartupLocation.CenterOwner)

    ok.Click.Add(fun _ ->
        answer.TrySetResult(Some box.Text) |> ignore
        window.Close())

    cancel.Click.Add(fun _ -> window.Close())
    window.Closed.Add(fun _ -> answer.TrySetResult None |> ignore)
    window.Opened.Add(fun _ -> box.Focus() |> ignore)
    window.ShowDialog owner |> ignore
    answer.Task

/// Asks to delete files for good, naming each: Delete, or Cancel, the default.
let confirmDeleting (owner: Window) (look: Palette.Look) (files: string list) : Task<bool> =
    let answer = TaskCompletionSource<bool>()

    let text =
        TextBlock(
            Text = "Delete these for good? They do not go to the bin.\n\n" + String.Join("\n", files),
            TextWrapping = TextWrapping.Wrap,
            Foreground = SolidColorBrush look.Text
        )

    let delete = Button(Content = "Delete")
    let cancel = Button(Content = "Cancel", IsDefault = true, IsCancel = true)
    let buttons = StackPanel(Orientation = Orientation.Horizontal, Spacing = Tokens.Space.s2, HorizontalAlignment = HorizontalAlignment.Right)
    buttons.Children.Add delete
    buttons.Children.Add cancel
    let panel = StackPanel(Spacing = Tokens.Space.s3, Margin = Thickness Tokens.Space.s4)
    panel.Children.Add text
    panel.Children.Add buttons

    let window =
        Window(Title = "Delete", Width = 480.0, SizeToContent = SizeToContent.Height, CanResize = false, Content = panel, WindowStartupLocation = WindowStartupLocation.CenterOwner)

    delete.Click.Add(fun _ ->
        answer.TrySetResult true |> ignore
        window.Close())

    cancel.Click.Add(fun _ -> window.Close())
    window.Closed.Add(fun _ -> answer.TrySetResult false |> ignore)
    window.Opened.Add(fun _ -> cancel.Focus() |> ignore)
    window.ShowDialog owner |> ignore
    answer.Task

/// Asks a question with two answers: the yes, the default, or the no, as is closing it.
let askToConfirm (owner: Window) (look: Palette.Look) (question: string) (yes: string) (no: string) : Task<bool> =
    let answer = TaskCompletionSource<bool>()
    let text = TextBlock(Text = question, TextWrapping = TextWrapping.Wrap, Foreground = SolidColorBrush look.Text)
    let yesButton = Button(Content = yes, IsDefault = true)
    let noButton = Button(Content = no, IsCancel = true)
    let buttons = StackPanel(Orientation = Orientation.Horizontal, Spacing = Tokens.Space.s2, HorizontalAlignment = HorizontalAlignment.Right)
    buttons.Children.Add noButton
    buttons.Children.Add yesButton
    let panel = StackPanel(Spacing = Tokens.Space.s3, Margin = Thickness Tokens.Space.s4)
    panel.Children.Add text
    panel.Children.Add buttons

    let window =
        Window(Title = "Tambour", Width = 440.0, SizeToContent = SizeToContent.Height, CanResize = false, Content = panel, WindowStartupLocation = WindowStartupLocation.CenterOwner)

    yesButton.Click.Add(fun _ ->
        answer.TrySetResult true |> ignore
        window.Close())

    noButton.Click.Add(fun _ -> window.Close())
    window.Closed.Add(fun _ -> answer.TrySetResult false |> ignore)
    window.ShowDialog owner |> ignore
    answer.Task

/// Asks which table to take, each named with its tab, its rows and columns; none where it was
/// cancelled.
let askForTable (owner: Window) (look: Palette.Look) (choices: (string * Tambour.Data.Table) list) : Task<Tambour.Data.Table option> =
    let answer = TaskCompletionSource<Tambour.Data.Table option>()
    let text = TextBlock(Text = "Copy which table into this workspace?", Foreground = SolidColorBrush look.Text)

    let row (tab: string, t: Tambour.Data.Table) =
        let line = DockPanel()
        let size = TextBlock(Text = $"{tab} · {t.Rows} × {t.Columns.Length}", FontSize = Tokens.Type.small, Foreground = SolidColorBrush look.Dim, VerticalAlignment = VerticalAlignment.Center)
        DockPanel.SetDock(size, Dock.Right)
        line.Children.Add size
        line.Children.Add(TextBlock(Text = t.Name, Foreground = SolidColorBrush look.Text, VerticalAlignment = VerticalAlignment.Center))
        line

    let list = ListBox(ItemsSource = [ for c in choices -> row c ], SelectedIndex = 0, MaxHeight = 320.0)
    let yesButton = Button(Content = "Copy", IsDefault = true)
    let noButton = Button(Content = "Cancel", IsCancel = true)
    let buttons = StackPanel(Orientation = Orientation.Horizontal, Spacing = Tokens.Space.s2, HorizontalAlignment = HorizontalAlignment.Right)
    buttons.Children.Add noButton
    buttons.Children.Add yesButton
    let panel = StackPanel(Spacing = Tokens.Space.s3, Margin = Thickness Tokens.Space.s4)
    panel.Children.Add text
    panel.Children.Add list
    panel.Children.Add buttons

    let window =
        Window(Title = "Tambour", Width = 440.0, SizeToContent = SizeToContent.Height, CanResize = false, Content = panel, WindowStartupLocation = WindowStartupLocation.CenterOwner)

    let take () =
        match List.tryItem list.SelectedIndex choices with
        | Some(_, t) -> answer.TrySetResult(Some t) |> ignore
        | None -> ()

        window.Close()

    yesButton.Click.Add(fun _ -> take ())
    list.DoubleTapped.Add(fun _ -> take ())
    noButton.Click.Add(fun _ -> window.Close())
    window.Closed.Add(fun _ -> answer.TrySetResult None |> ignore)
    window.ShowDialog owner |> ignore
    answer.Task

/// A workspace: everything a window held before there were tabs. Its script or project, figure,
/// tables, history and layout, run by its own program on its own workers, which end when it is
/// disposed. The language service is the window's, shared by its workspaces. It opens `path` once
/// started, if one is given, and `script` otherwise.
type Workspace(script: string, path: string option, language: LanguageService, tabs: Tabs) as this =
    inherit HostControl()

    // The window holding it, which changes when its tab is moved to another.
    let mutable owner = tabs

    let forwarded =
        { New = fun () -> owner.New()
          Open = fun p -> owner.Open p
          Close = fun () -> owner.Close()
          Cycle = fun step -> owner.Cycle step
          Opened = fun p -> owner.Opened p
          ForgetRecent = fun missingOnly -> owner.ForgetRecent missingOnly
          SaveAll = fun () -> owner.SaveAll()
          CloseProject = fun dir -> owner.CloseProject dir
          AskName = fun title current -> owner.AskName title current
          ConfirmDelete = fun files -> owner.ConfirmDelete files
          Confirm = fun question yes no -> owner.Confirm question yes no
          Renamed = fun before after -> owner.Renamed before after
          Deleted = fun files -> owner.Deleted files
          Detach = fun () -> owner.Detach()
          Preferences = fun () -> owner.Preferences()
          OtherTables = fun () -> owner.OtherTables()
          PickTable = fun choices -> owner.PickTable choices }

    let mutable dispatch: Msg -> unit = ignore
    let mutable title = "Tambour"
    let titleChanged = Event<string>()
    let updated = Event<Msg>()
    let mutable latest: Model = Unchecked.defaultof<_>

    // Started when first asked; given the workspace's tables each time they start.
    let workers =
        new LazyWorkers(
            Command.ofThisApp (),
            { References = [||]; Prelude = prelude; Skia = { Dpi = float Marey.Space.screenDpi; FontFiles = [||] } },
            fun () -> [ for s in latest.Tables -> Tables.request s.Table ]
        )

    let env =
        { Workers = workers
          Language = language
          Host = this
          Tabs = forwarded }

    // A tab in the background, its workers idle past the limit, gives them up and keeps its figure.
    // Looked at from a timer of the thread pool, on the UI thread, where the workspace's state is.
    let ui = Dispatcher.UIThread

    let idle =
        new Timer(
            TimerCallback(fun _ ->
                ui.Post(fun () ->
                    if workers.Live && not this.IsEffectivelyVisible && not latest.Running && workers.Idle > Workspace.IdleLimit then
                        workers.Drop())),
            null,
            (Workspace.IdleCheck: TimeSpan),
            (Workspace.IdleCheck: TimeSpan)
        )

    let titled (text: string) =
        if text <> title then
            title <- text
            titleChanged.Trigger text

    do
        let init () =
            let model, cmd = init script ()
            latest <- model
            model, (match path with Some p -> Cmd.batch [ cmd; Cmd.ofMsg (OpenPath p) ] | None -> cmd)

        let step (msg: Msg) (model: Model) =
            let next, cmd = update env msg model
            latest <- next
            updated.Trigger msg
            next, cmd

        Program.mkProgram init step (view env)
        |> Program.withHost this
        |> Program.withSubscription (subscriptions env this titled (fun d -> dispatch <- d))
        |> Program.withTermination (function Ended -> true | _ -> false) ignore
        |> Program.runWithAvaloniaSyncDispatch ()

    /// Sends the workspace's loop a message, as its own controls do.
    member _.Post(msg: Msg) = dispatch msg

    /// The window's tabs it asks of, set when it moves to another window.
    member _.Owner
        with set (value: Tabs) = owner <- value

    /// How long a tab in the background keeps idle workers, and how often that is looked at.
    static member val IdleLimit = TimeSpan.FromMinutes 10.0 with get, set
    static member val IdleCheck = TimeSpan.FromSeconds 30.0 with get, set

    /// The workers' start, for whoever waits on it: nothing to wait for before they are asked.
    member _.Started = workers.Started

    /// Whether the workspace has workers now.
    member _.HasWorkers = workers.Live

    /// The script's name, marked while anything is unsaved, as the window's title shows it.
    member _.Title = title

    member _.TitleChanged = titleChanged.Publish

    /// Each message the workspace has taken, once its model is updated.
    member _.Updated = updated.Publish

    /// The script's name.
    member _.ScriptName = scriptName latest

    /// What its tab shows: the bundle's name where it is one, the script's otherwise.
    member _.TabName = workspaceName latest

    /// Its tables, as they are now.
    member _.Tables = [ for s in latest.Tables -> s.Table ]

    /// The text settings its figures are exported with: its project's, or the app's.
    member _.Skia = skiaOf latest

    /// What is unsaved in it, by name; empty when everything is on disk.
    member _.Unsaved = unsaved latest

    /// Where its script is, and its project directory, if it has them.
    member _.Path = latest.Path
    member _.Project = latest.Project

    /// The `.tambour` it was opened from, if it was.
    member _.Bundle = latest.Bundle

    /// Where it is kept, as the recent list and the next start name it: its bundle, or its script.
    member _.Location = latest.Bundle |> Option.orElse latest.Path

    /// Whether nothing has been done in it: no file, nothing typed, nothing in its history. Opening
    /// a file into such a tab takes it rather than leaving it empty beside a new one.
    member _.Untouched =
        latest.Path.IsNone && (unsaved latest).IsEmpty && latest.History.Done.IsEmpty && latest.Tables.IsEmpty

    /// Saves it: true once everything is on disk, false where the save failed or its file was not
    /// chosen.
    member _.SaveAsync() : Task<bool> =
        let result = TaskCompletionSource<bool>()
        let mutable watching: IDisposable = null

        watching <-
            updated.Publish.Subscribe(fun msg ->
                let finish (ok: bool) =
                    result.TrySetResult ok |> ignore
                    if not (isNull watching) then watching.Dispose()

                if (unsaved latest).IsEmpty then finish true
                else
                    match msg with
                    | Save when latest.Path.IsSome -> finish false
                    | Dismissed
                    | Failed _ -> finish false
                    | _ -> ())

        dispatch Save
        result.Task

    interface IDisposable with
        member _.Dispose() =
            dispatch Ended
            idle.Dispose()
            (workers :> IDisposable).Dispose()
            // Written back if its settings changed since, and its cache removed.
            latest.Bundle |> Option.iter (fun file -> try Bundle.close file with _ -> ())

/// The windows a tab has been moved between, from one first window: the language service they
/// share, ended once the last of them closes, and the tabs they keep for the next start.
type WindowGroup() =
    let language = new LanguageService(Command.ofThisApp (), { References = [||]; Prelude = prelude })

    member _.Language = language
    member val Windows = ResizeArray<Window>()

    /// The tabs' scripts and the one shown, as last kept in the user's settings.
    member val Kept: string list * int = ([], 0) with get, set

    member this.Leave(window: Window) =
        this.Windows.Remove window |> ignore
        if this.Windows.Count = 0 then (language :> IDisposable).Dispose()

/// The main window: a tab per workspace, and the language service they share, which end when it
/// closes. Closing a tab or the window asks first about what is unsaved. It opens a tab for each
/// script or path given, and shows the start view while it has none. A tab dragged out of it, or
/// moved from its menu, goes to a window of its own in the same group, or into another of the
/// group's windows; a window made so closes with its last tab.
type MainWindow private (opening: (string * string option) list, group: WindowGroup, moved: bool) as this =
    inherit Window()

    let language = group.Language
    let workspaces = ResizeArray<Workspace>()
    let mutable active = -1
    let strip = StackPanel(Orientation = Orientation.Horizontal, Margin = Thickness(Tokens.Space.s2, 0.0))
    let deck = Avalonia.Controls.Grid()
    /// The start view, in the deck while no tab is open, and what it has to tell.
    let start = Border(IsVisible = false)
    let mutable startNote = ""
    let startView: (unit -> Control) ref = ref (fun () -> null)
    /// Each tab's subscriptions to its workspace, ended when the tab moves to another window.
    let links = Collections.Generic.Dictionary<Workspace, IDisposable list>()
    let mutable labels: string list = []
    /// Set once the window's unsaved work has been asked about, so that closing goes ahead.
    let mutable closing = false

    /// The scripts and projects opened, most recent first: kept in the user's settings, or for this
    /// run only where there are none (a test's).
    let mutable recent: string list =
        match App.SettingsDirectory with
        | null -> []
        | dir -> (Preferences.read dir).Recent

    let setRecent (next: string list) =
        recent <- next

        match App.SettingsDirectory with
        | null -> ()
        | dir ->
            try
                Preferences.write dir { Preferences.read dir with Recent = next }
            with _ ->
                ()

        for w in workspaces do
            w.Post(RecentChanged next)

        if start.IsVisible then start.Child <- startView.Value ()

    let mutable ask: string list -> Task<Unsaved> = fun _ -> Task.FromResult Keep
    let mutable askName: string -> string -> Task<string option> = fun _ _ -> Task.FromResult None
    let mutable confirmDelete: string list -> Task<bool> = fun _ -> Task.FromResult false
    let mutable confirm: string -> string -> string -> Task<bool> = fun _ _ _ -> Task.FromResult false
    let mutable pickTable: (string * Tambour.Data.Table) list -> Task<Tambour.Data.Table option> = fun _ -> Task.FromResult None
    /// The open scripts as last told to the tabs.
    let mutable told: Map<string, bool> = Map.empty

    let look () = Palette.forVariant this.ActualThemeVariant

    let labelOf (w: Workspace) = w.TabName + (if w.Unsaved.IsEmpty then "" else " •")

    let rec refresh (force: bool) =
        let now = [ for w in workspaces -> labelOf w ] @ [ string active ]

        // The tabs kept for the next start: those with a file in each of the group's windows, and
        // the one shown here.
        let tabs = [ for win in group.Windows do yield! (win :?> MainWindow).Paths ]

        let shown =
            match (if active >= 0 then workspaces.[active].Location else None) with
            | Some p -> List.findIndex ((=) p) tabs
            | None -> 0

        if (tabs, shown) <> group.Kept then
            group.Kept <- (tabs, shown)

            match App.SettingsDirectory with
            | null -> ()
            | dir ->
                try
                    Preferences.write dir { Preferences.read dir with Tabs = tabs; Shown = shown }
                with _ ->
                    ()

        let opened = Map.ofList [ for w in workspaces do match w.Path with Some p -> p, not w.Unsaved.IsEmpty | None -> () ]

        if opened <> told then
            told <- opened

            for w in workspaces do
                w.Post(TabsChanged opened)

        if force || now <> labels then
            labels <- now
            let look = look ()
            strip.Children.Clear()

            // A drawn mark: stroked, at the icon size, in a colour.
            let mark (data: string) (colour: Color) =
                Shapes.Path(
                    Data = Geometry.Parse data,
                    Width = Tokens.Height.icon,
                    Height = Tokens.Height.icon,
                    Stroke = SolidColorBrush colour,
                    StrokeThickness = 1.5,
                    StrokeLineCap = PenLineCap.Round
                )

            let quiet (content: Control) (tip: string) =
                let b =
                    Button(
                        Content = content,
                        Width = Tokens.Height.controlSm,
                        Height = Tokens.Height.controlSm,
                        MinHeight = 0.0,
                        Padding = Thickness 0.0,
                        HorizontalContentAlignment = HorizontalAlignment.Center,
                        VerticalContentAlignment = VerticalAlignment.Center,
                        VerticalAlignment = VerticalAlignment.Center,
                        Background = Brushes.Transparent
                    )

                ToolTip.SetTip(b, tip)
                b

            let tabHeight = Tokens.Height.tabs - Tokens.Space.s1

            workspaces
            |> Seq.iteri (fun i w ->
                let shown = i = active
                let name = TextBlock(Text = w.ScriptName, VerticalAlignment = VerticalAlignment.Center, Foreground = SolidColorBrush(if shown then look.Text else look.Dim))
                let close = quiet (mark "M4.5,4.5 L11.5,11.5 M11.5,4.5 L4.5,11.5" look.Dim) "Close tab (Ctrl+W)"
                close.Click.Add(fun _ -> closeTab w |> ignore)
                let row = StackPanel(Orientation = Orientation.Horizontal, Spacing = Tokens.Space.s2)
                row.Children.Add name

                if not w.Unsaved.IsEmpty then
                    let dot = Shapes.Ellipse(Width = Tokens.Space.s2, Height = Tokens.Space.s2, Fill = SolidColorBrush(if shown then look.Text else look.Dim), VerticalAlignment = VerticalAlignment.Center)
                    ToolTip.SetTip(dot, "unsaved")
                    row.Children.Add dot

                row.Children.Add close

                // The shown tab on the bar's colour, open to it below: one piece with the bar.
                let tab =
                    Button(
                        Content = row,
                        Height = (if shown then tabHeight + Tokens.border else tabHeight),
                        MinHeight = 0.0,
                        VerticalAlignment = VerticalAlignment.Bottom,
                        VerticalContentAlignment = VerticalAlignment.Center,
                        Padding = Thickness(Tokens.Space.s3, 0.0, Tokens.Space.s1, 0.0),
                        CornerRadius = CornerRadius(Tokens.Radius.md, Tokens.Radius.md, 0.0, 0.0),
                        Background = (if shown then SolidColorBrush look.Surface :> IBrush else Brushes.Transparent),
                        BorderBrush = (if shown then SolidColorBrush look.Line :> IBrush else Brushes.Transparent),
                        BorderThickness = Thickness(Tokens.border, Tokens.border, Tokens.border, 0.0)
                    )

                ToolTip.SetTip(tab, (match w.Location with Some p -> p | None -> "not saved yet"))
                tab.Click.Add(fun _ -> show i)

                // Dragged and let go outside the window: into the group's window there, or a new one.
                let pressed = ref false

                tab.AddHandler(
                    InputElement.PointerPressedEvent,
                    EventHandler<PointerPressedEventArgs>(fun _ e -> pressed.Value <- e.GetCurrentPoint(tab).Properties.IsLeftButtonPressed),
                    RoutingStrategies.Tunnel,
                    true
                )
                |> ignore

                tab.AddHandler(
                    InputElement.PointerReleasedEvent,
                    EventHandler<PointerReleasedEventArgs>(fun _ e ->
                        if pressed.Value then
                            pressed.Value <- false
                            let at = e.GetPosition this

                            if not (Rect(this.ClientSize).Contains at) then
                                dropped w (this.PointToScreen at)),
                    RoutingStrategies.Tunnel,
                    true
                )
                |> ignore

                let menu = ContextMenu()

                tab.ContextRequested.Add(fun _ ->
                    let item (header: string) (enabled: bool) (act: unit -> unit) =
                        let m = MenuItem(Header = header, IsEnabled = enabled)
                        m.Click.Add(fun _ -> act ())
                        m

                    menu.Items.Clear()
                    menu.Items.Add(item "Move to a _new window" (workspaces.Count > 1) (fun () -> detach w None |> ignore)) |> ignore

                    for other in group.Windows do
                        if not (obj.ReferenceEquals(other, this)) then
                            menu.Items.Add(item $"Move to the window of {other.Title}" true (fun () -> give w (other :?> MainWindow))) |> ignore

                    menu.Items.Add(Separator()) |> ignore
                    menu.Items.Add(item "_Close tab" true (fun () -> closeTab w |> ignore)) |> ignore)

                tab.ContextMenu <- menu
                strip.Children.Add tab)

            let plus = quiet (mark "M8,3.5 V12.5 M3.5,8 H12.5" look.Dim) "New script (Ctrl+N)"
            plus.Margin <- Thickness(Tokens.Space.s1, 0.0, 0.0, Tokens.Space.s1)
            plus.VerticalAlignment <- VerticalAlignment.Bottom
            plus.Click.Add(fun _ -> add "" None)
            strip.Children.Add plus

    and show (i: int) =
        if i >= 0 && i < workspaces.Count then
            start.IsVisible <- false
            active <- i
            workspaces |> Seq.iteri (fun j w -> w.IsVisible <- (j = i))
            this.Title <- workspaces.[i].Title
            refresh false

    /// The start view, in place of a tab while there is none.
    and showStart () =
        active <- -1
        this.Title <- "Tambour"
        start.Child <- startView.Value ()
        start.IsVisible <- true
        refresh true

    /// What the start view's menus and keys do, there being no tab to do it.
    and startCommand (msg: Msg) =
        match msg with
        | NewScript -> add "" None
        | ChooseOpen folder -> chooseOpen folder |> ignore
        | OpenPreferences -> openPreferences ()
        | Quit -> this.Close()
        | About -> (aboutWindow (look ())).ShowDialog this |> ignore
        | _ -> ()

    /// Asks for a script or a folder, and opens it in a tab.
    and chooseOpen (folder: bool) =
        task {
            match! pickOpen this folder with
            | Some p -> openPath p
            | None -> ()
        }
        |> ignore

    /// The tabs a workspace in this window asks of: this window's.
    and tabsFor (self: unit -> Workspace) : Tabs =
        { New = fun () -> add "" None
          Open = openPath
          Close = fun () -> closeTab (self ()) |> ignore
          Cycle = fun step -> if workspaces.Count > 0 then show ((active + step + workspaces.Count) % workspaces.Count)
          Opened = fun path -> setRecent (Preferences.opened Preferences.Live.Current.RecentKeep path recent)
          ForgetRecent =
            fun missingOnly ->
                setRecent (if missingOnly then recent |> List.filter (fun p -> File.Exists p || Directory.Exists p) else [])
          SaveAll = fun () -> saveAll () |> ignore
          CloseProject = fun dir -> closeProject dir |> ignore
          AskName = fun title current -> askName title current
          ConfirmDelete = fun files -> confirmDelete files
          Confirm = fun question yes no -> confirm question yes no
          // A script moved or deleted is followed in every window of the group.
          Renamed =
            fun before after ->
                for win in group.Windows do
                    for (w: Workspace) in (win :?> MainWindow).Workspaces do
                        if w.Path = Some before then w.Post(Renamed after)
          Deleted =
            fun files ->
                for win in List.ofSeq group.Windows do
                    for (w: Workspace) in (win :?> MainWindow).Workspaces do
                        match w.Path with
                        | Some p when List.contains p files -> (win :?> MainWindow).Forget w
                        | _ -> ()
          Detach = fun () -> if workspaces.Count > 1 then detach (self ()) None |> ignore
          Preferences = fun () -> openPreferences ()
          OtherTables =
            fun () ->
                [ for win in List.ofSeq group.Windows do
                      for (w: Workspace) in (win :?> MainWindow).Workspaces do
                          if not (obj.ReferenceEquals(w, self ())) then
                              for t in w.Tables -> w.ScriptName, t ]
          PickTable = fun choices -> pickTable choices }

    /// The Preferences dialog, over this window, with the commands' shortcuts to show and change.
    and openPreferences () =
        let shortcuts: PreferencesView.Shortcut list =
            [ for c in commands ->
                  { Menu = c.Menu
                    Header = c.Header
                    Keys = Option.toList c.Gesture @ c.Also |> List.map string
                    InGrid = c.InGrid } ]

        // The Project page edits the shown tab's project, where it has one.
        let project: PreferencesView.ProjectPage option =
            if active < 0 then None
            else
                let w = workspaces.[active]
                w.Project |> Option.map (fun dir -> { Name = Path.GetFileName dir; Skia = w.Skia; Set = fun s -> w.Post(SetSkia s) })

        let dialog = PreferencesView.PreferencesWindow(shortcuts, openPath, project)
        dialog.ShowDialog this |> ignore

    and add (script: string) (path: string option) =
        let self = ref Unchecked.defaultof<Workspace>
        let w = new Workspace(script, path, language, tabsFor (fun () -> self.Value))
        self.Value <- w
        take w

    /// A workspace into a tab of this window, shown.
    and take (w: Workspace) =
        w.Owner <- tabsFor (fun () -> w)

        links.[w] <-
            [ w.TitleChanged.Subscribe(fun t -> if workspaces.IndexOf w = active then this.Title <- t)
              w.Updated.Subscribe(fun _ -> refresh false) ]

        workspaces.Add w
        deck.Children.Add w
        w.Post(RecentChanged recent)
        show (workspaces.Count - 1)

    /// A workspace's tab out of this window, the workspace kept: for another window to take, or
    /// ended by whoever took it out.
    and release (w: Workspace) =
        if workspaces.Contains w then
            // Unlinked first: taken out of the tree, its look changes, and its update would reach
            // this window mid-way.
            for link in links.[w] do
                link.Dispose()

            links.Remove w |> ignore
            let i = workspaces.IndexOf w
            workspaces.RemoveAt i
            deck.Children.Remove w |> ignore

            if workspaces.Count = 0 then emptied ()
            else show (min i (workspaces.Count - 1))

            refresh true

    /// No tab left: the start view, or for a window a tab was moved into, closing.
    and emptied () =
        if moved then
            active <- -1
            Dispatcher.UIThread.Post(fun () -> this.Close())
        else
            showStart ()

    /// A tab moved into another of the group's windows.
    and give (w: Workspace) (target: MainWindow) =
        if workspaces.Contains w && not (obj.ReferenceEquals(target, this)) then
            release w
            // This window's layout done first, which still has the tab's controls queued and
            // refuses them once they are in another window's tree.
            this.UpdateLayout()
            target.Take w
            target.Activate()

    /// A tab moved into a new window of the group, placed where it was let go if that is known.
    and detach (w: Workspace) (at: PixelPoint option) : MainWindow =
        let window = new MainWindow([], group, true)
        window.Width <- this.Width
        window.Height <- this.Height

        match at with
        | Some p -> window.Position <- PixelPoint(p.X - 60, p.Y - 15)
        | None -> window.Position <- PixelPoint(this.Position.X + 40, this.Position.Y + 40)

        window.Show()
        give w window
        window

    /// A tab let go at a place on the screen: into the group's window there, or a new one.
    and dropped (w: Workspace) (screen: PixelPoint) =
        let over =
            group.Windows
            |> Seq.cast<MainWindow>
            |> Seq.tryFind (fun o -> not (obj.ReferenceEquals(o, this)) && o.Covers screen)

        match over with
        | Some target -> give w target
        | None when workspaces.Count > 1 -> detach w (Some screen) |> ignore
        | None -> ()

    and openPath (path: string) =
        let full = Path.GetFullPath path
        let showing (w: Workspace) = w.Path = Some full || w.Project = Some full || w.Bundle = Some full

        match workspaces |> Seq.tryFindIndex showing with
        | Some i ->
            setRecent (Preferences.opened Preferences.Live.Current.RecentKeep full recent)
            show i
        | None when active >= 0 && workspaces.[active].Untouched -> workspaces.[active].Post(OpenPath full)
        | None -> add "" (Some full)

    /// Asks about a workspace's unsaved work: true once it may be dropped (saved, or not wanted).
    and settle (w: Workspace) : Task<bool> =
        task {
            match w.Unsaved with
            | [] -> return true
            | names ->
                show (workspaces.IndexOf w)

                match! ask names with
                | SaveIt -> return! w.SaveAsync()
                | Discard -> return true
                | Keep -> return false
        }

    /// Saves each tab with unsaved work in turn, showing it while its file is asked for.
    and saveAll () : Task =
        task {
            for w in List.ofSeq workspaces do
                if not w.Unsaved.IsEmpty then
                    show (workspaces.IndexOf w)
                    let! _ = w.SaveAsync()
                    ()
        }

    /// Closes the tabs of a project directory, asking about each one's unsaved work.
    and closeProject (dir: string) : Task =
        task {
            for w in List.ofSeq workspaces do
                if w.Project = Some dir then
                    do! closeTab w
        }

    /// Closes a tab without asking: what it showed is gone.
    and forget (w: Workspace) =
        if workspaces.Contains w then
            release w
            (w :> IDisposable).Dispose()

    and closeTab (w: Workspace) : Task =
        task {
            let! settled = settle w

            if settled && workspaces.Contains w then
                release w
                (w :> IDisposable).Dispose()
        }

    do
        this.Title <- "Tambour"
        // A button that cannot be taken now shows dimmed, on no box of its own.
        this.Resources.["ButtonBackgroundDisabled"] <- Brushes.Transparent
        this.Resources.["ButtonBorderBrushDisabled"] <- Brushes.Transparent
        this.Width <- 1280.0
        this.Height <- 800.0

        // The window where it was last closed, as large, maximized if it was; not a window a tab
        // was moved into. A place on no screen now (one unplugged since) is left to the system.
        if not moved && group.Windows.Count = 0 then
            match (preferences ()).Window with
            | Some p ->
                this.Width <- p.Width
                this.Height <- p.Height
                this.Position <- PixelPoint(p.X, p.Y)
                this.WindowStartupLocation <- WindowStartupLocation.Manual

                this.Opened.Add(fun _ ->
                    let onScreen = this.Screens.All |> Seq.exists (fun s -> s.Bounds.Contains(PixelPoint(p.X + 16, p.Y + 16)))
                    if not onScreen && this.Screens.All.Count > 0 then this.WindowStartupLocation <- WindowStartupLocation.CenterScreen

                    // No larger than the screen it is on, nor past its edges: a screen's scaling
                    // may have grown since, which makes the same size larger.
                    match this.Screens.ScreenFromWindow this with
                    | null -> ()
                    | screen ->
                        let area = screen.WorkingArea
                        let scale = screen.Scaling
                        let width = min this.Width (float area.Width / scale)
                        let height = min this.Height (float area.Height / scale)

                        if width < this.Width || height < this.Height then
                            this.Width <- width
                            this.Height <- height

                        let x = Math.Clamp(this.Position.X, area.X, max area.X (area.Right - int (width * scale)))
                        let y = Math.Clamp(this.Position.Y, area.Y, max area.Y (area.Bottom - int (height * scale)))
                        if PixelPoint(x, y) <> this.Position then this.Position <- PixelPoint(x, y)

                    if p.Maximized then this.WindowState <- WindowState.Maximized)
            | None -> ()

            // The size and place it had last while neither maximized nor minimized, kept on close.
            let mutable normal = (this.Position, this.Width, this.Height)

            let track () =
                if this.WindowState = WindowState.Normal then normal <- (this.Position, this.Width, this.Height)

            this.PositionChanged.Add(fun _ -> track ())
            this.PropertyChanged.Add(fun e -> if e.Property = Window.ClientSizeProperty then track ())
            // Taken as closing begins: once closed, the window no longer knows where it was.
            this.Closing.Add(fun _ -> track ())

            this.Closed.Add(fun _ ->
                match App.SettingsDirectory with
                | null -> ()
                | dir ->
                    let position, width, height = normal

                    try
                        Preferences.write
                            dir
                            { Preferences.read dir with
                                Window =
                                    Some
                                        { X = position.X
                                          Y = position.Y
                                          Width = width
                                          Height = height
                                          Maximized = (this.WindowState = WindowState.Maximized) } }
                    with _ ->
                        ())
        ask <-
            fun names ->
                match App.AnswerUnsaved with
                | Some answer -> Task.FromResult answer
                | None -> askToSave this (look ()) names

        // A test run answers no dialog it did not mean to: no name, no delete.
        askName <-
            fun title current ->
                if App.AnswerUnsaved.IsSome then Task.FromResult None else askForName this title current

        confirmDelete <- fun files -> if App.AnswerUnsaved.IsSome then Task.FromResult false else confirmDeleting this (look ()) files

        confirm <-
            fun question yes no ->
                if App.AnswerUnsaved.IsSome then Task.FromResult false else askToConfirm this (look ()) question yes no

        pickTable <- fun choices -> if App.AnswerUnsaved.IsSome then Task.FromResult None else askForTable this (look ()) choices
        // The strip, with its rule along the bottom drawn under the tabs, so that the shown one
        // covers it.
        let rule = Border(Height = Tokens.border, VerticalAlignment = VerticalAlignment.Bottom)
        let tabsRow = Avalonia.Controls.Grid(Height = Tokens.Height.tabs)
        tabsRow.Children.Add rule
        tabsRow.Children.Add(ScrollViewer(Content = strip, HorizontalScrollBarVisibility = Primitives.ScrollBarVisibility.Auto, VerticalScrollBarVisibility = Primitives.ScrollBarVisibility.Disabled))

        // The strip's colours follow the theme.
        let paintStrip () =
            let look = look ()
            tabsRow.Background <- SolidColorBrush look.Raised
            rule.Background <- SolidColorBrush look.Line

        paintStrip ()
        this.ActualThemeVariantChanged.Add(fun _ -> paintStrip ())
        let layout = DockPanel()
        DockPanel.SetDock(tabsRow, Dock.Top)
        layout.Children.Add tabsRow
        layout.Children.Add deck
        deck.Children.Add start
        this.Content <- layout
        group.Windows.Add this

        // New script, Open, Open folder, the example, and the recent list, what is gone dimmed.
        startView.Value <-
            fun () ->
                let look = look ()
                let brush (c: Color) = SolidColorBrush c

                let glyph (data: string) (colour: Color) =
                    Shapes.Path(
                        Data = Geometry.Parse data,
                        Width = Tokens.Height.icon,
                        Height = Tokens.Height.icon,
                        Stroke = brush colour,
                        StrokeThickness = 1.25,
                        StrokeJoin = PenLineJoin.Round,
                        StrokeLineCap = PenLineCap.Round,
                        VerticalAlignment = VerticalAlignment.Center
                    )

                let file = "M4,1.75 H9 L12,4.75 V14.25 H4 Z M9,1.75 V4.75 H12"
                let bundle = "M4,1.75 H9 L12,4.75 V14.25 H4 Z M9,1.75 V4.75 H12 M6,12 L7.5,9.5 L9,10.5 L10.5,8"
                let folder = "M1.75,4.25 A1,1 0 0 1 2.75,3.25 H5.75 L7.25,4.75 H13.25 A1,1 0 0 1 14.25,5.75 V12.25 A1,1 0 0 1 13.25,13.25 H2.75 A1,1 0 0 1 1.75,12.25 Z"

                /// A row: an icon, a name, and at its end a shortcut or where the file is.
                let entry (icon: string) (text: string) (fin: string) (there: bool) (act: unit -> unit) =
                    let row = DockPanel(Height = Tokens.Height.row)
                    let tail = TextBlock(Text = fin, FontSize = Tokens.Type.small, Foreground = brush (if there then look.Dim else look.Roles.Faint), TextTrimming = TextTrimming.CharacterEllipsis, VerticalAlignment = VerticalAlignment.Center, Margin = Thickness(Tokens.Space.s4, 0.0, 0.0, 0.0))
                    DockPanel.SetDock(tail, Dock.Right)
                    row.Children.Add tail
                    let name = StackPanel(Orientation = Orientation.Horizontal, Spacing = Tokens.Space.s2)
                    name.Children.Add(glyph icon (if there then look.Dim else look.Roles.Faint))
                    name.Children.Add(TextBlock(Text = text, Foreground = brush (if there then look.Text else look.Roles.Faint), TextTrimming = TextTrimming.CharacterEllipsis, VerticalAlignment = VerticalAlignment.Center))
                    row.Children.Add name

                    let b =
                        Button(
                            Content = row,
                            Background = Brushes.Transparent,
                            HorizontalAlignment = HorizontalAlignment.Stretch,
                            HorizontalContentAlignment = HorizontalAlignment.Stretch,
                            Padding = Thickness(Tokens.Space.s2, 0.0),
                            MinHeight = 0.0
                        )

                    b.Click.Add(fun _ -> act ())
                    b

                let section (title: string) =
                    let column = StackPanel()
                    column.Children.Add(TextBlock(Text = title, FontSize = Tokens.Type.small, FontWeight = FontWeight.SemiBold, Foreground = brush look.Dim, Height = Tokens.Height.row, Padding = Thickness(Tokens.Space.s2, Tokens.Space.s1 + Tokens.border, 0.0, 0.0)))
                    column

                let quiet (text: string) (act: unit -> unit) =
                    let b = Button(Content = text, Background = Brushes.Transparent)
                    b.Click.Add(fun _ -> act ())
                    b

                let panel = StackPanel(Spacing = Tokens.Space.s6, Width = 800.0, Margin = Thickness(Tokens.Space.s4, Tokens.Space.s7, Tokens.Space.s4, Tokens.Space.s5), HorizontalAlignment = HorizontalAlignment.Center)
                let title = StackPanel(Orientation = Orientation.Horizontal, Spacing = Tokens.Space.s3)

                // The app's icon, as tall as the name's line.
                try
                    use stream = Avalonia.Platform.AssetLoader.Open(Uri "avares://tambour/Assets/tambour.png")
                    let icon = Image(Name = "app icon", Source = new Avalonia.Media.Imaging.Bitmap(stream), Width = Tokens.Type.displayLine, Height = Tokens.Type.displayLine, VerticalAlignment = VerticalAlignment.Bottom)
                    // Drawn a tenth of its size: sampled smoothly, not every so many pixels.
                    RenderOptions.SetBitmapInterpolationMode(icon, Avalonia.Media.Imaging.BitmapInterpolationMode.HighQuality)
                    title.Children.Add icon
                with _ ->
                    ()

                title.Children.Add(TextBlock(Text = "Tambour", FontSize = Tokens.Type.display, FontWeight = FontWeight.SemiBold, Foreground = brush look.Text, VerticalAlignment = VerticalAlignment.Bottom))
                title.Children.Add(TextBlock(Text = "Measured data, figures and F# scripting", Foreground = brush look.Dim, VerticalAlignment = VerticalAlignment.Bottom, Margin = Thickness(0.0, 0.0, 0.0, Tokens.Space.s1)))
                panel.Children.Add title

                if startNote <> "" then
                    panel.Children.Add(TextBlock(Text = startNote, Foreground = brush look.Warning, TextWrapping = TextWrapping.Wrap))

                let columns = Avalonia.Controls.Grid(ColumnDefinitions = ColumnDefinitions $"280,{Tokens.Space.s6},*")
                let start = section "Start"
                start.Children.Add(entry "M4,1.75 H9 L12,4.75 V14.25 H4 Z M9,1.75 V4.75 H12 M8,7.5 V11.5 M6,9.5 H10" "New script" "Ctrl+N" true (fun () -> add "" None))
                start.Children.Add(entry file "Open…" "Ctrl+O" true (fun () -> chooseOpen false))
                start.Children.Add(entry folder "Open folder…" "Ctrl+Shift+O" true (fun () -> chooseOpen true))
                start.Children.Add(entry "M2.5,2.5 V13.5 H13.5 M5,10 L8,6.5 L10,8.5 L13.5,4.5" "The example figure" "" true (fun () -> add example None))
                columns.Children.Add start

                if not recent.IsEmpty then
                    let recentColumn = section "Recent"
                    Avalonia.Controls.Grid.SetColumn(recentColumn, 2)

                    for p in recent do
                        let there = File.Exists p || Directory.Exists p
                        let home = Environment.GetFolderPath Environment.SpecialFolder.UserProfile
                        let where = Path.GetDirectoryName p |> fun d -> if not (String.IsNullOrEmpty home) && d.StartsWith home then "~" + d.Substring home.Length else d
                        let icon = if Directory.Exists p then folder elif Bundle.isBundle p then bundle else file
                        let e = entry icon (Path.GetFileName p) (if there then where else $"missing · {where}") there (fun () -> if there then openPath p)

                        // A bundle's tip shows the figure it was saved with, read from the file.
                        match (if there && Bundle.isBundle p then Bundle.thumbnail p else None) with
                        | Some png ->
                            let tip = StackPanel(Spacing = Tokens.Space.s2)

                            try
                                use stream = new MemoryStream(png)
                                tip.Children.Add(Image(Source = new Avalonia.Media.Imaging.Bitmap(stream), MaxWidth = 256.0, MaxHeight = 192.0))
                            with _ ->
                                ()

                            tip.Children.Add(TextBlock(Text = p))
                            ToolTip.SetTip(e, tip)
                        | None -> ToolTip.SetTip(e, (if there then p else $"{p}: no longer there"))
                        recentColumn.Children.Add e

                    let actions = StackPanel(Orientation = Orientation.Horizontal, Spacing = Tokens.Space.s2, Margin = Thickness(0.0, Tokens.Space.s2, 0.0, 0.0))
                    actions.Children.Add(quiet "Remove missing" (fun () -> setRecent (recent |> List.filter (fun p -> File.Exists p || Directory.Exists p))))
                    actions.Children.Add(quiet "Clear" (fun () -> setRecent []))
                    recentColumn.Children.Add actions
                    columns.Children.Add recentColumn

                panel.Children.Add columns

                // The menus that need no tab, as the tabs' bar has them.
                let item (header: string) (act: unit -> unit) =
                    let command = commandNamed header
                    let m = MenuItem(Header = header, InputGesture = (gesturesOf command |> List.tryHead |> Option.toObj))
                    m.Click.Add(fun _ -> act ())
                    m

                let top (header: string) (items: Control list) =
                    let m = MenuItem(Header = header)
                    for i in items do
                        m.Items.Add i |> ignore
                    m

                let recentItem = MenuItem(Header = "Open _recent", IsEnabled = not recent.IsEmpty)
                for p in recent do
                    let there = File.Exists p || Directory.Exists p
                    let m = MenuItem(Header = p, IsEnabled = there)
                    m.Click.Add(fun _ -> openPath p)
                    recentItem.Items.Add m |> ignore

                let menu = Menu(VerticalAlignment = VerticalAlignment.Center)
                menu.Items.Add(
                    top "_File" [ for h in [ "_New script"; "_Open…"; "Open _folder…" ] do
                                      yield item h (fun () -> startCommand (commandNamed h).Msg) :> Control
                                  yield recentItem
                                  yield Separator()
                                  yield item "_Quit" (fun () -> startCommand Quit) ]) |> ignore
                menu.Items.Add(top "_Edit" [ item "_Preferences…" (fun () -> startCommand OpenPreferences) ]) |> ignore
                menu.Items.Add(top "_Help" [ item "_About Tambour" (fun () -> startCommand About) ]) |> ignore

                let bar = Border(Child = menu, Height = Tokens.Height.toolbar, Padding = Thickness(Tokens.Space.s2, 0.0), Background = brush look.Surface, BorderBrush = brush look.Line, BorderThickness = Thickness(0.0, 0.0, 0.0, Tokens.border))
                let view = DockPanel()
                DockPanel.SetDock(bar, Dock.Top)
                view.Children.Add bar
                view.Children.Add(ScrollViewer(Content = panel))
                view :> Control

        // While no tab is open, the start view's keys: those of its menus, as the user has them.
        this.KeyDown.Add(fun e ->
            if workspaces.Count = 0 then
                match commandFor e.Key e.KeyModifiers with
                | Some c when (match c.Msg with NewScript | ChooseOpen _ | OpenPreferences | Quit | About -> true | _ -> false) ->
                    startCommand c.Msg
                    e.Handled <- true
                | _ -> ())

        if opening.IsEmpty then showStart ()
        else
            for script, path in opening do
                add script path

        this.ActualThemeVariantChanged.Add(fun _ ->
            refresh true
            if start.IsVisible then start.Child <- startView.Value ())

        // Closing asks about every workspace's unsaved work first, one at a time; Cancel on any
        // keeps the window open.
        this.Closing.Add(fun e ->
            if not closing && workspaces |> Seq.exists (fun w -> not w.Unsaved.IsEmpty) then
                e.Cancel <- true

                task {
                    let mutable settled = true

                    for w in List.ofSeq workspaces do
                        if settled then
                            let! ok = settle w
                            settled <- ok

                    // Closed again once this close has been cancelled and is over: a Close from
                    // inside its own Closing is not taken.
                    if settled then
                        closing <- true
                        Dispatcher.UIThread.Post(fun () -> this.Close())
                }
                |> ignore)

        this.Closed.Add(fun _ ->
            for w in workspaces do
                (w :> IDisposable).Dispose()

            group.Leave this

            // The group's other windows keep their tabs for the next start.
            match Seq.tryHead group.Windows with
            | Some other -> (other :?> MainWindow).Keep()
            | None -> ())

    new(opening: (string * string option) list) = new MainWindow(opening, new WindowGroup(), false)
    new(script: string, path: string option) = new MainWindow([ script, path ])
    new() = new MainWindow(example, None)
    new(script: string) = new MainWindow(script, None)

    /// The window the app starts with: the tabs open when it last closed, those whose files are
    /// gone dropped with a note, and `path` if one is given; the start view where that is nothing.
    static member Start(path: string option) =
        let saved =
            match App.SettingsDirectory with
            | null -> Preferences.defaults
            | dir -> Preferences.read dir

        let there, gone = saved.Tabs |> List.partition File.Exists
        let window = new MainWindow([ for p in there -> "", Some p ])

        match List.tryItem saved.Shown saved.Tabs |> Option.bind (fun p -> List.tryFindIndex ((=) p) there) with
        | Some i -> window.ShowTab i
        | None -> ()

        match path with
        | Some p -> window.Open p
        | None -> ()

        if not gone.IsEmpty then
            let names = String.Join(", ", gone |> List.map Path.GetFileName)
            window.Tell $"Not opened again, as it is no longer there: {names}"

        window

    /// Shows a tab.
    member _.ShowTab(i: int) = show i

    /// Opens a script or a project directory in a tab: the one showing it, or a new one.
    member _.Open(path: string) = openPath path

    /// Tells something: in the tab shown, or on the start view.
    member _.Tell(text: string) =
        if active >= 0 then
            workspaces.[active].Post(Note text)
        else
            startNote <- text
            start.Child <- startView.Value ()

    /// Whether the start view is shown, there being no tab.
    member _.Starting = workspaces.Count = 0

    /// The scripts of its tabs that have one, in their order.
    member _.Paths = [ for w in workspaces do match w.Location with Some p -> p | None -> () ]

    /// Whether a place on the screen is inside it.
    member this.Covers(screen: PixelPoint) =
        this.IsVisible && Rect(this.ClientSize).Contains(this.PointToClient screen)

    /// Takes a workspace's tab from another window of its group, and shows it.
    member _.Take(w: Workspace) = take w

    /// Moves a tab into a new window of the group, which it returns.
    member _.MoveToNewWindow(w: Workspace) = detach w None

    /// Moves a tab into another window of the group.
    member _.MoveTo(w: Workspace, target: MainWindow) = give w target

    /// Writes the tabs kept for the next start again, as they are now.
    member _.Keep() = refresh true

    /// Closes a tab without asking, what it showed being gone.
    member _.Forget(w: Workspace) = forget w

    /// The workspaces, in their tabs' order, and the one shown.
    member _.Workspaces: Workspace list = List.ofSeq workspaces
    member _.Active = workspaces.[active]

    /// Sends the shown workspace's loop a message.
    member _.Post(msg: Msg) = workspaces.[active].Post msg

    /// The shown workspace's workers' start, for whoever waits on it.
    member _.Started = workspaces.[active].Started

    /// The language service's start.
    member _.LanguageReady = language.Ready

    /// The scripts and projects opened, most recent first.
    member _.Recent = recent

    /// What is asked for a new name, and before files are deleted; a test answers them itself.
    member _.AskName
        with get () = askName
        and set (value: string -> string -> Task<string option>) = askName <- value

    /// Asks which of the other tabs' tables to copy; a test's stands in for the dialog.
    member _.PickTable
        with get () = pickTable
        and set (value: (string * Tambour.Data.Table) list -> Task<Tambour.Data.Table option>) = pickTable <- value

    member _.ConfirmDelete
        with get () = confirmDelete
        and set (value: string list -> Task<bool>) = confirmDelete <- value

    /// What is asked where a question has two answers, as whether a renamed table's uses follow.
    member _.Confirm
        with get () = confirm
        and set (value: string -> string -> string -> Task<bool>) = confirm <- value

    /// What is asked before unsaved work is dropped; a test answers it itself.
    member _.Ask
        with get () = ask
        and set (value: string list -> Task<Unsaved>) = ask <- value
