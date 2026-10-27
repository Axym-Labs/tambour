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

/// A new script in a project with tables: the prelude loaded and opened, so the tables are in
/// reach by name, as they are when the script runs outside Tambour.
let newScriptWithTables = "#load \"prelude.fsx\"\nopen Prelude\n\n"

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
      CloseProject: string -> unit }

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
      /// The scripts and projects opened, most recent first, as the window keeps them.
      Recent: string list }

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
    | CloseTab
    | CycleTab of int
    | Failed of exn

/// The bottom panel's height where a project has none.
let defaultBottom = 170.0

let init (script: string) () : Model * Cmd<Msg> =
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
      BottomOpen = true
      BottomTab = 0
      Bottom = defaultBottom
      ActionsOpen = true
      Recent = [] },
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
            let scripts = FilePickerFileType("F# scripts", Patterns = [| "*.fsx" |])
            let! picked = window.StorageProvider.OpenFilePickerAsync(FilePickerOpenOptions(Title = "Open a script", FileTypeFilter = [| scripts |]))
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
                Bottom = model.Bottom }
    | _ -> ()

/// The tables by name, as the history holds them.
let private tablesOf (model: Model) =
    model.Tables |> List.map (fun s -> s.Table.Name, s.Table) |> Map.ofList

/// The model with the tables the history gives: a table keeps its file, a new one is kept as CSV
/// until it is saved, and a removed one goes.
let private withTables (model: Model) (tables: Map<string, Tambour.Data.Table>) =
    let kept =
        model.Tables
        |> List.choose (fun s -> tables.TryFind s.Table.Name |> Option.map (fun t -> { s with Table = t }))

    let added =
        tables
        |> Map.toList
        |> List.filter (fun (name, _) -> not (kept |> List.exists (fun s -> s.Table.Name = name)))
        |> List.map (fun (name, t) -> { Tambour.Data.Prelude.Table = t; Tambour.Data.Prelude.File = name + ".csv" })

    { model with Tables = kept @ added }

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

    let panel = StackPanel(Spacing = 12.0, Margin = Thickness 18.0)
    panel.Children.Add text
    panel.Children.Add close

    let window =
        Window(Title = "About Tambour", Width = 420.0, SizeToContent = SizeToContent.Height, CanResize = false, Content = panel, WindowStartupLocation = WindowStartupLocation.CenterOwner)

    close.Click.Add(fun _ -> window.Close())
    window

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
            let state = History.Entry.undo { Script = model.Script; Tables = tablesOf model } entry
            toState env { model with History = history; ActionNote = "undone" } state (History.Entry.changesSchema entry)
        | None -> model, Cmd.none
    | Redo ->
        match History.redo model.History with
        | Some(entry, history) ->
            let state = History.Entry.redo { Script = model.Script; Tables = tablesOf model } entry
            toState env { model with History = history; ActionNote = "redone" } state (History.Entry.changesSchema entry)
        | None -> model, Cmd.none
    | Run ->
        { model with Running = true; Status = "running…"; Playing = Set.empty },
        send env (Request.Evaluate { Text = model.Script; Path = Option.toObj model.Path })
    | Cancel when model.Running ->
        { model with Running = false; Status = "cancelled" }, Cmd.OfTask.attempt env.Workers.CancelAsync () Failed
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
        | Reply.Failed message -> { model with Running = false; Status = message }, Cmd.none
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
            let opened = Project.resolve path
            env.Tabs.Opened(Path.GetFullPath path)

            // The project's tables, their prelude written if it changed, and their columns sent.
            let tables, unread =
                match opened.Project with
                | Some dir ->
                    let tables, unread = Tables.load dir
                    if not tables.IsEmpty then Tables.writePrelude dir tables |> ignore
                    tables, unread
                | None -> [], []

            // A new script in a project with tables begins with them in reach.
            let text =
                if File.Exists opened.Script then File.ReadAllText opened.Script
                elif tables.IsEmpty then ""
                else newScriptWithTables

            let note =
                match opened.Settings, unread with
                | _, why :: _ -> $"a table did not read: {why}"
                | Some s, [] when s.Marey <> Project.mareyVersion -> $"last run on Marey {s.Marey}; this is {Project.mareyVersion}"
                | _ -> ""

            { model with
                Script = text
                Saved = text
                Path = Some opened.Script
                Project = opened.Project
                Settings = opened.Settings
                Split = (match opened.Settings with Some s when s.Split > 0.05 && s.Split < 0.95 -> s.Split | _ -> model.Split)
                Bottom = (match opened.Settings with Some s when s.Bottom > 0.0 -> s.Bottom | _ -> model.Bottom)
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
                { model with Saved = model.Script; Status = $"saved {Path.GetFileName path}" }, Cmd.ofMsg Run
            with e ->
                { model with Status = $"not saved: {e.Message}" }, Cmd.none
        | None ->
            model, Cmd.OfTask.either (fun () -> pickSave (topOf env) "Save the workspace: a folder named as its script" "figure.fsx" "fsx") () (Option.map SaveAs >> Option.defaultValue Dismissed) Failed
    | SaveAs path ->
        // A new workspace is a directory of its own, named as its script: the script, its tables
        // in `tables/`, and `workspace.json`.
        try
            let name = Path.GetFileNameWithoutExtension path
            let dir = Path.Combine(Path.GetDirectoryName path, name)
            Directory.CreateDirectory dir |> ignore
            Project.makeWorkspace dir
            env.Tabs.Opened dir
            { model with Path = Some(Path.Combine(dir, name + ".fsx")); Project = Some dir }, Cmd.ofMsg Save
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
        let format = model.Inputs.TryFind "export format" |> Option.defaultValue "png"
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
        { model with Exporting = Some path; Status = "exporting…" },
        send env (Request.Export { Format = format; Width = width; Height = height; Skia = skiaOf model })
    | ExportTo _ -> { model with Status = "there is no figure to export" }, Cmd.none
    | Dismissed -> model, Cmd.none
    | SelectTab tab -> { model with Tab = tab; Header = None }, Cmd.none
    | TableEdited edits ->
        let tables = edits |> List.fold Tambour.Data.TableEdit.apply (tablesOf model)
        let schema = edits |> List.exists Tambour.Data.TableEdit.changesSchema

        toState
            env
            { model with History = History.recordTables edits model.History; ActionNote = "" }
            { Script = model.Script; Tables = tables }
            schema
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
        { model with Tab = model.Tables.Length + 1 }, Cmd.ofMsg (TableEdited [ Tambour.Data.AddTable t ])
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
            { model with Tab = model.Tables.Length + 1 }, Cmd.ofMsg (TableEdited [ Tambour.Data.AddTable named ])
    | RemoveTable name ->
        match model.Tables |> List.tryFind (fun s -> s.Table.Name = name) with
        | Some s -> { model with Tab = 0 }, Cmd.ofMsg (TableEdited [ Tambour.Data.RemoveTable s.Table ])
        | None -> model, Cmd.none
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
    | ToggleBottom -> { model with BottomOpen = not model.BottomOpen }, Cmd.none
    | ShowBottom tab -> { model with BottomOpen = true; BottomTab = tab }, Cmd.none
    | ToggleActions -> { model with ActionsOpen = not model.ActionsOpen }, Cmd.none
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
        let name = model.Path |> Option.map Path.GetFileName |> Option.defaultValue "figure.fsx"
        model, Cmd.OfTask.either (fun () -> pickSave (topOf env) "Save the workspace as: a folder named as its script" name "fsx") () (Option.map SaveAs >> Option.defaultValue Dismissed) Failed
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
    | OpenTab path ->
        env.Tabs.Open path
        model, Cmd.none
    | CloseTab ->
        env.Tabs.Close()
        model, Cmd.none
    | CycleTab step ->
        env.Tabs.Cycle step
        model, Cmd.none
    | Failed e -> { model with Status = e.Message }, Cmd.none

/// The one spacing unit; every margin and gap is a multiple of it.
let gap = 6.0

let private label (text: string) : IView =
    TextBlock.create [ TextBlock.text text; TextBlock.verticalAlignment VerticalAlignment.Center; TextBlock.margin (Thickness(0.0, 0.0, 8.0, 0.0)) ]

let private controlView (dispatch: Msg -> unit) (index: int) (c: ControlInfo) (playing: bool) : IView =
    let row (children: IView list) : IView =
        DockPanel.create [ DockPanel.margin (Thickness(8.0, 2.0)); DockPanel.children children ]

    let shown = TextBlock.create [ TextBlock.dock Dock.Right; TextBlock.text c.Shown; TextBlock.width 90.0; TextBlock.margin (Thickness(12.0, 0.0, 0.0, 0.0)); TextBlock.verticalAlignment VerticalAlignment.Center ]

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
            TextBlock.create [ TextBlock.dock Dock.Left; TextBlock.text c.Label; TextBlock.margin (Thickness(8.0, 0.0)); TextBlock.verticalAlignment VerticalAlignment.Center ]
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

    let input field (watermark: string) (width: float) : IView =
        TextBox.create [
            TextBox.placeHolderText watermark
            TextBox.width width
            TextBox.text (model.Inputs.TryFind field |> Option.defaultValue "")
            TextBox.onTextChanged (fun t -> if Some t <> model.Inputs.TryFind field then dispatch (Typed(field, t)))
        ]

    let button kind (label: string) (spec: unit -> ActionSpec option) : IView =
        Button.create [
            Button.content label
            Button.isEnabled (enabled kind)
            Button.tip (tip kind)
            // Every render's handler, since it reads what is typed now.
            Button.onClick ((fun _ -> spec () |> Option.iter (Act >> dispatch)), SubPatchOptions.Always)
        ]

    /// A menu that acts on a choice and then shows its label again.
    let menu kind (label: string) (items: string[]) : IView =
        ComboBox.create [
            ComboBox.placeholderText label
            ComboBox.minWidth 120.0
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

    let text kind field = fun () -> Some { Kind = kind; Text = model.Inputs.TryFind field |> Option.defaultValue ""; Lo = 0.0; Hi = 0.0 }

    let domain () =
        let parse field =
            match Double.TryParse(model.Inputs.TryFind field |> Option.defaultValue "", Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
            | true, v -> Some v
            | _ -> None

        match parse "lo", parse "hi" with
        | Some lo, Some hi -> Some { Kind = "y domain"; Text = ""; Lo = lo; Hi = hi }
        | _ ->
            dispatch (Failed(FormatException "a y domain wants two numbers"))
            None

    let group (children: IView list) : IView =
        StackPanel.create [ StackPanel.orientation Orientation.Horizontal; StackPanel.spacing gap; StackPanel.margin (Thickness(0.0, 0.0, 2.0 * gap, gap)); StackPanel.children children ]

    match Array.tryItem model.Selected model.Figures with
    | Some f when f.Controls > 0 ->
        TextBlock.create [
            TextBlock.margin (Thickness(8.0, 4.0))
            TextBlock.textWrapping TextWrapping.Wrap
            TextBlock.text "This figure is inside interact { }: it is shown and driven, but the actions edit plot { } blocks and Chart.* pipelines only."
        ]
    | Some _ ->
        let section (name: string) (children: IView list) : IView =
            DockPanel.create [
                DockPanel.margin (Thickness(0.0, 0.0, 0.0, gap))
                DockPanel.children [
                    TextBlock.create [
                        TextBlock.dock Dock.Left
                        TextBlock.width 64.0
                        TextBlock.margin (Thickness(0.0, 6.0, 0.0, 0.0))
                        TextBlock.text name
                        TextBlock.fontSize 12.0
                        TextBlock.foreground (SolidColorBrush model.Look.Dim)
                    ]
                    WrapPanel.create [ WrapPanel.children children ]
                ]
            ]

        StackPanel.create [
            StackPanel.margin (Thickness(2.0 * gap, gap))
            StackPanel.children [
                section "Labels" [
                    group [ input "title" "title" 140.0; button "title" "Title" (text "title" "title") ]
                    group [ input "x label" "x label" 100.0; button "x label" "X label" (text "x label" "x label") ]
                    group [ input "y label" "y label" 100.0; button "y label" "Y label" (text "y label" "y label") ]
                ]
                section "Axes" [
                    group [ button "log y" "Log y" (fun () -> Some { Kind = "log y"; Text = ""; Lo = 0.0; Hi = 0.0 }) ]
                    group [ input "lo" "from" 60.0; input "hi" "to" 60.0; button "y domain" "Y domain" domain ]
                ]
                section "Look" [
                    group [ menu "theme" "Theme" themes; menu "legend" "Legend" placements; menu "colour by" "Colour by" model.Columns ]
                ]
                section "Layers" [
                    group [ menu "layer" "Add layer" geoms; button "fit" "Fit" (fun () -> Some { Kind = "fit"; Text = ""; Lo = 0.0; Hi = 0.0 }); menu "facet" "Facet by" model.Columns ]
                ]
            ]
        ]
    | None -> TextBlock.create [ TextBlock.margin (Thickness(8.0, 4.0)); TextBlock.text "Run a script that binds a figure to edit it here." ]

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
        Button.padding (Thickness(6.0, 1.0))
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
        StackPanel.spacing 6.0
        StackPanel.margin (Thickness(6.0, 4.0))
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
    DockPanel.create [
        DockPanel.children [
            StackPanel.create [
                StackPanel.dock Dock.Top
                StackPanel.orientation Orientation.Horizontal
                StackPanel.spacing 6.0
                StackPanel.margin (Thickness(6.0, 4.0))
                StackPanel.children [
                    Button.create [ Button.content "Add column"; Button.onClick ((fun _ -> dispatch (EditHeader(t.Name, -1 - t.Columns.Length))), SubPatchOptions.Always) ]
                    Button.create [
                        Button.content "Add 10 rows"
                        Button.onClick ((fun _ -> dispatch (TableEdited [ Tambour.Data.TableEdit.addRows t t.Rows 10 ])), SubPatchOptions.Always)
                    ]
                    Button.create [ Button.content "Remove table"; Button.onClick ((fun _ -> dispatch (RemoveTable t.Name)), SubPatchOptions.Always) ]
                    Button.create [ Button.content "Plot selected columns"; Button.onClick (fun _ -> dispatch PlotSelected) ]
                    label $"{t.Rows} rows. Right-click, or the Table menu, for what can be done to cells, rows and columns; Ctrl+click or Alt+drag in a figure picks rows."
                ]
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
      c "_File" "_Export figure…" "Ctrl+E" [] ChooseExport (fun m -> not (isNull m.Picture))
      c "_File" "_Quit" "Ctrl+Q" [] Quit always
      c "_Edit" "_Undo" "Ctrl+Z" [] Undo (fun m -> not m.History.Done.IsEmpty)
      c "_Edit" "_Redo" "Ctrl+Y" [ "Ctrl+Shift+Z" ] Redo (fun m -> not m.History.Undone.IsEmpty)
      c "_Run" "_Run" "F5" [ "Ctrl+Enter" ] Run always
      c "_Run" "_Cancel" "Escape" [] Cancel (fun m -> m.Running)
      c "_Table" "_New table" "Ctrl+T" [] NewTable always
      c "_Table" "_Import table…" "Ctrl+Shift+I" [] ChooseImport always
      c "_Table" "_Plot selected columns" "Ctrl+Shift+P" [] PlotSelected (fun m -> m.TableSelection.IsSome)
      for kind, header, keys in GridCommand.all do
          { c "_Table" header keys [] (Grid kind) (fun m -> m.Tab > 0) with InGrid = true }
      c "_View" "_Problems" "Ctrl+Shift+M" [] (ShowBottom 0) always
      c "_View" "_Output" "Ctrl+Shift+U" [] (ShowBottom 1) always
      c "_View" "_Bottom panel" "Ctrl+J" [] ToggleBottom always
      c "_View" "_Actions" "Ctrl+Shift+A" [] ToggleActions always
      c "_View" "_Next tab" "Ctrl+Tab" [] (CycleTab 1) always
      c "_View" "Pre_vious tab" "Ctrl+Shift+Tab" [] (CycleTab -1) always
      c "_Help" "_About Tambour" "" [] About always ]

/// The command a key and its modifiers take, if one does.
let commandFor (key: Key) (modifiers: KeyModifiers) =
    commands
    |> List.tryFind (fun c -> (Option.toList c.Gesture @ c.Also) |> List.exists (fun g -> g.Key = key && g.KeyModifiers = modifiers))

let private commandNamed (header: string) = commands |> List.find (fun c -> c.Header = header)

/// A small drawn icon, in the look's text colour: stroked unless it is a solid shape.
let private icon (look: Palette.Look) (data: string) (solid: bool) (enabled: bool) : IView =
    Avalonia.FuncUI.DSL.Path.create [
        Shapes.Path.data (Geometry.Parse data)
        Shapes.Path.width 16.0
        Shapes.Path.height 16.0
        Shapes.Path.opacity (if enabled then 1.0 else 0.4)
        if solid then
            Shapes.Path.fill (SolidColorBrush look.Accent)
        else
            Shapes.Path.stroke (SolidColorBrush look.Text)
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
            match command.Gesture with
            | Some g -> MenuItem.inputGesture g
            | None -> ()
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
            Menu.dock Dock.Top
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

    let tool (header: string) : IView =
        let command = commandNamed header
        let data, solid = icons.[header]
        let enabled = command.Enabled model
        let name = header.Replace("_", "").TrimEnd('…')

        Button.create [
            Button.content (icon look data solid enabled)
            Button.isEnabled enabled
            Button.padding (Thickness(gap, gap * 0.75))
            Button.background Brushes.Transparent
            Button.tip (match command.Gesture with Some g -> $"{name} ({g})" | None -> name)
            Button.onClick ((fun _ -> dispatch command.Msg), SubPatchOptions.Always)
        ]

    let toolbar =
        Border.create [
            Border.dock Dock.Top
            Border.borderBrush (brush look.Line)
            Border.borderThickness (Thickness(0.0, 0.0, 0.0, 1.0))
            Border.child (
                StackPanel.create [
                    StackPanel.orientation Orientation.Horizontal
                    StackPanel.margin (Thickness(gap, gap / 2.0))
                    StackPanel.spacing (gap / 2.0)
                    StackPanel.children [
                        tool "_Run"
                        tool "_Cancel"
                        Border.create [ Border.width 1.0; Border.margin (Thickness(gap, gap / 2.0)); Border.background (brush look.Line) ]
                        tool "_Undo"
                        tool "_Redo"
                        Border.create [ Border.width 1.0; Border.margin (Thickness(gap, gap / 2.0)); Border.background (brush look.Line) ]
                        tool "_Export figure…"
                        ComboBox.create [
                            ComboBox.dataItems exportFormats
                            ComboBox.verticalAlignment VerticalAlignment.Center
                            ComboBox.tip "the format Export writes"
                            ComboBox.selectedIndex (exportFormats |> Array.tryFindIndex (fun f -> Some f = model.Inputs.TryFind "export format") |> Option.defaultValue 0)
                            ComboBox.onSelectedIndexChanged ((fun i -> if i >= 0 then dispatch (Typed("export format", exportFormats.[i]))), SubPatchOptions.Always)
                        ]
                        ComboBox.create [
                            ComboBox.minWidth 140.0
                            ComboBox.margin (Thickness(gap, 0.0, 0.0, 0.0))
                            ComboBox.verticalAlignment VerticalAlignment.Center
                            ComboBox.isVisible (model.Figures.Length > 1)
                            ComboBox.tip "the figure shown"
                            ComboBox.dataItems (model.Figures |> Array.map (fun f -> f.Name))
                            ComboBox.selectedIndex model.Selected
                            ComboBox.onSelectedIndexChanged (fun i -> dispatch (Select i))
                        ]
                        TextBlock.create [
                            TextBlock.text model.ActionNote
                            TextBlock.margin (Thickness(gap, 0.0, 0.0, 0.0))
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
            TextBlock.fontSize 12.0
            TextBlock.verticalAlignment VerticalAlignment.Center
            TextBlock.margin (Thickness(gap, 0.0))
            TextBlock.foreground (brush colour)
        ]

    let statusBar =
        Border.create [
            Border.dock Dock.Bottom
            Border.background (brush look.Raised)
            Border.borderBrush (brush look.Line)
            Border.borderThickness (Thickness(0.0, 1.0, 0.0, 0.0))
            Border.child (
                DockPanel.create [
                    DockPanel.margin (Thickness(gap, 2.0))
                    DockPanel.children [
                        StackPanel.create [
                            StackPanel.dock Dock.Right
                            StackPanel.orientation Orientation.Horizontal
                            StackPanel.children [
                                Button.create [
                                    Button.padding (Thickness(gap, 1.0))
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
                                            (if errors > 0 then look.Error elif warnings > 0 then look.Warning else look.Text)
                                    )
                                ]
                                statusText $"Ln {fst model.Caret}, Col {snd model.Caret}" look.Text
                                statusText $"Marey {Project.mareyVersion}" look.Dim
                            ]
                        ]
                        statusText model.Status look.Text
                    ]
                ]
            )
        ]

    let bottomPanel =
        TabControl.create [
            Grid.row 2
            TabControl.isVisible model.BottomOpen
            TabControl.selectedIndex model.BottomTab
            TabControl.onSelectedIndexChanged ((fun i -> if i >= 0 && i <> model.BottomTab then dispatch (ShowBottom i)), SubPatchOptions.Always)
            TabControl.viewItems [
                TabItem.create [
                    TabItem.header (if model.Problems.Length = 0 then "Problems" else $"Problems ({model.Problems.Length})")
                    TabItem.fontSize 13.0
                    TabItem.content (
                        ScrollViewer.create [
                            ScrollViewer.content (
                                StackPanel.create [
                                    StackPanel.children [
                                        if model.Problems.Length = 0 then
                                            TextBlock.create [ TextBlock.margin (Thickness gap); TextBlock.text "No problems in the script."; TextBlock.foreground (brush look.Dim) ]
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
                    TabItem.fontSize 13.0
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

    let actionsHeader =
        Button.create [
            Button.horizontalAlignment HorizontalAlignment.Left
            Button.horizontalContentAlignment HorizontalAlignment.Left
            Button.background Brushes.Transparent
            Button.padding (Thickness(2.0 * gap, gap / 2.0))
            Button.tip "show or hide the figure's actions (Ctrl+Shift+A)"
            Button.onClick ((fun _ -> dispatch ToggleActions), SubPatchOptions.Always)
            Button.content (
                TextBlock.create [
                    TextBlock.text ((if model.ActionsOpen then "▾ " else "▸ ") + "Actions")
                    TextBlock.fontWeight FontWeight.SemiBold
                    TextBlock.foreground (brush look.Text)
                ]
            )
        ]

    DockPanel.create [
        DockPanel.background (brush look.Surface)
        DockPanel.children [
            menuBar
            toolbar
            statusBar
            Grid.create [
                Grid.columnDefinitions $"{inv split}*,4,{inv (1.0 - split)}*"
                Grid.children [
                    Grid.create [
                        Grid.column 0
                        Grid.rowDefinitions (if model.BottomOpen then $"*,4,{inv model.Bottom}" else "*,0,0")
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
                            GridSplitter.create [
                                Grid.row 1
                                GridSplitter.isVisible model.BottomOpen
                                GridSplitter.background (brush look.Line)
                                Primitives.Thumb.onDragCompleted dragged
                            ]
                            bottomPanel
                        ]
                    ]
                    GridSplitter.create [ Grid.column 1; GridSplitter.background (brush look.Line); Primitives.Thumb.onDragCompleted dragged ]
                    TabControl.create [
                        Grid.column 2
                        TabControl.name "sheets"
                        TabControl.selectedIndex model.Tab
                        TabControl.onSelectedIndexChanged ((fun i -> if i >= 0 && i <> model.Tab then dispatch (SelectTab i)), SubPatchOptions.Always)
                        TabControl.viewItems [
                            TabItem.create [
                                TabItem.header "Figure"
                                TabItem.fontSize 15.0
                                TabItem.content (
                                    DockPanel.create [
                                        DockPanel.children [
                                            DockPanel.create [
                                                DockPanel.dock Dock.Top
                                                DockPanel.children [
                                                    TextBlock.create [
                                                        TextBlock.dock Dock.Right
                                                        TextBlock.margin (Thickness(gap, 0.0, 2.0 * gap, 0.0))
                                                        TextBlock.verticalAlignment VerticalAlignment.Center
                                                        TextBlock.fontSize 12.0
                                                        TextBlock.foreground (brush look.Dim)
                                                        TextBlock.text "drag to zoom · Shift+drag pans · double-click resets"
                                                    ]
                                                    actionsHeader
                                                ]
                                            ]
                                            Border.create [
                                                Border.dock Dock.Top
                                                Border.isVisible model.ActionsOpen
                                                Border.borderBrush (brush look.Line)
                                                Border.borderThickness (Thickness(0.0, 0.0, 0.0, 1.0))
                                                Border.child (actionsView model dispatch)
                                            ]
                                            StackPanel.create [
                                                StackPanel.dock Dock.Bottom
                                                StackPanel.children (model.Controls |> Array.mapi (fun i c -> controlView dispatch i c (model.Playing.Contains i)) |> List.ofArray)
                                            ]
                                            Pane.create [
                                                Pane.picture model.Picture
                                                Pane.readout model.Readout
                                                Pane.marks model.Marks
                                                Pane.onInput (Input >> dispatch)
                                                Pane.onPicked (Pick >> dispatch)
                                            ]
                                        ]
                                    ]
                                )
                            ]
                            for source in model.Tables do
                                TabItem.create [ TabItem.header source.Table.Name; TabItem.fontSize 15.0; TabItem.content (tableView model dispatch source.Table) ]
                        ]
                    ]
                ]
            ]
        ]
    ]

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

/// The window's title: the script's name, marked while anything is unsaved.
let title (model: Model) =
    let mark = if (unsaved model).IsEmpty then "" else " •"
    $"{scriptName model}{mark} — Tambour"

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
      // Made again when what can be taken changes, as the keys read it.
      [ "keys"; commands |> List.map (fun c -> if c.Enabled model then "1" else "0") |> String.concat "" ], keys
      [ "post" ], exposed
      [ "look" ], look
      [ "title"; title model ], retitle (title model)
      match model.Path with
      | Some path when Directory.Exists(Path.GetDirectoryName path) -> [ "watch"; path ], watch path
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
    let buttons = StackPanel(Orientation = Orientation.Horizontal, Spacing = 8.0, HorizontalAlignment = HorizontalAlignment.Right)
    buttons.Children.Add discard
    buttons.Children.Add keep
    buttons.Children.Add save
    let panel = StackPanel(Spacing = 12.0, Margin = Thickness 18.0)
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

/// A workspace: everything a window held before there were tabs. Its script or project, figure,
/// tables, history and layout, run by its own program on its own workers, which end when it is
/// disposed. The language service is the window's, shared by its workspaces. It opens `path` once
/// started, if one is given, and `script` otherwise.
type Workspace(script: string, path: string option, language: LanguageService, tabs: Tabs) as this =
    inherit HostControl()

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
          Tabs = tabs }

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
        |> Program.runWithAvaloniaSyncDispatch ()

    /// Sends the workspace's loop a message, as its own controls do.
    member _.Post(msg: Msg) = dispatch msg

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

    /// The script's name, as its tab shows it.
    member _.ScriptName = scriptName latest

    /// What is unsaved in it, by name; empty when everything is on disk.
    member _.Unsaved = unsaved latest

    /// Where its script is, and its project directory, if it has them.
    member _.Path = latest.Path
    member _.Project = latest.Project

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
            idle.Dispose()
            (workers :> IDisposable).Dispose()

/// The main window: a tab per workspace, and the language service they share, which end when it
/// closes. Closing a tab or the window asks first about what is unsaved.
type MainWindow(script: string, path: string option) as this =
    inherit Window()

    let language = new LanguageService(Command.ofThisApp (), { References = [||]; Prelude = prelude })
    let workspaces = ResizeArray<Workspace>()
    let mutable active = -1
    let strip = StackPanel(Orientation = Orientation.Horizontal)
    let deck = Avalonia.Controls.Grid()
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
    let mutable ask: string list -> Task<Unsaved> = fun _ -> Task.FromResult Keep

    let look () = Palette.forVariant this.ActualThemeVariant

    let labelOf (w: Workspace) = w.ScriptName + (if w.Unsaved.IsEmpty then "" else " •")

    let rec refresh (force: bool) =
        let now = [ for w in workspaces -> labelOf w ] @ [ string active ]

        if force || now <> labels then
            labels <- now
            let look = look ()
            strip.Children.Clear()

            workspaces
            |> Seq.iteri (fun i w ->
                let shown = i = active
                let name = TextBlock(Text = labelOf w, VerticalAlignment = VerticalAlignment.Center, Foreground = SolidColorBrush(if shown then look.Text else look.Dim))
                let close = Button(Content = "×", Padding = Thickness(4.0, 0.0), Background = Brushes.Transparent, VerticalAlignment = VerticalAlignment.Center)
                ToolTip.SetTip(close, "Close tab (Ctrl+W)")
                close.Click.Add(fun _ -> closeTab w |> ignore)
                let row = StackPanel(Orientation = Orientation.Horizontal, Spacing = 6.0)
                row.Children.Add name
                row.Children.Add close

                let tab =
                    Button(
                        Content = row,
                        Padding = Thickness(10.0, 3.0, 4.0, 3.0),
                        CornerRadius = CornerRadius(4.0, 4.0, 0.0, 0.0),
                        Background = SolidColorBrush(if shown then look.Surface else look.Raised),
                        BorderThickness = Thickness(0.0)
                    )

                ToolTip.SetTip(tab, (match w.Path with Some p -> p | None -> "not saved yet"))
                tab.Click.Add(fun _ -> show i)
                strip.Children.Add tab)

    and show (i: int) =
        if i >= 0 && i < workspaces.Count then
            active <- i
            workspaces |> Seq.iteri (fun j w -> w.IsVisible <- (j = i))
            this.Title <- workspaces.[i].Title
            refresh false

    and add (script: string) (path: string option) =
        let mutable self: Workspace = Unchecked.defaultof<_>

        let tabs =
            { New = fun () -> add "" None
              Open = openPath
              Close = fun () -> closeTab self |> ignore
              Cycle = fun step -> if workspaces.Count > 0 then show ((active + step + workspaces.Count) % workspaces.Count)
              Opened = fun path -> setRecent (Preferences.opened path recent)
              ForgetRecent =
                fun missingOnly ->
                    setRecent (if missingOnly then recent |> List.filter (fun p -> File.Exists p || Directory.Exists p) else [])
              SaveAll = fun () -> saveAll () |> ignore
              CloseProject = fun dir -> closeProject dir |> ignore }

        let w = new Workspace(script, path, language, tabs)
        self <- w
        w.TitleChanged.Add(fun t -> if workspaces.IndexOf w = active then this.Title <- t)
        w.Updated.Add(fun _ -> refresh false)
        workspaces.Add w
        deck.Children.Add w
        w.Post(RecentChanged recent)
        show (workspaces.Count - 1)

    and openPath (path: string) =
        let full = Path.GetFullPath path
        let showing (w: Workspace) = w.Path = Some full || w.Project = Some full

        match workspaces |> Seq.tryFindIndex showing with
        | Some i ->
            setRecent (Preferences.opened full recent)
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

    and closeTab (w: Workspace) : Task =
        task {
            let! settled = settle w

            if settled && workspaces.Contains w then
                let i = workspaces.IndexOf w
                workspaces.RemoveAt i
                deck.Children.Remove w |> ignore
                (w :> IDisposable).Dispose()

                if workspaces.Count = 0 then add "" None
                else show (min i (workspaces.Count - 1))

                refresh true
        }

    do
        this.Title <- "Tambour"
        // A button that cannot be taken now shows dimmed, on no box of its own.
        this.Resources.["ButtonBackgroundDisabled"] <- Brushes.Transparent
        this.Resources.["ButtonBorderBrushDisabled"] <- Brushes.Transparent
        this.Width <- 1280.0
        this.Height <- 800.0
        ask <-
            fun names ->
                match App.AnswerUnsaved with
                | Some answer -> Task.FromResult answer
                | None -> askToSave this (look ()) names
        let tabsRow = ScrollViewer(Content = strip, HorizontalScrollBarVisibility = Primitives.ScrollBarVisibility.Auto, VerticalScrollBarVisibility = Primitives.ScrollBarVisibility.Disabled)
        let layout = DockPanel()
        DockPanel.SetDock(tabsRow, Dock.Top)
        layout.Children.Add tabsRow
        layout.Children.Add deck
        this.Content <- layout
        add script path
        this.ActualThemeVariantChanged.Add(fun _ -> refresh true)

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

            (language :> IDisposable).Dispose())

    new() = new MainWindow(example, None)
    new(script: string) = new MainWindow(script, None)

    /// The workspaces, in their tabs' order, and the one shown.
    member _.Workspaces = List.ofSeq workspaces
    member _.Active = workspaces.[active]

    /// Sends the shown workspace's loop a message.
    member _.Post(msg: Msg) = workspaces.[active].Post msg

    /// The shown workspace's workers' start, for whoever waits on it.
    member _.Started = workspaces.[active].Started

    /// The language service's start.
    member _.LanguageReady = language.Ready

    /// The scripts and projects opened, most recent first.
    member _.Recent = recent

    /// What is asked before unsaved work is dropped; a test answers it itself.
    member _.Ask
        with get () = ask
        and set (value: string list -> Task<Unsaved>) = ask <- value
