/// The window: the script on the left, its figure on the right, the output below the script and
/// the figure's controls below the figure. An Elmish loop over FuncUI, whose messages include
/// everything the worker says.
module Tambour.Shell

open System
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
open Avalonia.Interactivity
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Threading
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

    let onTextChanged f = subscription "TextChanged" (fun e -> e.TextChanged) f
    let onProblemsChanged f = subscription "ProblemsChanged" (fun e -> e.ProblemsChanged) f

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

/// The workers, the task that has them started, and the language service.
type Env =
    { Workers: Workers
      Started: Task
      Language: LanguageService }

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
      ActionNote: string }

type Msg =
    | Edited of string
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
    | Failed of exn

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
      ActionNote = "" },
    Cmd.none

let private send (env: Env) (request: Request) : Cmd<Msg> =
    let go (r: Request) : Task<unit> =
        task {
            do! env.Started
            do! env.Workers.SendAsync r
        }

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

let update (env: Env) (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | Edited text -> { model with Script = text; ActionNote = "" }, Cmd.none
    | Run ->
        { model with Running = true; Status = "running…"; Playing = Set.empty },
        send env (Request.Evaluate { Text = model.Script; Path = null })
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

            next, (if e.Ok then loadActions env next else Cmd.none)
        | Reply.Scene frame ->
            match Picture.Decode frame with
            | Ok p -> { model with Picture = p; Readout = None }, Cmd.none
            | Error e -> { model with Status = $"a scene did not decode: {e}" }, Cmd.none
        | Reply.Patch frame when not (isNull model.Picture) && frame.Figure = model.Picture.Figure ->
            match model.Picture.Apply frame with
            | Ok p -> { model with Picture = p }, Cmd.none
            | Error e -> { model with Status = $"a patch did not decode: {e}" }, Cmd.none
        | Reply.Patch _ -> model, Cmd.none
        | Reply.Controls c -> { model with Controls = c.Controls }, Cmd.none
        | Reply.Readout r -> { model with Readout = Some r }, Cmd.none
        | Reply.Output o -> { model with Output = appendOutput model o.Text }, Cmd.none
        | Reply.Exported _ -> model, Cmd.none
        | Reply.Failed message -> { model with Running = false; Status = message }, Cmd.none
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

        { model with Script = a.Text; ActionNote = note }, Cmd.ofMsg Run
    | Failed e -> { model with Status = e.Message }, Cmd.none

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
        StackPanel.create [ StackPanel.orientation Orientation.Horizontal; StackPanel.spacing 4.0; StackPanel.margin (Thickness(0.0, 0.0, 10.0, 4.0)); StackPanel.children children ]

    match Array.tryItem model.Selected model.Figures with
    | Some f when f.Controls > 0 ->
        TextBlock.create [
            TextBlock.margin (Thickness(8.0, 4.0))
            TextBlock.textWrapping TextWrapping.Wrap
            TextBlock.text "This figure is inside interact { }: it is shown and driven, but the actions edit plot { } blocks and Chart.* pipelines only."
        ]
    | Some _ ->
        WrapPanel.create [
            WrapPanel.margin (Thickness(6.0, 4.0))
            WrapPanel.children [
                group [ input "title" "title" 140.0; button "title" "Title" (text "title" "title") ]
                group [ input "x label" "x label" 100.0; button "x label" "X label" (text "x label" "x label") ]
                group [ input "y label" "y label" 100.0; button "y label" "Y label" (text "y label" "y label") ]
                group [ button "log y" "Log y" (fun () -> Some { Kind = "log y"; Text = ""; Lo = 0.0; Hi = 0.0 }) ]
                group [ input "lo" "from" 60.0; input "hi" "to" 60.0; button "y domain" "Y domain" domain ]
                group [ menu "theme" "Theme" themes; menu "legend" "Legend" placements ]
                group [ menu "colour by" "Colour by" model.Columns; menu "facet" "Facet by" model.Columns ]
                group [ button "fit" "Fit" (fun () -> Some { Kind = "fit"; Text = ""; Lo = 0.0; Hi = 0.0 }); menu "layer" "Add layer" geoms ]
            ]
        ]
    | None -> TextBlock.create [ TextBlock.margin (Thickness(8.0, 4.0)); TextBlock.text "Run a script that binds a figure to edit it here." ]

let private problemView (dispatch: Msg -> unit) (d: Diagnostic) : IView =
    Button.create [
        Button.background Brushes.Transparent
        Button.horizontalAlignment HorizontalAlignment.Stretch
        Button.horizontalContentAlignment HorizontalAlignment.Left
        Button.padding (Thickness(6.0, 1.0))
        Button.onClick ((fun _ -> dispatch (GoTo(d.StartLine, d.StartColumn))), SubPatchOptions.Always)
        Button.tip (diagnosticLine d)
        Button.content (
            TextBlock.create [
                // One line each; the whole message is the tip.
                TextBlock.text (Text.RegularExpressions.Regex.Replace(diagnosticLine d, @"\s+", " "))
                TextBlock.textWrapping TextWrapping.NoWrap
                TextBlock.textTrimming TextTrimming.CharacterEllipsis
                TextBlock.foreground (if d.Severity = "error" then Brushes.DarkRed else Brushes.DarkGoldenrod)
            ]
        )
    ]

let view (env: Env) (model: Model) (dispatch: Msg -> unit) : IView =
    let output =
        String.Join("\n", model.Diagnostics |> Array.map diagnosticLine)
        + (if model.Diagnostics.Length > 0 && model.Output.Length > 0 then "\n" else "")
        + model.Output

    DockPanel.create [
        DockPanel.children [
            StackPanel.create [
                StackPanel.dock Dock.Top
                StackPanel.orientation Orientation.Horizontal
                StackPanel.margin (Thickness 6.0)
                StackPanel.spacing 6.0
                StackPanel.children [
                    Button.create [ Button.content "Run"; Button.onClick (fun _ -> dispatch Run) ]
                    Button.create [ Button.content "Cancel"; Button.isEnabled model.Running; Button.onClick (fun _ -> dispatch Cancel) ]
                    ComboBox.create [
                        ComboBox.minWidth 140.0
                        ComboBox.isVisible (model.Figures.Length > 1)
                        ComboBox.dataItems (model.Figures |> Array.map (fun f -> f.Name))
                        ComboBox.selectedIndex model.Selected
                        ComboBox.onSelectedIndexChanged (fun i -> dispatch (Select i))
                    ]
                    label model.Status
                    TextBlock.create [
                        TextBlock.text model.ActionNote
                        TextBlock.verticalAlignment VerticalAlignment.Center
                        TextBlock.foreground (if model.ActionNote.Contains "whole expression" || model.ActionNote.StartsWith "not done" then Brushes.DarkGoldenrod else Brushes.DimGray)
                    ]
                ]
            ]
            Grid.create [
                Grid.columnDefinitions "*,4,*"
                Grid.children [
                    Grid.create [
                        Grid.column 0
                        Grid.rowDefinitions "*,4,Auto,160"
                        Grid.children [
                            Editor.create [
                                Grid.row 0
                                Editor.text model.Script
                                Editor.language env.Language
                                Editor.goTo model.GoTo
                                Editor.onTextChanged (Edited >> dispatch)
                                Editor.onProblemsChanged (Problems >> dispatch)
                            ]
                            GridSplitter.create [ Grid.row 1 ]
                            ScrollViewer.create [
                                Grid.row 2
                                ScrollViewer.maxHeight 110.0
                                ScrollViewer.content (
                                    StackPanel.create [
                                        StackPanel.children (model.Problems |> Array.map (problemView dispatch) |> List.ofArray)
                                    ]
                                )
                            ]
                            TextBox.create [
                                Grid.row 3
                                TextBox.text output
                                TextBox.isReadOnly true
                                TextBox.fontFamily (FontFamily "monospace")
                                TextBox.textWrapping TextWrapping.Wrap
                            ]
                        ]
                    ]
                    GridSplitter.create [ Grid.column 1 ]
                    DockPanel.create [
                        Grid.column 2
                        DockPanel.children [
                            Expander.create [
                                Expander.dock Dock.Top
                                Expander.header "Actions"
                                Expander.isExpanded true
                                Expander.horizontalAlignment HorizontalAlignment.Stretch
                                Expander.content (actionsView model dispatch)
                            ]
                            StackPanel.create [
                                StackPanel.dock Dock.Bottom
                                StackPanel.children (model.Controls |> Array.mapi (fun i c -> controlView dispatch i c (model.Playing.Contains i)) |> List.ofArray)
                            ]
                            Pane.create [
                                Pane.picture model.Picture
                                Pane.readout model.Readout
                                Pane.onInput (Input >> dispatch)
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

/// The worker's replies, and the clock of a playing time control.
let private subscriptions (env: Env) (window: Window) (model: Model) : Sub<Msg> =
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

    let keys (dispatch: Msg -> unit) : IDisposable =
        let handler =
            EventHandler<KeyEventArgs>(fun _ e ->
                match e.Key, e.KeyModifiers with
                | Key.Enter, KeyModifiers.Control
                | Key.F5, _ ->
                    e.Handled <- true
                    dispatch Run
                | Key.Escape, _ ->
                    e.Handled <- true
                    dispatch Cancel
                | _ -> ())

        window.AddHandler(InputElement.KeyDownEvent, handler, RoutingStrategies.Tunnel) |> ignore

        { new IDisposable with
            member _.Dispose() = window.RemoveHandler(InputElement.KeyDownEvent, handler) }

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

    [ [ "replies" ], replies
      [ "keys" ], keys
      if not model.Playing.IsEmpty then [ "clock" ], clock ]

/// The main window, with its workers, which end when it closes.
type MainWindow(script: string) as this =
    inherit HostWindow()

    let workers =
        new Workers(Command.ofThisApp (), { References = [||]; Prelude = prelude; Skia = { Dpi = float Marey.Space.screenDpi; FontFiles = [||] } })

    let language = new LanguageService(Command.ofThisApp (), { References = [||]; Prelude = prelude })

    let env =
        { Workers = workers
          Started = workers.StartAsync()
          Language = language }

    do
        this.Title <- "Tambour"
        this.Width <- 1280.0
        this.Height <- 800.0

        this.Closed.Add(fun _ ->
            (workers :> IDisposable).Dispose()
            (language :> IDisposable).Dispose())

        Program.mkProgram (init script) (update env) (view env)
        |> Program.withHost this
        |> Program.withSubscription (subscriptions env this)
        |> Program.runWithAvaloniaSyncDispatch ()

    new() = new MainWindow(example)

    /// The workers' start, for whoever waits on it.
    member _.Started = env.Started

    /// The language service's start.
    member _.LanguageReady = language.Ready
