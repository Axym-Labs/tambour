/// The Preferences dialog (Edit → Preferences…): a face on `settings.json`. Each change is written
/// to the file and put in force at once; a change made to the file by hand shows in an open dialog;
/// a file that cannot be read is shown as the defaults, with a note.
module Tambour.PreferencesView

open System
open System.IO
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Input
open Avalonia.Interactivity
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Threading

/// A command whose shortcut the dialog shows and changes: its menu, its name there, its own keys
/// (the shortcut first), and whether its keys act only in a table's grid.
type Shortcut =
    { Menu: string
      Header: string
      Keys: string list
      InGrid: bool }

/// A command's keys now: as the user changed them, or its own.
let keysOf (preferences: Preferences.Preferences) (s: Shortcut) =
    match preferences.Keys.TryFind s.Header with
    | Some "" -> []
    | Some k -> [ k ]
    | None -> s.Keys

/// A gesture as menus show it.
let shown (keys: string) =
    try
        (KeyGesture.Parse keys).ToString("p", null)
    with _ ->
        keys

let private plain (header: string) = header.Replace("_", "")

let private pages = [ "Appearance"; "Editor"; "Keyboard shortcuts"; "Export"; "Recent files" ]

type PreferencesWindow(shortcuts: Shortcut list, openFile: string -> unit) as this =
    inherit Window()

    let mutable page = "Appearance"
    let mutable filter = ""
    /// The command whose keys are being recorded, and keys pressed for it that another command has.
    let mutable recording: Shortcut option = None
    let mutable clash: (Shortcut * string * Shortcut) option = None
    let pageHost = ContentControl()
    let nav = StackPanel()

    let directory () = App.SettingsDirectory
    let current () = Preferences.Live.Current
    let look () = Palette.forVariant this.ActualThemeVariant
    let brush (c: Color) = SolidColorBrush c

    /// A change: written to the file where there is one, and in force at once.
    let apply (change: Preferences.Preferences -> Preferences.Preferences) =
        let next = change (current ())

        if next <> current () then
            match directory () with
            | null -> ()
            | dir ->
                try
                    Preferences.write dir { Preferences.read dir with
                                                EditorSize = next.EditorSize
                                                AutoClose = next.AutoClose
                                                RunOnSave = next.RunOnSave
                                                ExportFormat = next.ExportFormat
                                                RecentKeep = next.RecentKeep
                                                Keys = next.Keys
                                                Theme = next.Theme }
                with _ ->
                    ()

            Preferences.Live.Set next

    let text (s: string) (colour: Color) = TextBlock(Text = s, Foreground = brush colour, VerticalAlignment = VerticalAlignment.Center)

    let small (s: string) (colour: Color) =
        TextBlock(Text = s, Foreground = brush colour, FontSize = Tokens.Type.small, VerticalAlignment = VerticalAlignment.Center)

    let title (s: string) =
        TextBlock(Text = s, FontSize = Tokens.Type.title, FontWeight = FontWeight.SemiBold, Foreground = brush (look ()).Text)

    /// A page's section: its title, then rows of a label and what it sets.
    let section (name: string) (rows: (string * Control) list) =
        let panel = StackPanel(Spacing = Tokens.Space.s3)
        panel.Children.Add(title name)
        let grid = Grid(ColumnDefinitions = ColumnDefinitions $"136,{Tokens.Space.s4},*")

        rows
        |> List.iteri (fun i (label, control) ->
            grid.RowDefinitions.Add(RowDefinition(GridLength.Auto))
            let l = text label (look ()).Dim
            l.Margin <- Thickness(0.0, Tokens.Space.s1)
            Grid.SetRow(l, i)
            grid.Children.Add l
            control.Margin <- Thickness(0.0, Tokens.Space.s1)
            control.HorizontalAlignment <- HorizontalAlignment.Left
            Grid.SetRow(control, i)
            Grid.SetColumn(control, 2)
            grid.Children.Add control)

        panel.Children.Add grid
        panel

    let check (name: string) (label: string) (note: string) (value: bool) (set: bool -> unit) =
        let content = StackPanel(Orientation = Orientation.Horizontal, Spacing = Tokens.Space.s2)
        content.Children.Add(TextBlock(Text = label))
        if note <> "" then content.Children.Add(small note (look ()).Dim)
        let box = CheckBox(Name = name, Content = content, IsChecked = Nullable value)
        box.IsCheckedChanged.Add(fun _ -> set (box.IsChecked = Nullable true))
        box :> Control

    let rec appearance () : Control =
        let p = current ()
        let row = StackPanel(Orientation = Orientation.Horizontal)

        for theme, label in [ Preferences.FollowSystem, "System"; Preferences.Light, "Light"; Preferences.Dark, "Dark" ] do
            let on = p.Theme = theme
            let look = look ()

            let b =
                Button(
                    Name = $"theme {label.ToLowerInvariant()}",
                    Content = label,
                    CornerRadius = CornerRadius 0.0,
                    Background = (if on then brush look.Roles.AccentSubtle :> IBrush else brush look.Background),
                    Foreground = brush (if on then look.Accent else look.Text),
                    BorderBrush = brush look.Roles.LineStrong,
                    BorderThickness = Thickness(Tokens.border)
                )

            b.Click.Add(fun _ ->
                // The app's own theme switch keeps it in the file too.
                App.Choose theme
                apply (fun p -> { p with Theme = theme })
                show ())

            row.Children.Add b

        section "Appearance" [ "Theme", row ]

    and editor () : Control =
        let p = current ()
        let lo, hi = Preferences.editorSizes
        let size = NumericUpDown(Name = "editor size", Minimum = decimal lo, Maximum = decimal hi, Increment = 1m, Value = Nullable(decimal p.EditorSize), FormatString = "0", Width = 120.0)
        size.ValueChanged.Add(fun e -> if e.NewValue.HasValue then apply (fun p -> { p with EditorSize = float e.NewValue.Value }))
        let sizeRow = StackPanel(Orientation = Orientation.Horizontal, Spacing = Tokens.Space.s2)
        sizeRow.Children.Add size
        sizeRow.Children.Add(text "px, JetBrains Mono" (look ()).Dim)

        section
            "Editor"
            [ "Type size", sizeRow
              "", check "auto close" "Close brackets and quotes as I type" "not < or '" p.AutoClose (fun v -> apply (fun p -> { p with AutoClose = v }))
              "", check "run on save" "Run the script on save" "" p.RunOnSave (fun v -> apply (fun p -> { p with RunOnSave = v })) ]

    and export () : Control =
        let p = current ()
        let formats = ComboBox(Name = "export format", ItemsSource = Preferences.exportFormats, SelectedItem = p.ExportFormat, Width = 120.0)
        formats.SelectionChanged.Add(fun _ -> match formats.SelectedItem with :? string as f -> apply (fun p -> { p with ExportFormat = f }) | _ -> ())
        section "Export" [ "Format", formats ]

    and recent () : Control =
        let p = current ()
        let lo, hi = Preferences.recentKeeps
        let keep = NumericUpDown(Name = "recent keep", Minimum = decimal lo, Maximum = decimal hi, Increment = 1m, Value = Nullable(decimal p.RecentKeep), FormatString = "0", Width = 120.0)
        keep.ValueChanged.Add(fun e -> if e.NewValue.HasValue then apply (fun p -> { p with RecentKeep = int e.NewValue.Value }))
        let row = StackPanel(Orientation = Orientation.Horizontal, Spacing = Tokens.Space.s2)
        row.Children.Add keep
        row.Children.Add(text "files and folders" (look ()).Dim)
        section "Recent files" [ "Keep", row ]

    /// Every command with its keys, by menu; a click on a command's keys records new ones.
    and keys () : Control =
        let p = current ()
        let look = look ()
        let panel = StackPanel(Spacing = Tokens.Space.s2)
        let top = DockPanel()
        let reset = Button(Content = "Reset all", Background = Brushes.Transparent)
        reset.Click.Add(fun _ -> apply (fun p -> { p with Keys = Map.empty }); show ())
        DockPanel.SetDock(reset, Dock.Right)
        top.Children.Add reset
        let search = TextBox(Name = "shortcut filter", PlaceholderText = "Filter by command or key", Text = filter, Margin = Thickness(0.0, 0.0, Tokens.Space.s2, 0.0))
        top.Children.Add search
        panel.Children.Add top
        panel.Children.Add(TextBlock(Text = "Click a shortcut to change it; Escape stops, Backspace takes the key away. Those marked grid act only while a table's grid has the focus.", FontSize = Tokens.Type.small, Foreground = brush look.Dim, TextWrapping = TextWrapping.Wrap))
        let list = StackPanel()
        panel.Children.Add list

        let rec fill () =
            list.Children.Clear()
            let f = filter.Trim()

            let matches (s: Shortcut) =
                f = ""
                || (plain s.Header).Contains(f, StringComparison.OrdinalIgnoreCase)
                || keysOf p s |> List.exists (fun k -> k.Contains(f, StringComparison.OrdinalIgnoreCase) || (shown k).Contains(f, StringComparison.OrdinalIgnoreCase))

            for menu in shortcuts |> List.map (fun s -> s.Menu) |> List.distinct do
                let rows = shortcuts |> List.filter (fun s -> s.Menu = menu && matches s)

                if not rows.IsEmpty then
                    list.Children.Add(TextBlock(Text = plain menu, FontSize = Tokens.Type.small, FontWeight = FontWeight.SemiBold, Foreground = brush look.Dim, Margin = Thickness(Tokens.Space.s2, Tokens.Space.s2, 0.0, Tokens.Space.s1)))

                for s in rows do
                    let row = Grid(ColumnDefinitions = ColumnDefinitions "*,Auto,Auto,180", Height = Tokens.Height.row)
                    let name = text (plain s.Header) look.Text
                    name.Margin <- Thickness(Tokens.Space.s2, 0.0)
                    row.Children.Add name

                    if s.InGrid then
                        let g = small "grid" look.Dim
                        g.Margin <- Thickness(Tokens.Space.s2, 0.0)
                        Grid.SetColumn(g, 1)
                        row.Children.Add g

                    if p.Keys.ContainsKey s.Header then
                        let changed = Button(Content = small "changed · reset" look.Accent, Background = Brushes.Transparent, Padding = Thickness(Tokens.Space.s1, 0.0), MinHeight = 0.0)
                        changed.Click.Add(fun _ -> apply (fun p -> { p with Keys = p.Keys.Remove s.Header }); show ())
                        Grid.SetColumn(changed, 2)
                        row.Children.Add changed

                    let chips = StackPanel(Orientation = Orientation.Horizontal, Spacing = Tokens.Space.s1, HorizontalAlignment = HorizontalAlignment.Right, VerticalAlignment = VerticalAlignment.Center)
                    let isRecording = recording |> Option.exists (fun r -> r.Header = s.Header)

                    let chip (label: string) =
                        Border(
                            Child = TextBlock(Text = label, FontSize = Tokens.Type.small, Foreground = brush look.Text),
                            Padding = Thickness(Tokens.Space.s1, 0.0),
                            BorderBrush = brush (if isRecording then look.Accent else look.Roles.LineStrong),
                            BorderThickness = Thickness(Tokens.border),
                            CornerRadius = CornerRadius Tokens.Radius.sm,
                            Background = brush look.Surface
                        )

                    if isRecording then chips.Children.Add(chip "Press keys…")
                    else
                        match keysOf p s with
                        | [] -> chips.Children.Add(small "none" look.Roles.Faint)
                        | ks -> for k in ks do chips.Children.Add(chip (shown k))

                    let record = Button(Name = $"keys {plain s.Header}", Content = chips, Background = Brushes.Transparent, HorizontalAlignment = HorizontalAlignment.Stretch, HorizontalContentAlignment = HorizontalAlignment.Right, Padding = Thickness(Tokens.Space.s1, 0.0), MinHeight = 0.0)
                    ToolTip.SetTip(record, "Click, then press the new keys")
                    record.Click.Add(fun _ ->
                        recording <- Some s
                        clash <- None
                        fill ())
                    Grid.SetColumn(record, 3)
                    row.Children.Add record
                    list.Children.Add row

                    match clash with
                    | Some(target, k, other) when target.Header = s.Header ->
                        let warning = DockPanel(Margin = Thickness(Tokens.Space.s2, 0.0, 0.0, Tokens.Space.s1))
                        let buttons = StackPanel(Orientation = Orientation.Horizontal, Spacing = Tokens.Space.s2)
                        let cancel = Button(Content = "Cancel", Background = Brushes.Transparent)
                        cancel.Click.Add(fun _ -> clash <- None; fill ())
                        let anyway = Button(Name = "assign anyway", Content = "Assign anyway")

                        anyway.Click.Add(fun _ ->
                            clash <- None
                            // The other command gives the keys up.
                            apply (fun p -> { p with Keys = p.Keys.Add(target.Header, k).Add(other.Header, "") })
                            fill ())

                        buttons.Children.Add cancel
                        buttons.Children.Add anyway
                        DockPanel.SetDock(buttons, Dock.Right)
                        warning.Children.Add buttons
                        let where = if other.InGrid then ", in the grid" else ""
                        warning.Children.Add(small $"{shown k} is already {plain other.Header}{where}." look.Warning)
                        list.Children.Add warning
                    | _ -> ()

        search.TextChanged.Add(fun _ ->
            filter <- search.Text |> Option.ofObj |> Option.defaultValue ""
            fill ())

        fill ()
        panel :> Control

    /// The page shown, drawn again from the preferences in force.
    and show () =
        let body =
            match page with
            | "Editor" -> editor ()
            | "Keyboard shortcuts" -> keys ()
            | "Export" -> export ()
            | "Recent files" -> recent ()
            | _ -> appearance ()

        let panel = StackPanel(Spacing = Tokens.Space.s4)

        match directory () with
        | null -> ()
        | dir when Preferences.unreadable dir ->
            panel.Children.Add(
                TextBlock(
                    Text = "settings.json could not be read, so the defaults are shown; a change here writes them over it.",
                    Foreground = brush (look ()).Warning,
                    TextWrapping = TextWrapping.Wrap
                )
            )
        | _ -> ()

        panel.Children.Add body
        pageHost.Content <- panel
        let look = look ()

        nav.Children.Clear()

        for name in pages do
            let b =
                Button(
                    Name = $"page {name}",
                    Content = name,
                    HorizontalAlignment = HorizontalAlignment.Stretch,
                    HorizontalContentAlignment = HorizontalAlignment.Left,
                    Height = Tokens.Height.row,
                    MinHeight = 0.0,
                    Padding = Thickness(Tokens.Space.s2, 0.0),
                    VerticalContentAlignment = VerticalAlignment.Center,
                    Background = (if name = page then brush look.Roles.AccentSubtle :> IBrush else Brushes.Transparent)
                )

            b.Click.Add(fun _ ->
                page <- name
                recording <- None
                clash <- None
                show ())

            nav.Children.Add b

    /// The whole dialog, in the theme's colours.
    let build () =
        let look = look ()
        this.Background <- brush look.Surface
        let layout = DockPanel()

        let header = DockPanel(Height = Tokens.Height.toolbar, Margin = Thickness(Tokens.Space.s4, 0.0, Tokens.Space.s2, 0.0))
        header.Children.Add(TextBlock(Text = "Preferences", FontSize = Tokens.Type.title, FontWeight = FontWeight.SemiBold, Foreground = brush look.Text, VerticalAlignment = VerticalAlignment.Center))
        let headerBorder = Border(Child = header, BorderBrush = brush look.Line, BorderThickness = Thickness(0.0, 0.0, 0.0, Tokens.border))
        DockPanel.SetDock(headerBorder, Dock.Top)
        layout.Children.Add headerBorder

        let footer = DockPanel(Margin = Thickness(Tokens.Space.s4, Tokens.Space.s3))
        let close = Button(Name = "close", Content = "Close", Background = brush look.Roles.AccentFill, Foreground = brush look.Roles.OnAccent)
        close.Click.Add(fun _ -> this.Close())
        DockPanel.SetDock(close, Dock.Right)
        footer.Children.Add close
        let note = small "Changes apply at once." look.Dim
        note.Margin <- Thickness(0.0, 0.0, Tokens.Space.s3, 0.0)
        DockPanel.SetDock(note, Dock.Right)
        footer.Children.Add note
        let openIt = Button(Content = "Open settings.json", Background = Brushes.Transparent, HorizontalAlignment = HorizontalAlignment.Left, IsEnabled = not (isNull (directory ())))
        openIt.Click.Add(fun _ ->
            match directory () with
            | null -> ()
            | dir ->
                let path = Path.Combine(dir, Preferences.FileName)
                if not (File.Exists path) then Preferences.write dir (Preferences.read dir)
                openFile path
                this.Close())
        footer.Children.Add openIt
        let footerBorder = Border(Child = footer, BorderBrush = brush look.Line, BorderThickness = Thickness(0.0, Tokens.border, 0.0, 0.0))
        DockPanel.SetDock(footerBorder, Dock.Bottom)
        layout.Children.Add footerBorder

        let body = Grid(ColumnDefinitions = ColumnDefinitions "176,*")
        let navBorder = Border(Child = nav, Padding = Thickness Tokens.Space.s2, BorderBrush = brush look.Line, BorderThickness = Thickness(0.0, 0.0, Tokens.border, 0.0))
        body.Children.Add navBorder
        let scroll = ScrollViewer(Content = pageHost, Padding = Thickness Tokens.Space.s5, Background = brush look.Background)
        Grid.SetColumn(scroll, 1)
        body.Children.Add scroll
        layout.Children.Add body
        this.Content <- layout
        show ()

    let watcher =
        match App.SettingsDirectory with
        | null -> None
        | dir when Directory.Exists dir ->
            let w = new FileSystemWatcher(dir, Preferences.FileName, EnableRaisingEvents = true)
            Some w
        | _ -> None

    /// The file changed by hand: what it holds now put in force and shown.
    let reread () =
        match directory () with
        | null -> ()
        | dir ->
            let read = Preferences.read dir
            let unreadable = Preferences.unreadable dir

            if read <> current () || unreadable then
                Preferences.Live.Set read
                if not (isNull App.Current) && read.Theme <> App.Theme then App.Choose read.Theme
                show ()

    do
        this.Title <- "Preferences"
        this.Width <- 720.0
        this.Height <- 536.0
        this.CanResize <- true
        this.WindowStartupLocation <- WindowStartupLocation.CenterOwner
        build ()
        this.ActualThemeVariantChanged.Add(fun _ -> build ())

        match watcher with
        | Some w ->
            let changed = FileSystemEventHandler(fun _ _ -> Dispatcher.UIThread.Post(fun () -> reread ()))
            w.Changed.AddHandler changed
            w.Created.AddHandler changed
            w.Renamed.AddHandler(RenamedEventHandler(fun _ _ -> Dispatcher.UIThread.Post(fun () -> reread ())))
            this.Closed.Add(fun _ -> w.Dispose())
        | None -> ()

        // Keys pressed while a command's keys are being recorded are its new keys.
        this.AddHandler(
            InputElement.KeyDownEvent,
            EventHandler<KeyEventArgs>(fun _ e ->
                match recording with
                | Some s ->
                    let modifierOnly =
                        match e.Key with
                        | Key.LeftCtrl | Key.RightCtrl | Key.LeftShift | Key.RightShift | Key.LeftAlt | Key.RightAlt | Key.LWin | Key.RWin -> true
                        | _ -> false

                    if not modifierOnly then
                        e.Handled <- true
                        recording <- None

                        match e.Key with
                        | Key.Escape when e.KeyModifiers = KeyModifiers.None -> ()
                        | Key.Back
                        | Key.Delete when e.KeyModifiers = KeyModifiers.None -> apply (fun p -> { p with Keys = p.Keys.Add(s.Header, "") })
                        | key ->
                            let k = string (KeyGesture(key, e.KeyModifiers))
                            let p = current ()

                            let other =
                                shortcuts
                                |> List.tryFind (fun o -> o.Header <> s.Header && keysOf p o |> List.exists (fun ok -> try KeyGesture.Parse ok = KeyGesture.Parse k with _ -> false))

                            match other with
                            | Some o -> clash <- Some(s, k, o)
                            | None when s.Keys |> List.tryHead = Some k -> apply (fun p -> { p with Keys = p.Keys.Remove s.Header })
                            | None -> apply (fun p -> { p with Keys = p.Keys.Add(s.Header, k) })

                        show ()
                | None -> ()),
            RoutingStrategies.Tunnel
        )
        |> ignore

    /// The page shown, by its name.
    member _.Page
        with get () = page
        and set (name: string) =
            page <- name
            show ()

    /// Starts recording a command's keys, as a click on them does.
    member _.Record(header: string) =
        recording <- shortcuts |> List.tryFind (fun s -> s.Header = header)
        clash <- None
        show ()

    /// The clash the last keys made, if they did: the command, the keys, and the command that has them.
    member _.Clash = clash |> Option.map (fun (s, k, o) -> s.Header, k, o.Header)

    /// Assigns the clashing keys anyway, as its button does.
    member _.AssignAnyway() =
        match clash with
        | Some(target, k, other) ->
            clash <- None
            apply (fun p -> { p with Keys = p.Keys.Add(target.Header, k).Add(other.Header, "") })
            show ()
        | None -> ()
