/// Gate 16 (c): the Preferences dialog over `settings.json`. Each preference is written to the file
/// and takes effect without a restart; a hand edit of the file shows in the open dialog; a file
/// that cannot be read falls back to the defaults, with a note.
[<Xunit.Collection("workers")>]
module Tambour.Tests.PreferencesTests

open System
open System.Diagnostics
open System.IO
open System.Linq
open System.Threading
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Headless.XUnit
open Avalonia.Input
open Avalonia.Threading
open Avalonia.VisualTree
open Tambour
open Xunit

let private pumpUntil (seconds: float) (what: string) (ready: unit -> bool) =
    // A shared runner is slower than a desk: three times the wait there.
    let seconds = if Environment.GetEnvironmentVariable "GITHUB_ACTIONS" = "true" then 3.0 * seconds else seconds
    let clock = Stopwatch.StartNew()

    while not (ready ()) do
        if clock.Elapsed.TotalSeconds > seconds then
            failwith $"{what} did not happen within {seconds} s"

        Dispatcher.UIThread.RunJobs()
        Thread.Sleep 10

let private shortcuts: PreferencesView.Shortcut list =
    [ for c in Shell.commands ->
          { Menu = c.Menu
            Header = c.Header
            Keys = Option.toList c.Gesture @ c.Also |> List.map string
            InGrid = c.InGrid
            InEditor = c.InEditor } ]

[<AvaloniaFact>]
let ``each preference is written to settings.json, takes effect at once, and follows the file`` () =
    let before = App.SettingsDirectory
    let dir = (Directory.CreateTempSubdirectory "tambour-preferences").FullName
    App.SettingsDirectory <- dir
    Preferences.Live.Set Preferences.defaults

    try
        let window = new Shell.MainWindow("let x = 1\n")
        window.Show()
        pumpUntil 10.0 "the editor" (fun () -> window.Active.GetVisualDescendants().OfType<ScriptEditor>().Any())
        let editor = window.Active.GetVisualDescendants().OfType<ScriptEditor>().Single()
        let dialog = PreferencesView.PreferencesWindow(shortcuts, ignore, None)
        dialog.Show()
        let named (name: string) = dialog.GetVisualDescendants().OfType<Control>() |> Seq.find (fun c -> c.Name = name)
        let stored () = Preferences.read dir

        // The theme: clicked as by hand, which redraws the dialog in the new colours.
        let themeBefore = App.Theme
        dialog.Page <- "Appearance"

        for name, theme in [ "theme dark", Preferences.Dark; "theme light", Preferences.Light; "theme system", Preferences.FollowSystem ] do
            (named name :?> Button).RaiseEvent(Avalonia.Interactivity.RoutedEventArgs Button.ClickEvent)
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(theme, (stored ()).Theme)
            Assert.Equal(theme, App.Theme)
            Assert.True(dialog.GetVisualDescendants().OfType<Button>().Any(fun b -> b.Name = "theme dark"), $"the dialog whole after {name}")

        App.Choose themeBefore

        // The editor's size: in the file, and on the open editor at once.
        dialog.Page <- "Editor"
        (named "editor size" :?> NumericUpDown).Value <- Nullable 18m
        Dispatcher.UIThread.RunJobs()
        Assert.Equal(18.0, (stored ()).EditorSize)
        Assert.Equal(18.0, editor.Editor.FontSize)

        // Closers off: an opener typed is only itself.
        (named "auto close" :?> CheckBox).IsChecked <- Nullable false
        Dispatcher.UIThread.RunJobs()
        Assert.False((stored ()).AutoClose)
        editor.Editor.CaretOffset <- editor.Text.Length
        editor.Editor.TextArea.PerformTextInput "("
        Assert.EndsWith("\n(", editor.Text)

        // Ligatures off: the editor in the cut of its font without them, and back.
        Assert.Contains("JetBrains Mono,", string editor.Editor.FontFamily)
        (named "ligatures" :?> CheckBox).IsChecked <- Nullable false
        Dispatcher.UIThread.RunJobs()
        Assert.False((stored ()).Ligatures)
        Assert.Contains("JetBrains Mono NL", string editor.Editor.FontFamily)
        // The face itself, from the app's own fonts, not one standing in for it.
        let face = Avalonia.Media.Typeface(Palette.codeWithoutLigatures).GlyphTypeface
        Assert.Equal("JetBrains Mono NL", face.FamilyName)
        (named "ligatures" :?> CheckBox).IsChecked <- Nullable true
        Dispatcher.UIThread.RunJobs()
        Assert.True((stored ()).Ligatures)
        Assert.DoesNotContain("JetBrains Mono NL", string editor.Editor.FontFamily)

        (named "run on save" :?> CheckBox).IsChecked <- Nullable false
        Dispatcher.UIThread.RunJobs()
        Assert.False((stored ()).RunOnSave)

        dialog.Page <- "Export"
        (named "export format" :?> ComboBox).SelectedItem <- "svg"
        Dispatcher.UIThread.RunJobs()
        Assert.Equal("svg", (stored ()).ExportFormat)

        dialog.Page <- "Recent files"
        (named "recent keep" :?> NumericUpDown).Value <- Nullable 3m
        Dispatcher.UIThread.RunJobs()
        Assert.Equal(3, (stored ()).RecentKeep)
        Assert.Equal(3, Preferences.Live.Current.RecentKeep)

        // A shortcut recorded: in the file, taken by its keys, and shown in the menu.
        dialog.Page <- "Keyboard shortcuts"
        dialog.Record "_Run"
        dialog.KeyPress(Key.R, RawInputModifiers.Control ||| RawInputModifiers.Alt, PhysicalKey.R, null)
        Dispatcher.UIThread.RunJobs()
        Assert.Equal(Some "Ctrl+Alt+R", (stored ()).Keys.TryFind "_Run")
        Assert.Equal(Some "_Run", Shell.commandFor Key.R (KeyModifiers.Control ||| KeyModifiers.Alt) |> Option.map (fun c -> c.Header))
        Assert.Equal(None, Shell.commandFor Key.F5 KeyModifiers.None |> Option.map (fun c -> c.Header))
        let menu = window.Active.GetVisualDescendants().OfType<Menu>().Single()
        let rec leaves (items: obj seq) = [ for i in items do match i with :? MenuItem as m -> yield m; yield! leaves (m.Items |> Seq.cast) | _ -> () ]
        pumpUntil 5.0 "the menu's shortcut" (fun () -> leaves (menu.Items |> Seq.cast) |> List.exists (fun m -> string m.Header = "_Run" && string m.InputGesture = "Ctrl+Alt+R"))

        // Keys another command has: named first, then given over only when asked.
        dialog.Record "_Save"
        dialog.KeyPress(Key.R, RawInputModifiers.Control ||| RawInputModifiers.Alt, PhysicalKey.R, null)
        Dispatcher.UIThread.RunJobs()
        Assert.Equal(Some("_Save", "Ctrl+Alt+R", "_Run"), dialog.Clash)
        Assert.Equal(None, (stored ()).Keys.TryFind "_Save")
        dialog.AssignAnyway()
        Assert.Equal(Some "Ctrl+Alt+R", (stored ()).Keys.TryFind "_Save")
        Assert.Equal(Some "", (stored ()).Keys.TryFind "_Run")

        // An edit of the file by hand: in force, and in the open dialog.
        dialog.Page <- "Editor"
        let path = Path.Combine(dir, Preferences.FileName)
        File.WriteAllText(path, File.ReadAllText(path).Replace("\"editorSize\": 18", "\"editorSize\": 20"))
        pumpUntil 10.0 "the hand edit taken" (fun () -> Preferences.Live.Current.EditorSize = 20.0)
        Assert.Equal(20.0, editor.Editor.FontSize)
        pumpUntil 5.0 "the dialog showing it" (fun () -> (named "editor size" :?> NumericUpDown).Value = Nullable 20m)

        // A file that cannot be read: the defaults, with a note.
        File.WriteAllText(path, "{ not json")
        pumpUntil 10.0 "the note" (fun () ->
            dialog.GetVisualDescendants().OfType<TextBlock>().Any(fun t -> not (isNull t.Text) && t.Text.Contains "could not be read"))
        Assert.Equal(Preferences.defaults, Preferences.read dir)
        Assert.Equal(Preferences.defaults.EditorSize, editor.Editor.FontSize)

        match Environment.GetEnvironmentVariable "TAMBOUR_CHECK_OUT" with
        | null
        | "" -> ()
        | out ->
            dialog.Page <- "Keyboard shortcuts"
            use frame = dialog.CaptureRenderedFrame()
            frame.Save(Path.Combine(out, "preferences.png"), Avalonia.Media.Imaging.PngBitmapEncoderOptions.Default)

        dialog.Close()
        window.Ask <- fun _ -> Threading.Tasks.Task.FromResult Discard
        window.Close()
    finally
        Preferences.Live.Set Preferences.defaults
        App.SettingsDirectory <- before

/// The Project page: the shown tab's own settings, from its project file and written back to it,
/// the export's resolution and its fonts; a tab without a project says how to give it one.
[<AvaloniaFact>]
let ``the project page edits the shown tab's resolution and fonts in its project file`` () =
    let dir = (Directory.CreateTempSubdirectory "tambour-project-page").FullName
    Project.makeWorkspace dir
    File.WriteAllText(Path.Combine(dir, "figure.fsx"), "let n = 1\n")
    let font = Path.Combine(dir, "Some.ttf")
    File.WriteAllBytes(font, [||])

    let window = new Shell.MainWindow("", Some dir)
    window.Show()
    pumpUntil 10.0 "the project open" (fun () -> window.Active.Project = Some dir)
    window.Post Shell.OpenPreferences
    pumpUntil 5.0 "the preferences" (fun () -> window.OwnedWindows.Count = 1)
    let dialog = window.OwnedWindows.[0] :?> PreferencesView.PreferencesWindow
    let named (name: string) = dialog.GetVisualDescendants().OfType<Control>() |> Seq.find (fun c -> c.Name = name)
    let stored () = (Project.read dir).Value.Skia

    dialog.Page <- "Project"
    let dpi = named "project dpi" :?> NumericUpDown
    Assert.Equal(Nullable 96m, dpi.Value)
    dpi.Value <- Nullable 300m
    Dispatcher.UIThread.RunJobs()
    pumpUntil 5.0 "the resolution written" (fun () -> abs (stored().Dpi - 300.0 / 25.4) < 1e-9)

    dialog.AddFont font
    Dispatcher.UIThread.RunJobs()
    pumpUntil 5.0 "the font written" (fun () -> stored().FontFiles = [| font |])
    Assert.True(dialog.GetVisualDescendants().OfType<TextBlock>().Any(fun t -> t.Text = font))
    dialog.Close()

    // A tab without a project: no settings to edit, and what would give it some.
    window.Post Shell.NewScript
    pumpUntil 5.0 "a new tab" (fun () -> window.Workspaces.Length = 2)
    window.Post Shell.OpenPreferences
    pumpUntil 5.0 "the preferences again" (fun () -> window.OwnedWindows.Count = 1)
    let again = window.OwnedWindows.[0] :?> PreferencesView.PreferencesWindow
    again.Page <- "Project"
    Dispatcher.UIThread.RunJobs()
    Assert.False(again.GetVisualDescendants().OfType<Control>().Any(fun c -> c.Name = "project dpi"))
    Assert.True(again.GetVisualDescendants().OfType<TextBlock>().Any(fun t -> not (isNull t.Text) && t.Text.Contains "no project"))
    again.Close()

    window.Ask <- fun _ -> Threading.Tasks.Task.FromResult Discard
    window.Close()
