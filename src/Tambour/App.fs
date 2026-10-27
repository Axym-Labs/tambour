namespace Tambour

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Styling
open Avalonia.Themes.Fluent

type App() =
    inherit Application()

    /// The window shown when the app is started without a role; set before the lifetime starts.
    static member val MainWindow: unit -> Window = (fun () -> Window()) with get, set

    /// The theme the app starts in, from the user's settings.
    static member val Theme = Preferences.FollowSystem with get, set

    /// Where a theme chosen in the window is kept, or null to keep it for this run only (a test's).
    static member val SettingsDirectory: string = null with get, set

    /// The variant a theme asks for: the system's own for `FollowSystem`.
    static member VariantOf(theme: Preferences.Theme) =
        match theme with
        | Preferences.Light -> ThemeVariant.Light
        | Preferences.Dark -> ThemeVariant.Dark
        | Preferences.FollowSystem -> ThemeVariant.Default

    /// Draws the whole app in a theme from now on, and keeps it in the user's settings.
    static member Choose(theme: Preferences.Theme) =
        App.Theme <- theme

        match Application.Current with
        | null -> ()
        | app -> app.RequestedThemeVariant <- App.VariantOf theme

        match App.SettingsDirectory with
        | null -> ()
        | dir ->
            try
                Preferences.write dir { Preferences.read dir with Theme = theme }
            with _ ->
                ()

    override this.Initialize() =
        // Fluent's controls in Tambour's colours, the same on every OS: without these Windows
        // draws them in its own accent.
        let fluent = FluentTheme()
        fluent.Palettes.[ThemeVariant.Light] <- Palette.fluent Palette.light
        fluent.Palettes.[ThemeVariant.Dark] <- Palette.fluent Palette.dark
        this.Styles.Add fluent
        // AvaloniaEdit's control template.
        this.Styles.Add(Avalonia.Markup.Xaml.Styling.StyleInclude(Uri "avares://Tambour", Source = Uri "avares://AvaloniaEdit/Themes/Fluent/AvaloniaEdit.xaml"))
        this.RequestedThemeVariant <- App.VariantOf App.Theme

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktop -> desktop.MainWindow <- App.MainWindow()
        | _ -> ()
        base.OnFrameworkInitializationCompleted()

module App =

    /// Popups (tooltips, the completion list, menus) drawn inside the window on Linux. As X11
    /// windows of their own they are placed by whatever manages windows, and under XWayland a
    /// compositor may move them: Hyprland showed a problem's tooltip at the screen's centre.
    let builder () =
        AppBuilder.Configure<App>().UsePlatformDetect().With(X11PlatformOptions(OverlayPopups = true))
