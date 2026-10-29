namespace Tambour

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Styling
open Avalonia.Themes.Fluent

/// What to do with unsaved work before it would be dropped.
type Unsaved =
    | SaveIt
    | Discard
    | Keep

type App() =
    inherit Application()

    /// An answer to the question before unsaved work is dropped, given in place of asking: a test
    /// run's, so that a window it closes with unsaved work closes. None asks.
    static member val AnswerUnsaved: Unsaved option = None with get, set

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
        // Fluent's sizes from the design tokens: its controls at the UI's type size and the
        // tokens' heights and radii rather than its own larger ones.
        let inside height = (height - Tokens.Type.uiLine - 2.0 * Tokens.border) / 2.0
        let sizes: (string * obj) list =
            [ "ControlContentThemeFontSize", box Tokens.Type.ui
              "ToolTipContentThemeFontSize", box Tokens.Type.ui
              "TabItemHeaderFontSize", box Tokens.Type.title
              "TabItemMinHeight", box Tokens.Height.paneHeader
              "ControlCornerRadius", box (CornerRadius Tokens.Radius.sm)
              "OverlayCornerRadius", box (CornerRadius Tokens.Radius.md)
              "TextControlThemeMinHeight", box Tokens.Height.control
              "ComboBoxMinHeight", box Tokens.Height.control
              "ButtonPadding", box (Thickness(Tokens.Space.s3, inside Tokens.Height.control))
              "TextControlThemePadding", box (Thickness(Tokens.Space.s2, inside Tokens.Height.control))
              "ComboBoxPadding", box (Thickness(Tokens.Space.s2, inside Tokens.Height.control, 0.0, inside Tokens.Height.control))
              "MenuBarItemPadding", box (Thickness(Tokens.Space.s2, 0.0))
              "MenuFlyoutItemThemePadding", box (Thickness(Tokens.Space.s3, (Tokens.Height.row - Tokens.Type.uiLine) / 2.0)) ]
        for key, value in sizes do
            this.Resources.[key] <- value
        // AvaloniaEdit's control template.
        this.Styles.Add(Avalonia.Markup.Xaml.Styling.StyleInclude(Uri "avares://Tambour", Source = Uri "avares://AvaloniaEdit/Themes/Fluent/AvaloniaEdit.xaml"))
        this.RequestedThemeVariant <- App.VariantOf App.Theme

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktop -> desktop.MainWindow <- App.MainWindow()
        | _ -> ()
        base.OnFrameworkInitializationCompleted()

module App =

    /// Inter as the one UI font on every OS, carried in the app. Popups (tooltips, the completion
    /// list, menus) drawn inside the window on Linux: as X11 windows of their own they are placed
    /// by whatever manages windows, and under XWayland a compositor may move them (Hyprland showed
    /// a problem's tooltip at the screen's centre).
    let builder () =
        AppBuilder.Configure<App>().UsePlatformDetect().WithInterFont().With(X11PlatformOptions(OverlayPopups = true))
