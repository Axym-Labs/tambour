namespace Tambour

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Themes.Fluent

type App() =
    inherit Application()

    /// The window shown when the app is started without a role; set before the lifetime starts.
    static member val MainWindow: unit -> Window = (fun () -> Window()) with get, set

    override this.Initialize() =
        this.Styles.Add(FluentTheme())
        // AvaloniaEdit's control template.
        this.Styles.Add(Avalonia.Markup.Xaml.Styling.StyleInclude(Uri "avares://Tambour", Source = Uri "avares://AvaloniaEdit/Themes/Fluent/AvaloniaEdit.xaml"))

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktop -> desktop.MainWindow <- App.MainWindow()
        | _ -> ()
        base.OnFrameworkInitializationCompleted()

module App =

    let builder () = AppBuilder.Configure<App>().UsePlatformDetect()
