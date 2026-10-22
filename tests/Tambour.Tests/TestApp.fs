namespace Tambour.Tests

open Avalonia
open Avalonia.Headless

type TestApp =
    static member BuildAvaloniaApp() =
        AppBuilder
            .Configure<Tambour.App>()
            .UseSkia()
            .UseHeadless(AvaloniaHeadlessPlatformOptions(UseHeadlessDrawing = false))

[<assembly: AvaloniaTestApplication(typeof<TestApp>)>]
do ()
