namespace Tambour.Tests

open Avalonia
open Avalonia.Headless

type TestApp =
    static member BuildAvaloniaApp() =
        // A window a test closes with unsaved work closes without it: no one is there to answer
        // the dialog. A test of the question answers it on its window.
        Tambour.App.AnswerUnsaved <- Some Tambour.Discard

        AppBuilder
            .Configure<Tambour.App>()
            .UseSkia()
            .UseHeadless(AvaloniaHeadlessPlatformOptions(UseHeadlessDrawing = false))

[<assembly: AvaloniaTestApplication(typeof<TestApp>)>]
do ()
