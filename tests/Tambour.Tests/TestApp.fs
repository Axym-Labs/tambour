namespace Tambour.Tests

open System
open System.IO
open Avalonia
open Avalonia.Headless

/// Where the tests unpack bundles: a directory of this run's own, not the user's cache.
module TestPaths =
    let useScratchCache () =
        if isNull Tambour.Bundle.Root then
            Tambour.Bundle.Root <- Path.Combine(Path.GetTempPath(), $"tambour-tests-{Environment.ProcessId}", "open")

type TestApp =
    static member BuildAvaloniaApp() =
        // A window a test closes with unsaved work closes without it: no one is there to answer
        // the dialog. A test of the question answers it on its window.
        Tambour.App.AnswerUnsaved <- Some Tambour.Discard
        TestPaths.useScratchCache ()

        AppBuilder
            .Configure<Tambour.App>()
            .UseSkia()
            .UseHeadless(AvaloniaHeadlessPlatformOptions(UseHeadlessDrawing = false))

[<assembly: AvaloniaTestApplication(typeof<TestApp>)>]
do ()
