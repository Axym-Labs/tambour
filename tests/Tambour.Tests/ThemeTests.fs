/// The two themes: every text colour readable on what it is drawn on, the theme kept as the user
/// chose it, and the window drawn in it.
[<Xunit.Collection("workers")>]
module Tambour.Tests.ThemeTests

open System
open System.IO
open System.Text.Json
open Avalonia
open Avalonia.Controls
open Avalonia.Headless.XUnit
open Avalonia.Media
open Avalonia.Styling
open Avalonia.Threading
open Tambour
open Xunit

/// WCAG's contrast ratio of two opaque colours.
let contrast (a: Color) (b: Color) =
    let channel (c: byte) =
        let v = float c / 255.0
        if v <= 0.03928 then v / 12.92 else ((v + 0.055) / 1.055) ** 2.4

    let luminance (c: Color) = 0.2126 * channel c.R + 0.7152 * channel c.G + 0.0722 * channel c.B
    let hi, lo = max (luminance a) (luminance b), min (luminance a) (luminance b)
    (hi + 0.05) / (lo + 0.05)

let looks = [ "light", Palette.light; "dark", Palette.dark ]

/// Every foreground a grammar theme can give a token.
let grammarColours (name: string) =
    use stream = typeof<Palette.Look>.Assembly.GetManifestResourceStream $"Tambour.Themes.{name}.json"
    use document = JsonDocument.Parse stream

    [ for rule in document.RootElement.GetProperty("tokenColors").EnumerateArray() do
          match rule.GetProperty("settings").TryGetProperty "foreground" with
          | true, fg -> Color.Parse(fg.GetString())
          | _ -> () ]
    |> List.distinct

/// Gate 14 (a): every text colour of both themes at least 4.5:1 (WCAG AA) on its background.
[<Fact>]
let ``every text colour is at least 4.5 to 1 on what it is drawn on`` () =
    let low =
        [ for name, look in looks do
              for what, fg, bg in Palette.pairs look do
                  let r = contrast fg bg
                  if r < 4.5 then $"{name}: {what} {fg} on {bg}, {r:F2}"

              for fg in grammarColours name do
                  let r = contrast fg look.Background
                  if r < 4.5 then $"{name}: the grammar theme's {fg}, {r:F2}" ]

    Assert.True(low.IsEmpty, String.Join("\n", low))

/// The colour pairs reach every kind a script's names are drawn in.
[<Fact>]
let ``the pairs cover every kind`` () =
    for _, look in looks do
        let named = Palette.pairs look |> List.map (fun (what, _, _) -> what) |> Set.ofList

        for kind in look.Kinds.Keys do
            Assert.Contains($"kind {kind}", named)

/// Both grammar themes load as the editor loads them.
[<Fact>]
let ``both grammar themes load`` () =
    for _, look in looks do
        let theme = Palette.grammarTheme look
        Assert.NotEmpty(theme.GetTokenColors())

[<Fact>]
let ``the theme is kept as chosen, and a new user's follows the system`` () =
    let dir = Path.Combine(Path.GetTempPath(), $"tambour-prefs-{Guid.NewGuid():N}")

    try
        Assert.Equal(Preferences.FollowSystem, (Preferences.read dir).Theme)

        for theme in [ Preferences.Dark; Preferences.Light; Preferences.FollowSystem ] do
            Preferences.write dir { Theme = theme }
            Assert.Equal(theme, (Preferences.read dir).Theme)

        File.WriteAllText(Path.Combine(dir, Preferences.FileName), "{ not json")
        Assert.Equal(Preferences.defaults, Preferences.read dir)
    finally
        if Directory.Exists dir then
            Directory.Delete(dir, true)

/// View → Theme draws the whole app in the theme chosen: Fluent's controls in the look's own
/// colours, and the shell's model in its look.
[<AvaloniaFact>]
let ``choosing a theme draws the window in it`` () =
    let window = Window(Width = 300.0, Height = 200.0)
    window.Show()

    try
        for theme, look in [ Preferences.Dark, Palette.dark; Preferences.Light, Palette.light ] do
            App.Choose theme
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(look, Palette.forVariant window.ActualThemeVariant)

            match window.TryFindResource("SystemRegionColor", window.ActualThemeVariant) with
            | true, (:? Color as c) -> Assert.Equal(look.Surface, c)
            | found, value -> failwith $"SystemRegionColor: {found} {value}"
    finally
        App.Choose Preferences.FollowSystem
        window.Close()
