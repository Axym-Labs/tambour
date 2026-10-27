/// What this user has chosen for every project: `settings.json` in their settings directory. A
/// project's own choices are in its `project.json`.
module Tambour.Preferences

open System
open System.IO
open System.Text.Json

[<Literal>]
let FileName = "settings.json"

/// Which of the two themes the window draws in.
type Theme =
    | FollowSystem
    | Light
    | Dark

let themeName =
    function
    | FollowSystem -> "system"
    | Light -> "light"
    | Dark -> "dark"

let private themeOf =
    function
    | "light" -> Light
    | "dark" -> Dark
    | _ -> FollowSystem

type Preferences = { Theme: Theme }

/// A new user's: the theme follows the system's.
let defaults = { Theme = FollowSystem }

/// The preferences in a directory; the defaults where there is no file or it cannot be read.
let read (directory: string) : Preferences =
    try
        use document = JsonDocument.Parse(File.ReadAllText(Path.Combine(directory, FileName)))

        match document.RootElement.TryGetProperty "theme" with
        | true, theme when theme.ValueKind = JsonValueKind.String -> { Theme = themeOf (theme.GetString()) }
        | _ -> defaults
    with _ ->
        defaults

let write (directory: string) (preferences: Preferences) =
    Directory.CreateDirectory directory |> ignore
    let stored = dict [ "theme", themeName preferences.Theme ]
    File.WriteAllText(Path.Combine(directory, FileName), JsonSerializer.Serialize(stored, JsonSerializerOptions(WriteIndented = true)))
