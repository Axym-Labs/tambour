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

type Preferences =
    { Theme: Theme
      /// The scripts and projects opened, most recent first.
      Recent: string list }

/// A new user's: the theme follows the system's, and nothing opened yet.
let defaults = { Theme = FollowSystem; Recent = [] }

/// How many opened files the recent list keeps.
[<Literal>]
let RecentLength = 10

/// The recent list with a path opened: at its head, once, the oldest past the length dropped.
let opened (path: string) (recent: string list) =
    path :: (recent |> List.filter (fun p -> p <> path)) |> List.truncate RecentLength

/// The preferences in a directory; the defaults where there is no file or it cannot be read.
let read (directory: string) : Preferences =
    try
        use document = JsonDocument.Parse(File.ReadAllText(Path.Combine(directory, FileName)))

        let root = document.RootElement

        let theme =
            match root.TryGetProperty "theme" with
            | true, theme when theme.ValueKind = JsonValueKind.String -> themeOf (theme.GetString())
            | _ -> FollowSystem

        let recent =
            match root.TryGetProperty "recent" with
            | true, list when list.ValueKind = JsonValueKind.Array ->
                [ for p in list.EnumerateArray() do
                      if p.ValueKind = JsonValueKind.String then p.GetString() ]
            | _ -> []

        { Theme = theme; Recent = recent }
    with _ ->
        defaults

let write (directory: string) (preferences: Preferences) =
    Directory.CreateDirectory directory |> ignore
    let stored = dict [ "theme", box (themeName preferences.Theme); "recent", box (Array.ofList preferences.Recent) ]
    File.WriteAllText(Path.Combine(directory, FileName), JsonSerializer.Serialize(stored, JsonSerializerOptions(WriteIndented = true)))
