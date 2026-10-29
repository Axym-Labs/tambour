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

/// Where the window was and how large, in its screen's pixels and its own units, and whether it
/// was maximized (its size then the one it returns to).
type Placement =
    { X: int
      Y: int
      Width: float
      Height: float
      Maximized: bool }

type Preferences =
    { Theme: Theme
      /// The scripts and projects opened, most recent first.
      Recent: string list
      /// The scripts open in the window's tabs when it was last changed, in their order, and the
      /// one shown, opened again at the next start.
      Tabs: string list
      Shown: int
      /// The window as it was last closed; none before the first close.
      Window: Placement option
      /// Which of the window's panes are open: the file list, the panel under the script, the
      /// figure's actions.
      FilesOpen: bool
      BottomOpen: bool
      ActionsOpen: bool
      /// The editor's type size, in px.
      EditorSize: float
      /// Whether typing an opener puts in its closer.
      AutoClose: bool
      /// Whether code is drawn with its font's ligatures (`->` an arrow, `|>` a triangle).
      Ligatures: bool
      /// Whether saving the script runs it.
      RunOnSave: bool
      /// The format Export writes unless another is chosen.
      ExportFormat: string
      /// How many opened files the recent list keeps.
      RecentKeep: int
      /// Shortcuts changed from the app's own, by the command's name; an empty one takes the key away.
      Keys: Map<string, string> }

/// A new user's: the theme follows the system's, and nothing opened yet.
let defaults =
    { Theme = FollowSystem
      Recent = []
      Tabs = []
      Shown = 0
      Window = None
      FilesOpen = true
      BottomOpen = true
      ActionsOpen = true
      EditorSize = 14.0
      AutoClose = true
      Ligatures = true
      RunOnSave = true
      ExportFormat = "png"
      RecentKeep = 10
      Keys = Map.empty }

/// The bounds a preference read from the file is held to.
let editorSizes = (9.0, 32.0)
let recentKeeps = (1, 50)
let exportFormats = [ "png"; "pdf"; "svg" ]

/// The recent list with a path opened: at its head, once, the oldest past the length kept dropped.
let opened (keep: int) (path: string) (recent: string list) =
    path :: (recent |> List.filter (fun p -> p <> path)) |> List.truncate keep

/// The preferences in a directory; the defaults where there is no file or it cannot be read.
let read (directory: string) : Preferences =
    try
        use document = JsonDocument.Parse(File.ReadAllText(Path.Combine(directory, FileName)))

        let root = document.RootElement

        let theme =
            match root.TryGetProperty "theme" with
            | true, theme when theme.ValueKind = JsonValueKind.String -> themeOf (theme.GetString())
            | _ -> FollowSystem

        let paths (name: string) =
            match root.TryGetProperty name with
            | true, list when list.ValueKind = JsonValueKind.Array ->
                [ for p in list.EnumerateArray() do
                      if p.ValueKind = JsonValueKind.String then p.GetString() ]
            | _ -> []

        let shown =
            match root.TryGetProperty "shown" with
            | true, n when n.ValueKind = JsonValueKind.Number -> n.GetInt32()
            | _ -> 0

        let flag (name: string) =
            match root.TryGetProperty name with
            | true, b when b.ValueKind = JsonValueKind.False -> false
            | _ -> true

        let window =
            match root.TryGetProperty "window" with
            | true, w when w.ValueKind = JsonValueKind.Object ->
                try
                    Some
                        { X = w.GetProperty("x").GetInt32()
                          Y = w.GetProperty("y").GetInt32()
                          Width = w.GetProperty("width").GetDouble()
                          Height = w.GetProperty("height").GetDouble()
                          Maximized = w.GetProperty("maximized").GetBoolean() }
                with _ ->
                    None
            | _ -> None

        let number (name: string) (fallback: float) (lo: float, hi: float) =
            match root.TryGetProperty name with
            | true, n when n.ValueKind = JsonValueKind.Number -> Math.Clamp(n.GetDouble(), lo, hi)
            | _ -> fallback

        let keys =
            match root.TryGetProperty "keys" with
            | true, k when k.ValueKind = JsonValueKind.Object ->
                Map.ofList [ for p in k.EnumerateObject() do if p.Value.ValueKind = JsonValueKind.String then p.Name, p.Value.GetString() ]
            | _ -> Map.empty

        let format =
            match root.TryGetProperty "exportFormat" with
            | true, f when f.ValueKind = JsonValueKind.String && List.contains (f.GetString()) exportFormats -> f.GetString()
            | _ -> defaults.ExportFormat

        { Theme = theme
          Recent = paths "recent"
          Tabs = paths "tabs"
          Shown = shown
          Window = window
          FilesOpen = flag "files"
          BottomOpen = flag "bottom"
          ActionsOpen = flag "actions"
          EditorSize = number "editorSize" defaults.EditorSize editorSizes
          AutoClose = flag "autoClose"
          Ligatures = flag "ligatures"
          RunOnSave = flag "runOnSave"
          ExportFormat = format
          RecentKeep = int (number "recentKeep" (float defaults.RecentKeep) (float (fst recentKeeps), float (snd recentKeeps)))
          Keys = keys }
    with _ ->
        defaults

/// Whether the settings file is there but cannot be read, so that the defaults stand in for it.
let unreadable (directory: string) =
    let path = Path.Combine(directory, FileName)

    File.Exists path
    && (try
            use _ = JsonDocument.Parse(File.ReadAllText path)
            false
        with _ ->
            true)

let write (directory: string) (preferences: Preferences) =
    Directory.CreateDirectory directory |> ignore
    let stored =
        dict
            [ "theme", box (themeName preferences.Theme)
              "recent", box (Array.ofList preferences.Recent)
              "tabs", box (Array.ofList preferences.Tabs)
              "shown", box preferences.Shown
              "files", box preferences.FilesOpen
              "bottom", box preferences.BottomOpen
              "actions", box preferences.ActionsOpen
              "editorSize", box preferences.EditorSize
              "autoClose", box preferences.AutoClose
              "ligatures", box preferences.Ligatures
              "runOnSave", box preferences.RunOnSave
              "exportFormat", box preferences.ExportFormat
              "recentKeep", box preferences.RecentKeep
              "keys", box (dict [ for KeyValue(k, v) in preferences.Keys -> k, v ])
              match preferences.Window with
              | Some w -> "window", box (dict [ "x", box w.X; "y", box w.Y; "width", box w.Width; "height", box w.Height; "maximized", box w.Maximized ])
              | None -> () ]
    File.WriteAllText(Path.Combine(directory, FileName), JsonSerializer.Serialize(stored, JsonSerializerOptions(WriteIndented = true)))

/// The preferences in force now, for what is drawn or done by them, and word when they change:
/// open editors take a new size, the windows' menus and keys new shortcuts.
type Live private () =
    static let changed = Event<Preferences>()
    static member val Current = defaults with get, set

    [<CLIEvent>]
    static member Changed = changed.Publish

    static member Set(preferences: Preferences) =
        Live.Current <- preferences
        changed.Trigger preferences
