/// A project is a directory: its `*.fsx` scripts and a `project.json` holding what the window
/// needs to open it again, which script was open, the Skia settings the figures are measured and
/// exported with, and the Marey the scripts were last run on. A single `.fsx` opens without one.
module Tambour.Project

open System
open System.IO
open System.Text.Json
open Tambour.Protocol

[<Literal>]
let FileName = "project.json"

/// What `project.json` holds.
type Settings =
    { /// The script that was open, by its name in the directory.
      Script: string
      /// The Marey the project's scripts were last run on.
      Marey: string
      Skia: SkiaSettings
      /// Where the divider between the script and the figure is, as a fraction of the width.
      Split: float }

/// The Marey this app runs scripts on.
let mareyVersion =
    let assembly = typeof<Marey.Figure>.Assembly

    match assembly.GetCustomAttributes(typeof<Reflection.AssemblyInformationalVersionAttribute>, false) with
    | [| :? Reflection.AssemblyInformationalVersionAttribute as v |] -> v.InformationalVersion.Split('+').[0]
    | _ -> string (assembly.GetName().Version)

let defaults (script: string) =
    { Script = script
      Marey = mareyVersion
      Skia = { Dpi = float Marey.Space.screenDpi; FontFiles = [||] }
      Split = 0.5 }

let private json = JsonSerializerOptions(WriteIndented = true)

/// The project's settings, if it has a readable `project.json`.
let read (directory: string) : Settings option =
    let path = Path.Combine(directory, FileName)

    if File.Exists path then
        try
            match JsonSerializer.Deserialize<Settings>(File.ReadAllText path, json) with
            | s when isNull (box s) -> None
            | s -> Some { s with Skia = (if isNull (box s.Skia) then (defaults s.Script).Skia else s.Skia) }
        with _ ->
            None
    else
        None

let write (directory: string) (settings: Settings) =
    File.WriteAllText(Path.Combine(directory, FileName), JsonSerializer.Serialize(settings, json))

/// The project's scripts, by name.
let scripts (directory: string) : string[] =
    Directory.GetFiles(directory, "*.fsx") |> Array.map Path.GetFileName |> Array.sort

/// What opening a path comes to: the script to show, and the project it belongs to, if any.
type Opened =
    { Script: string
      Project: string option
      Settings: Settings option }

/// Opens a script, or a directory as a project: the script it last had open, or its first, or a
/// new `figure.fsx`.
let resolve (path: string) : Opened =
    let path = Path.GetFullPath path

    if Directory.Exists path then
        let settings = read path
        let names = scripts path

        let name =
            match settings with
            | Some s when Array.contains s.Script names -> s.Script
            | _ when names.Length > 0 -> names.[0]
            | _ -> "figure.fsx"

        { Script = Path.Combine(path, name)
          Project = Some path
          Settings = settings }
    else
        { Script = path
          Project = None
          Settings = None }
