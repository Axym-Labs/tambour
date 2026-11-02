// Writes the design tokens out of design/tokens.json: design/project/tokens.css for the mock-up
// (custom properties on `.tb`, the dark colours on `.tb.dark`) and src/Tambour/Tokens.fs for the
// app. Run after every change to the tokens:
//
//   dotnet fsi design/tokens.fsx

open System
open System.IO
open System.Text
open System.Text.Json

let here = __SOURCE_DIRECTORY__
let root = (JsonDocument.Parse(File.ReadAllText(Path.Combine(here, "tokens.json")))).RootElement
let entries (e: JsonElement) = [ for p in e.EnumerateObject() -> p.Name, p.Value ]
let number (e: JsonElement) = e.GetDouble()
let group (name: string) = entries (root.GetProperty name)

/// `line-strong` as an F# name: `LineStrong`.
let pascal (name: string) =
    name.Split '-' |> Array.map (fun w -> string (Char.ToUpperInvariant w.[0]) + w.Substring 1) |> String.concat ""

let camel (name: string) =
    let p = pascal name
    if Char.IsDigit p.[0] then "s" + p else string (Char.ToLowerInvariant p.[0]) + p.Substring 1

/// #RRGGBBAA, as CSS reads it, to the #AARRGGBB Avalonia reads.
let avalonia (hex: string) =
    if hex.Length = 9 then "#" + hex.Substring(7, 2) + hex.Substring(1, 6) else hex

let colours (theme: string) = entries ((root.GetProperty "colour").GetProperty theme)
let font (name: string) = (root.GetProperty "font").GetProperty(name).GetString()
let shadow = root.GetProperty "shadow"
let uiFont, codeFont = font "ui", font "code"
let size (e: JsonElement) = number (e.GetProperty "size")
let height (e: JsonElement) = number (e.GetProperty "line")
let border = number (root.GetProperty "border")
let shadowX, shadowY, shadowBlur = number (shadow.GetProperty "x"), number (shadow.GetProperty "y"), number (shadow.GetProperty "blur")
let property (role: string) = if role = "shadow" then "shadow-colour" else role
let generated = "Generated from design/tokens.json by design/tokens.fsx: change those, not this file."

let css =
    let b = StringBuilder()
    let line (s: string) = b.AppendLine s |> ignore
    line $"/* {generated} */"
    line ".tb {"
    for n, v in group "space" do line $"  --space-{n}: {number v}px;"
    line $"  --font-ui: \"{uiFont}\", system-ui, sans-serif;"
    line $"  --font-code: \"{codeFont}\", ui-monospace, monospace;"
    for n, v in group "type" do
        line $"  --size-{n}: {size v}px;"
        line $"  --line-{n}: {height v}px;"
    for n, v in group "weight" do line $"  --weight-{n}: {number v};"
    for n, v in group "height" do line $"  --h-{n}: {number v}px;"
    for n, v in group "width" do line $"  --w-{n}: {number v}px;"
    for n, v in group "radius" do line $"  --radius-{n}: {number v}px;"
    line $"  --border: {border}px;"
    line $"  --shadow: {shadowX}px {shadowY}px {shadowBlur}px var(--shadow-colour);"
    for n, v in colours "light" do line $"  --{property n}: {v.GetString()};"
    line "}"
    line ".tb.dark {"
    for n, v in colours "dark" do line $"  --{property n}: {v.GetString()};"
    line "}"
    b.ToString()

let fsharp =
    let b = StringBuilder()
    let line (s: string) = b.AppendLine s |> ignore
    let literals (name: string) (doc: string) (values: (string * float) list) =
        line $"/// {doc}"
        line $"module {name} ="
        for n, v in values do
            line "    [<Literal>]"
            line $"    let {camel n} = %.1f{v}"
        line ""
    line $"/// Tambour's design tokens, the one source of the shell's spacing, type, sizes, radii and"
    line $"/// colours. {generated}"
    line "module Tambour.Tokens"
    line ""
    line "open Avalonia.Media"
    line ""
    literals "Space" "The spacing scale, in device-independent pixels." [ for n, v in group "space" -> n, number v ]
    line "/// The type scale: each step's size and line height."
    line "module Type ="
    for n, v in group "type" do
        line "    [<Literal>]"
        line $"    let {camel n} = %.1f{size v}"
        line "    [<Literal>]"
        line $"    let {camel n}Line = %.1f{height v}"
    line ""
    line "/// The UI's typeface; code's is `Palette.code`, with its fallbacks."
    line $"let uiFont = FontFamily \"{uiFont}\""
    line ""
    literals "Weight" "Font weights." [ for n, v in group "weight" -> n, number v ]
    literals "Height" "The heights of controls, rows and the window's bars." [ for n, v in group "height" -> n, number v ]
    literals "Width" "The widths of the file list, a grid column's bounds, and a row's label." [ for n, v in group "width" -> n, number v ]
    literals "Radius" "Corner radii." [ for n, v in group "radius" -> n, number v ]
    line "[<Literal>]"
    line $"let border = %.1f{border}"
    line ""
    line "/// The one shadow, for what floats over the window: popups, menus, dialogs."
    line $"let shadowOffset = (%.1f{shadowX}, %.1f{shadowY})"
    line $"let shadowBlur = %.1f{shadowBlur}"
    line ""
    line "/// A theme's colour roles."
    line "type Colours ="
    let names = colours "light" |> List.map fst
    names |> List.iteri (fun i n -> line $"""    {(if i = 0 then "{ " else "  ")}{pascal n}: Color{(if i = names.Length - 1 then " }" else "")}""")
    line ""
    for theme in [ "light"; "dark" ] do
        line $"let {theme} ="
        let values = colours theme
        if (values |> List.map fst) <> names then failwith $"the {theme} colours name other roles than the light ones"
        values |> List.iteri (fun i (n, v) ->
            line $"""    {(if i = 0 then "{ " else "  ")}{pascal n} = Color.Parse "{avalonia (v.GetString())}"{(if i = values.Length - 1 then " }" else "")}""")
        line ""
    b.ToString().TrimEnd() + "\n"

let write (path: string) (text: string) =
    Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
    File.WriteAllText(path, text)
    printfn "%s" (Path.GetRelativePath(Path.Combine(here, ".."), path))

write (Path.Combine(here, "project", "tokens.css")) css
write (Path.Combine(here, "..", "src", "Tambour", "Tokens.fs")) fsharp
