/// Tambour's two themes, light and dark, one palette each for everything the window draws: the
/// editor's page, the problems, the kinds of thing a script names (which semantic highlighting and
/// the completion list's glyphs share), the grid, and Fluent's own controls.
module Tambour.Palette

open Avalonia.Media
open Avalonia.Styling

type Look =
    { /// The editor's page and the grid's cells.
      Background: Color
      Foreground: Color
      /// The window's own ground: the panels, and the surround of a figure.
      Surface: Color
      /// Headers and boxes raised on the surface or the page.
      Raised: Color
      /// The grid's rules and the panels' borders.
      Line: Color
      /// Text on the surface, raised or not.
      Text: Color
      /// Text that says less: a column's type, a row's number, a note.
      Dim: Color
      Accent: Color
      /// The grid's selection, over its cells.
      Selected: Color
      /// Rows a pick marked.
      Marked: Color
      LineNumbers: Color
      CurrentLine: Color
      Selection: Color
      /// The uses of the symbol at the caret, and the bracket matching the caret's.
      Uses: Color
      Bracket: Color
      Error: Color
      Warning: Color
      Info: Color
      /// A semantic token type's colour, by its name (`function`, `enumMember`, …).
      Kinds: Map<string, Color> }

let private rgb (hex: string) = Color.Parse hex

/// The font code is shown in (the editor, the output, a signature): JetBrains Mono, carried in the
/// app so the editor looks the same on every OS.
let code = FontFamily "avares://tambour/Fonts#JetBrains Mono, Cascadia Code, Consolas, DejaVu Sans Mono, monospace"

/// The editor's colours are Visual Studio Code's Light+, as its grammar theme is.
let light =
    { Background = rgb "#FFFFFF"
      Foreground = rgb "#000000"
      Surface = rgb "#F3F4F6"
      Raised = rgb "#EAECEF"
      Line = rgb "#D8DCE1"
      Text = rgb "#1F2328"
      Dim = rgb "#57606A"
      Accent = rgb "#0969DA"
      Selected = Color.Parse "#330969DA"
      Marked = Color.Parse "#50F5C028"
      LineNumbers = rgb "#6E7681"
      CurrentLine = rgb "#F3F3F3"
      Selection = rgb "#ADD6FF"
      Uses = Color.Parse "#40A0C8FF"
      Bracket = Color.Parse "#4080A0C0"
      Error = rgb "#D11E1E"
      Warning = rgb "#8A5C00"
      Info = rgb "#1A6FB0"
      Kinds =
        Map.ofList
            [ "namespace", rgb "#267F99"
              "type", rgb "#267F99"
              "class", rgb "#267F99"
              "struct", rgb "#237A52"
              "enum", rgb "#237A52"
              "interface", rgb "#267F99"
              "typeParameter", rgb "#267F99"
              "parameter", rgb "#001080"
              "variable", rgb "#001080"
              "property", rgb "#001080"
              "enumMember", rgb "#0070C1"
              "event", rgb "#795E26"
              "function", rgb "#795E26"
              "method", rgb "#795E26"
              "operator", rgb "#000000"
              "keyword", rgb "#0000FF" ] }

/// A dark grey, not black. The editor's colours are Visual Studio Code's Dark+, as its grammar
/// theme is.
let dark =
    { Background = rgb "#1E1E1E"
      Foreground = rgb "#D4D4D4"
      Surface = rgb "#252526"
      Raised = rgb "#2D2D30"
      Line = rgb "#3E3E42"
      Text = rgb "#CCCCCC"
      Dim = rgb "#A0A4AA"
      Accent = rgb "#4DA3FF"
      Selected = Color.Parse "#404DA3FF"
      Marked = Color.Parse "#50C8A028"
      LineNumbers = rgb "#858585"
      CurrentLine = rgb "#2A2D2E"
      Selection = rgb "#264F78"
      Uses = Color.Parse "#40C8C8C8"
      Bracket = Color.Parse "#50A0A0A0"
      Error = rgb "#FF6464"
      Warning = rgb "#CCA700"
      Info = rgb "#3794FF"
      Kinds =
        Map.ofList
            [ "namespace", rgb "#4EC9B0"
              "type", rgb "#4EC9B0"
              "class", rgb "#4EC9B0"
              "struct", rgb "#86C691"
              "enum", rgb "#B8D7A3"
              "interface", rgb "#B8D7A3"
              "typeParameter", rgb "#B8D7A3"
              "parameter", rgb "#9CDCFE"
              "variable", rgb "#9CDCFE"
              "property", rgb "#9CDCFE"
              "enumMember", rgb "#4FC1FF"
              "event", rgb "#DCDCAA"
              "function", rgb "#DCDCAA"
              "method", rgb "#DCDCAA"
              "operator", rgb "#D4D4D4"
              "keyword", rgb "#569CD6" ] }

let forVariant (variant: ThemeVariant) = if variant = ThemeVariant.Dark then dark else light

/// The editor's grammar theme for a look: Light+ or Dark+ as Tambour carries them, with the few
/// colours under 4.5:1 on the page raised to it.
let grammarTheme (look: Look) =
    let name = if obj.ReferenceEquals(look, dark) then "dark" else "light"
    let assembly = typeof<Look>.Assembly
    use stream = assembly.GetManifestResourceStream $"Tambour.Themes.{name}.json"
    use reader = new System.IO.StreamReader(stream)
    TextMateSharp.Internal.Themes.Reader.ThemeReader.ReadThemeSync reader

/// Fluent's resources for a look, so its controls are drawn in the same colours on every OS
/// rather than in the system's accent and Fluent's own greys.
let fluent (look: Look) =
    let isDark = obj.ReferenceEquals(look, dark)

    Avalonia.Themes.Fluent.ColorPaletteResources(
        Accent = look.Accent,
        RegionColor = look.Surface,
        AltHigh = look.Background,
        BaseHigh = look.Text,
        ChromeMediumLow = look.Raised,
        ChromeMedium = (if isDark then rgb "#333337" else rgb "#E2E5E9"),
        ErrorText = look.Error
    )

/// Every text colour of a look with the background it is drawn on, by a name for each: the
/// editor's page, the grid, the panels, the readout over a figure. The grammar theme's colours
/// are drawn on the page too.
let pairs (look: Look) : (string * Color * Color) list =
    [ "text", look.Foreground, look.Background
      "line numbers", look.LineNumbers, look.Background
      "error", look.Error, look.Background
      "warning", look.Warning, look.Background
      "info", look.Info, look.Background
      for KeyValue(kind, c) in look.Kinds do
          $"kind {kind}", c, look.Background
      "cell", look.Foreground, look.Background
      "column header", look.Text, look.Raised
      "column type", look.Dim, look.Raised
      "row number", look.Dim, look.Raised
      "panel text", look.Text, look.Surface
      "panel note", look.Dim, look.Surface
      "panel error", look.Error, look.Surface
      "panel warning", look.Warning, look.Surface
      "readout", look.Text, look.Raised ]

/// A completion's glyph, by the name of its `FSharpGlyph` case: the mark it shows, and the kind
/// whose colour it takes (or `error`).
let glyphs: Map<string, string * string> =
    Map.ofList
        [ "NameSpace", ("N", "namespace")
          "Module", ("M", "namespace")
          "Class", ("C", "class")
          "Struct", ("S", "struct")
          "Union", ("U", "type")
          "Type", ("T", "type")
          "Typedef", ("T", "type")
          "Interface", ("I", "interface")
          "Exception", ("X", "type")
          "Enum", ("E", "enum")
          "Delegate", ("D", "type")
          "TypeParameter", ("'", "typeParameter")
          "Method", ("m", "method")
          "OverridenMethod", ("m", "method")
          "ExtensionMethod", ("m", "method")
          "Property", ("p", "property")
          "Field", ("f", "property")
          "Event", ("e", "event")
          "EnumMember", ("c", "enumMember")
          "Constant", ("k", "enumMember")
          "Variable", ("v", "variable")
          "Error", ("!", "error") ]

/// A kind's colour in a look; a problem's for `error`, the text's for one it does not name.
let colourOf (look: Look) (kind: string) =
    match kind with
    | "error" -> look.Error
    | _ -> look.Kinds.TryFind kind |> Option.defaultValue look.Foreground
