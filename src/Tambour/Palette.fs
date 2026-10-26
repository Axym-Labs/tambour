/// The editor's colours, light and dark: the page, the problems, and the kinds of thing a script
/// names, which semantic highlighting and the completion list's glyphs share.
module Tambour.Palette

open Avalonia.Media
open Avalonia.Styling

type Look =
    { Background: Color
      Foreground: Color
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

/// The colours of Visual Studio Code's Light+, which the editor's grammar theme is.
let light =
    { Background = rgb "#FFFFFF"
      Foreground = rgb "#000000"
      LineNumbers = rgb "#6E7681"
      CurrentLine = rgb "#F3F3F3"
      Selection = rgb "#ADD6FF"
      Uses = Color.Parse "#40A0C8FF"
      Bracket = Color.Parse "#4080A0C0"
      Error = rgb "#D11E1E"
      Warning = rgb "#9A6700"
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

/// The colours of Visual Studio Code's Dark+, which the editor's grammar theme is.
let dark =
    { Background = rgb "#1E1E1E"
      Foreground = rgb "#D4D4D4"
      LineNumbers = rgb "#858585"
      CurrentLine = rgb "#2A2D2E"
      Selection = rgb "#264F78"
      Uses = Color.Parse "#40C8C8C8"
      Bracket = Color.Parse "#50A0A0A0"
      Error = rgb "#F14C4C"
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
