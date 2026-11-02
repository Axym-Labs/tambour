/// Tambour's design tokens, the one source of the shell's spacing, type, sizes, radii and
/// colours. Generated from design/tokens.json by design/tokens.fsx: change those, not this file.
module Tambour.Tokens

open Avalonia.Media

/// The spacing scale, in device-independent pixels.
module Space =
    [<Literal>]
    let s0 = 0.0
    [<Literal>]
    let s1 = 4.0
    [<Literal>]
    let s2 = 8.0
    [<Literal>]
    let s3 = 12.0
    [<Literal>]
    let s4 = 16.0
    [<Literal>]
    let s5 = 24.0
    [<Literal>]
    let s6 = 32.0
    [<Literal>]
    let s7 = 48.0

/// The type scale: each step's size and line height.
module Type =
    [<Literal>]
    let mark = 10.0
    [<Literal>]
    let markLine = 12.0
    [<Literal>]
    let small = 12.0
    [<Literal>]
    let smallLine = 16.0
    [<Literal>]
    let ui = 13.0
    [<Literal>]
    let uiLine = 18.0
    [<Literal>]
    let title = 14.0
    [<Literal>]
    let titleLine = 20.0
    [<Literal>]
    let heading = 18.0
    [<Literal>]
    let headingLine = 24.0
    [<Literal>]
    let display = 26.0
    [<Literal>]
    let displayLine = 32.0
    [<Literal>]
    let code = 14.0
    [<Literal>]
    let codeLine = 20.0

/// The UI's typeface; code's is `Palette.code`, with its fallbacks.
let uiFont = FontFamily "Inter"

/// Font weights.
module Weight =
    [<Literal>]
    let regular = 400.0
    [<Literal>]
    let medium = 500.0
    [<Literal>]
    let semibold = 600.0

/// The heights of controls, rows and the window's bars.
module Height =
    [<Literal>]
    let controlSm = 24.0
    [<Literal>]
    let control = 28.0
    [<Literal>]
    let controlLg = 32.0
    [<Literal>]
    let row = 28.0
    [<Literal>]
    let gridRow = 24.0
    [<Literal>]
    let tabs = 36.0
    [<Literal>]
    let toolbar = 40.0
    [<Literal>]
    let paneHeader = 36.0
    [<Literal>]
    let status = 24.0
    [<Literal>]
    let icon = 16.0

/// The widths of the file list, a grid column's bounds, and a row's label.
module Width =
    [<Literal>]
    let files = 224.0
    [<Literal>]
    let columnMin = 40.0
    [<Literal>]
    let columnMax = 320.0
    [<Literal>]
    let label = 48.0

/// Corner radii.
module Radius =
    [<Literal>]
    let sm = 4.0
    [<Literal>]
    let md = 6.0
    [<Literal>]
    let lg = 8.0

[<Literal>]
let border = 1.0

/// The one shadow, for what floats over the window: popups, menus, dialogs.
let shadowOffset = (0.0, 8.0)
let shadowBlur = 24.0

/// A theme's colour roles.
type Colours =
    { Page: Color
      Surface: Color
      Raised: Color
      Line: Color
      LineStrong: Color
      Text: Color
      Muted: Color
      Faint: Color
      Accent: Color
      AccentFill: Color
      OnAccent: Color
      AccentSubtle: Color
      Hover: Color
      Selected: Color
      Marked: Color
      Pick: Color
      Error: Color
      Warning: Color
      Success: Color
      Scrim: Color
      Shadow: Color
      SyntaxKeyword: Color
      SyntaxType: Color
      SyntaxFunction: Color
      SyntaxVariable: Color
      SyntaxString: Color
      SyntaxNumber: Color
      SyntaxComment: Color }

let light =
    { Page = Color.Parse "#FFFFFF"
      Surface = Color.Parse "#F5F6F8"
      Raised = Color.Parse "#ECEEF1"
      Line = Color.Parse "#D9DDE2"
      LineStrong = Color.Parse "#BCC3CB"
      Text = Color.Parse "#1F2328"
      Muted = Color.Parse "#59636E"
      Faint = Color.Parse "#8C959F"
      Accent = Color.Parse "#0969DA"
      AccentFill = Color.Parse "#0969DA"
      OnAccent = Color.Parse "#FFFFFF"
      AccentSubtle = Color.Parse "#E3EEFC"
      Hover = Color.Parse "#0F1F2328"
      Selected = Color.Parse "#330969DA"
      Marked = Color.Parse "#50F5C028"
      Pick = Color.Parse "#C25E00"
      Error = Color.Parse "#CF222E"
      Warning = Color.Parse "#9A6700"
      Success = Color.Parse "#1A7F37"
      Scrim = Color.Parse "#591F2328"
      Shadow = Color.Parse "#381F2328"
      SyntaxKeyword = Color.Parse "#0000FF"
      SyntaxType = Color.Parse "#267F99"
      SyntaxFunction = Color.Parse "#795E26"
      SyntaxVariable = Color.Parse "#001080"
      SyntaxString = Color.Parse "#A31515"
      SyntaxNumber = Color.Parse "#098658"
      SyntaxComment = Color.Parse "#008000" }

let dark =
    { Page = Color.Parse "#1E1E1E"
      Surface = Color.Parse "#252526"
      Raised = Color.Parse "#2D2D30"
      Line = Color.Parse "#3C3C40"
      LineStrong = Color.Parse "#56565C"
      Text = Color.Parse "#D4D4D4"
      Muted = Color.Parse "#A0A4AA"
      Faint = Color.Parse "#6E7178"
      Accent = Color.Parse "#4DA3FF"
      AccentFill = Color.Parse "#1F6FEB"
      OnAccent = Color.Parse "#FFFFFF"
      AccentSubtle = Color.Parse "#0F3A5F"
      Hover = Color.Parse "#12FFFFFF"
      Selected = Color.Parse "#404DA3FF"
      Marked = Color.Parse "#50C8A028"
      Pick = Color.Parse "#F08A24"
      Error = Color.Parse "#F85149"
      Warning = Color.Parse "#D29922"
      Success = Color.Parse "#3FB950"
      Scrim = Color.Parse "#80000000"
      Shadow = Color.Parse "#99000000"
      SyntaxKeyword = Color.Parse "#569CD6"
      SyntaxType = Color.Parse "#4EC9B0"
      SyntaxFunction = Color.Parse "#DCDCAA"
      SyntaxVariable = Color.Parse "#9CDCFE"
      SyntaxString = Color.Parse "#CE9178"
      SyntaxNumber = Color.Parse "#B5CEA8"
      SyntaxComment = Color.Parse "#6A9955" }
