/// Gate 16 (a): the design tokens are the only source of spacing, type sizes, radii and colours in
/// the shell's views. The views' source is read, and any margin, padding, spacing, radius or font
/// size written as a number, or any colour written out, is named.
module Tambour.Tests.DesignTests

open System
open System.IO
open System.Text.RegularExpressions
open Xunit

/// The views: everything that draws the shell, but not the token table and the palette themselves.
let private views =
    [ "Shell.fs"; "TableGrid.fs"; "FigurePane.fs"; "PreferencesView.fs"; "Markdown.fs"; "ScriptEditor.fs" ]

let private source (name: string) =
    let rec root (dir: DirectoryInfo) =
        if isNull dir then failwith "no Tambour.slnx above the tests"
        elif File.Exists(Path.Combine(dir.FullName, "Tambour.slnx")) then dir.FullName
        else root dir.Parent

    File.ReadAllLines(Path.Combine(root (DirectoryInfo AppContext.BaseDirectory), "src", "Tambour", name))

/// A number other than 0 written where a size goes.
let private literal = Regex(@"(?<![\w.])[1-9]\d*(\.\d+)?(?![\w.])")

/// A token scaled by a number is the token's; a number compared with is no size.
let private scaled = Regex(@"\d+(\.\d+)?\s*[*/]\s*Tokens\.[\w.]+|Tokens\.[\w.]+\s*[*/+-]\s*\d+(\.\d+)?|[<>]=?\s*\d+(\.\d+)?")

/// Where the argument of a size ends: at the next named argument or attribute.
let private next = Regex(@";|,\s*[A-Z]\w*\s*=")

let private sized =
    Regex(@"(?:Thickness|CornerRadius|[Ss]pacing|FontSize|fontSize|[Mm]argin|[Pp]adding)\b\s*[=(]?\s*\(?([^\n]*)")

let private colour =
    Regex(@"Color\.Parse|Color\.From\w+|rgb ""|Brushes\.(?!Transparent\b)\w+|Colors\.\w+")

/// The lines of a source that write a size or a colour themselves, each with its number.
let offenders (lines: string seq) =
    [ for i, line in Seq.indexed lines do
          let code = (match line.IndexOf "//" with -1 -> line | at -> line.Substring(0, at))
          // A drawn icon's stroke is the drawing's own, not the layout's.
          if not (code.Contains "trokeThickness") then
              let code = scaled.Replace(code, "token")

              let bad =
                  colour.IsMatch code
                  || sized.Matches code |> Seq.exists (fun m -> literal.IsMatch(next.Split(m.Groups.[1].Value).[0]))

              if bad then yield i + 1, line.Trim() ]

[<Fact>]
let ``the check finds a size or colour written out, and lets a token be`` () =
    let samples =
        [ "let panel = StackPanel(Spacing = 12.0, Margin = Thickness 18.0)"
          "    TextBlock.fontSize 15.0"
          "    Border.cornerRadius (CornerRadius 3.0)"
          "    Foreground = Brushes.Black"
          "    let accent = rgb \"#0969DA\""
          "    Button.padding (Thickness(Tokens.Space.s2, 0.0))"
          "    Border.borderThickness (Thickness(0.0, 0.0, 0.0, 2.0 * Tokens.border))"
          "    Shapes.Path.strokeThickness 1.5"
          "    Background = Brushes.Transparent // Thickness 4.0 in a comment"
          "    StackPanel(Spacing = Tokens.Space.s2, MaxWidth = 640.0)"
          "    FontSize = (if level <= 2 then Tokens.Type.title else Tokens.Type.ui)"
          "    Pen(SolidColorBrush(Color.FromRgb(230uy, 120uy, 0uy)), 2.0)"
          "    SolidColorBrush(Color.FromArgb(40uy, 30uy, 90uy, 200uy))" ]

    Assert.Equal<int list>([ 1; 2; 3; 4; 5; 12; 13 ], offenders samples |> List.map fst)

[<Fact>]
let ``the views take every size and colour from the tokens`` () =
    let found =
        [ for name in views do
              for line, text in offenders (source name) -> $"{name}:{line}: {text}" ]

    Assert.True(found.IsEmpty, "sizes or colours written out in the views:\n" + String.Join("\n", found))
