/// A library's documentation in a tooltip: its Markdown drawn as such, and a signature's names in
/// their kinds' colours.
module Tambour.Tests.MarkdownTests

open Avalonia.Controls.Documents
open Avalonia.Media
open Tambour
open Tambour.Markdown
open Xunit

[<Fact>]
let ``Marey's documentation reads as headings, paragraphs, items and code`` () =
    let text =
        "**The quick, front door.**\n\n```fsharp\nChart.lineIn U.second U.metre ts xs\n|> Chart.withTitle \"Fall\"\n```\n\n### Why the witnesses\n\nThere is no\nalternative.\n\n- one `U.second`\n- two"

    Assert.Equal<Block list>(
        [ Paragraph "**The quick, front door.**"
          Code "Chart.lineIn U.second U.metre ts xs\n|> Chart.withTitle \"Fall\""
          Heading(3, "Why the witnesses")
          Paragraph "There is no alternative."
          Item "one `U.second`"
          Item "two" ],
        parse text
    )

[<Fact>]
let ``bold, italic and code are taken only where they close`` () =
    Assert.Equal<Span list>(
        [ Plain "a "; Bold "b"; Plain " "; Italic "c"; Plain " "; InlineCode "float<s>"; Plain " 2 * 3 and **open" ],
        spans "a **b** *c* `float<s>` 2 * 3 and **open"
    )

let private coloured (text: string) =
    [ for i in Shown.signature Palette.light text do
          match i with
          | :? Run as r -> (r.Text, (r.Foreground :?> ISolidColorBrush).Color)
          | _ -> () ]
    |> List.filter (fun (t, _) -> t.Trim() <> "")

[<Fact>]
let ``a module's name and where it is from are drawn as a namespace, not a function`` () =
    let colour kind = Palette.colourOf Palette.light kind

    Assert.Equal<(string * Color) list>(
        [ "module", colour "keyword"; "Chart", colour "namespace"; "from", colour "keyword"; "Marey", colour "namespace" ],
        coloured "module Chart\n\nfrom Marey"
    )

    Assert.Equal<(string * Color) list>(
        [ "val", colour "keyword"; "lineIn", colour "function"; ":", Palette.light.Foreground; "x", colour "type" ],
        coloured "val lineIn: x"
    )

/// Marey's own documentation of `Chart`, as a hover shows it: a code block and headings, not
/// Markdown's marks. Saved in both looks where TAMBOUR_CHECK_OUT names a directory.
[<Avalonia.Headless.XUnit.AvaloniaFact>]
let ``Chart's documentation is drawn as Markdown`` () =
    let file = System.IO.Path.Combine(System.AppContext.BaseDirectory, "Marey.Plot.xml")

    let xml =
        System.Xml.Linq.XDocument.Load(file).Descendants(System.Xml.Linq.XName.Get "member")
        |> Seq.find (fun m -> m.Attribute(System.Xml.Linq.XName.Get "name").Value = "T:Marey.ChartModule")
        |> fun m -> System.String.Concat(m.Nodes() |> Seq.map string)

    let doc = Analysis.readable xml
    let blocks = parse doc
    Assert.Contains(blocks, fun b -> match b with Code c -> c.Contains "Chart.withTitle" | _ -> false)
    Assert.Contains(blocks, fun b -> match b with Heading(3, _) -> true | _ -> false)

    for name, look in [ "light", Palette.light; "dark", Palette.dark ] do
        let panel = Avalonia.Controls.StackPanel(Spacing = 6.0, MaxWidth = 640.0, Margin = Avalonia.Thickness 10.0)
        panel.Children.Add(Avalonia.Controls.TextBlock(Inlines = Shown.signature look "module Chart\n\nfrom Marey", FontFamily = Palette.code))
        panel.Children.Add(view look doc)
        let border = Avalonia.Controls.Border(Background = SolidColorBrush look.Background, Child = panel)
        let window = Avalonia.Controls.Window(Width = 680.0, Height = 900.0, Content = border)
        window.Show()

        match System.Environment.GetEnvironmentVariable "TAMBOUR_CHECK_OUT" with
        | null
        | "" -> ()
        | dir ->
            use frame = Avalonia.Headless.HeadlessWindowExtensions.CaptureRenderedFrame window
            frame.Save(System.IO.Path.Combine(dir, $"doc-{name}.png"), Avalonia.Media.Imaging.PngBitmapEncoderOptions.Default)

        window.Close()
