/// Telling the OS about `.tambour` files (gate 17 (d), what a test can see of it): on Linux,
/// everything under the data directory given and nothing beside it; the thumbnailer's command
/// writes the figure a bundle keeps, at the size asked.
module Tambour.Tests.AssociationTests

open System.IO
open Tambour
open Xunit

[<Fact>]
let ``the association on Linux writes only under the data directory, naming the app`` () =
    let data = (Directory.CreateTempSubdirectory "tambour-data").FullName
    let exe = "/opt/my apps/tambour"
    Association.linux data exe [| 137uy; 80uy |] false |> ignore

    let written =
        Directory.GetFiles(data, "*", SearchOption.AllDirectories)
        |> Array.map (fun f -> Path.GetRelativePath(data, f).Replace('\\', '/'))
        |> Array.sort

    Assert.Equal<string[]>(Association.linuxFiles |> List.sort |> Array.ofList, written)

    let read (f: string) = File.ReadAllText(Path.Combine(data, f))
    let desktop = read "applications/tambour.desktop"
    Assert.Contains("Exec=\"/opt/my apps/tambour\" %f", desktop)
    Assert.Contains($"MimeType={Bundle.MimeType};", desktop)
    Assert.Contains("Icon=tambour", desktop)
    let mime = read "mime/packages/tambour.xml"
    Assert.Contains($"<glob pattern=\"*{Bundle.Extension}\"/>", mime)
    Assert.Contains($"offset=\"30\" value=\"mimetype{Bundle.MimeType}\"", mime)
    Assert.Contains("--thumbnail %i %o %s", read "thumbnailers/tambour.thumbnailer")
    Assert.Equal<byte[]>([| 137uy; 80uy |], File.ReadAllBytes(Path.Combine(data, "icons/hicolor/256x256/mimetypes/application-x-tambour.png")))

/// Written at every start: each file only where it is missing or differs, the executable that
/// started last named.
[<Fact>]
let ``the association is written again only where it differs`` () =
    let data = (Directory.CreateTempSubdirectory "tambour-data").FullName
    let desktop () = File.ReadAllText(Path.Combine(data, "applications/tambour.desktop"))
    Assert.True(Association.linux data "/opt/tambour" [| 1uy |] false)
    Assert.Contains("StartupWMClass=tambour", desktop ())
    let written = File.GetLastWriteTimeUtc(Path.Combine(data, "mime/packages/tambour.xml"))
    Assert.False(Association.linux data "/opt/tambour" [| 1uy |] false)

    Assert.True(Association.linux data "/home/me/tambour.AppImage" [| 2uy |] false)
    Assert.Contains("Exec=\"/home/me/tambour.AppImage\" %f", desktop ())
    Assert.Contains($"MimeType={Bundle.MimeType};", desktop ())
    Assert.Equal<byte[]>([| 2uy |], File.ReadAllBytes(Path.Combine(data, "icons/hicolor/256x256/apps/tambour.png")))
    Assert.Equal(written, File.GetLastWriteTimeUtc(Path.Combine(data, "mime/packages/tambour.xml")))

[<Fact>]
let ``the thumbnailer writes a bundle's figure at the size asked, and fails on one without`` () =
    TestPaths.useScratchCache ()
    let dir = (Directory.CreateTempSubdirectory "tambour-thumb").FullName
    File.WriteAllText(Path.Combine(dir, "figure.fsx"), "")

    do
        use bitmap = new SkiaSharp.SKBitmap(400, 200)
        bitmap.Erase SkiaSharp.SKColors.SteelBlue
        use data = bitmap.Encode(SkiaSharp.SKEncodedImageFormat.Png, 100)
        File.WriteAllBytes(Path.Combine(dir, Bundle.Thumbnail), data.ToArray())

    let out = (Directory.CreateTempSubdirectory "tambour-thumb-out").FullName
    let file = Path.Combine(out, "thumb.tambour")
    Bundle.pack dir file
    let png = Path.Combine(out, "thumb.png")
    Assert.Equal(0, Program.main [| "--thumbnail"; file; png; "128" |])

    do
        use image = SkiaSharp.SKBitmap.Decode png
        Assert.Equal(128, image.Width)
        Assert.Equal(64, image.Height)

    File.Delete(Path.Combine(dir, Bundle.Thumbnail))
    Bundle.pack dir file
    Assert.Equal(1, Program.main [| "--thumbnail"; file; png |])
