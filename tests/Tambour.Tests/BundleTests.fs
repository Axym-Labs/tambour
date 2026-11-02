/// A workspace as one `.tambour` file (gate 17 (a), (b)): what is saved comes back, a save stopped
/// part way leaves the file as it was, and the file is written only when what is saved changed.
module Tambour.Tests.BundleTests

open System
open System.IO
open System.IO.Compression
open System.Text
open Tambour
open Tambour.Data
open Xunit

let private scratch (name: string) =
    TestPaths.useScratchCache ()
    (Directory.CreateTempSubdirectory $"tambour-{name}").FullName

/// A workspace directory: two scripts, a table in each format, its settings, and a prelude.
let private workspace (dir: string) =
    Project.makeWorkspace dir
    File.WriteAllText(Path.Combine(dir, "figure.fsx"), "let x = 1\n// ü, a comment\n")
    File.WriteAllText(Path.Combine(dir, "other.fsx"), "let y = 2\n")

    let table =
        { Name = "trial"
          Columns =
            [ { Name = "t"; Unit = "s"; Values = Floats [| 0.0; 0.5; nan |] }
              { Name = "label"; Unit = null; Values = Texts [| "a"; null; "c" |] } ] }

    Csv.writeFile (Path.Combine(dir, "tables", "trial.csv")) table
    Arrow.writeFile (Path.Combine(dir, "tables", "big.arrow")) { table with Name = "big" }
    Project.write dir (Project.defaults "figure.fsx")
    File.WriteAllText(Path.Combine(dir, Prelude.FileName), "// written again at every open\n")

let private files (dir: string) =
    Directory.GetFiles(dir, "*", SearchOption.AllDirectories)
    |> Array.map (fun f -> Path.GetRelativePath(dir, f).Replace('\\', '/'))
    |> Array.filter (fun f -> not (f.StartsWith ".") && f <> Prelude.FileName)
    |> Array.sort

[<Fact>]
let ``a workspace saved as a bundle opens as the workspace saved`` () =
    let dir = scratch "bundle-source"
    workspace dir
    let file = Path.Combine(scratch "bundle", "paper.tambour")
    Bundle.pack dir file

    match Bundle.openBundle file with
    | Error why -> failwith why
    | Ok opened ->
        Assert.False opened.WrittenBack
        Assert.Equal<string[]>(files dir, files opened.Directory)

        for f in files dir do
            Assert.Equal<byte[]>(File.ReadAllBytes(Path.Combine(dir, f)), File.ReadAllBytes(Path.Combine(opened.Directory, f)))

        Assert.Equal(Project.read dir, Project.read opened.Directory)
        Assert.Equal(Some file, Bundle.ofDirectory opened.Directory)
        Bundle.close file
        Assert.False(Directory.Exists opened.Directory)

[<Fact>]
let ``a bundle begins with its stored mimetype, and a zip without one does not open`` () =
    let dir = scratch "bundle-mime"
    workspace dir
    let file = Path.Combine(dir, "..", "mime.tambour") |> Path.GetFullPath
    Bundle.pack dir file

    let bytes = File.ReadAllBytes file
    // A zip's first local header: the name at 30, the data stored right after it.
    Assert.Equal("mimetype", Encoding.ASCII.GetString(bytes, 30, 8))
    Assert.Equal(Bundle.MimeType, Encoding.ASCII.GetString(bytes, 38, Bundle.MimeType.Length))

    let plain = Path.Combine(scratch "bundle-plain", "plain.tambour")
    ZipFile.CreateFromDirectory(dir, plain)

    match Bundle.openBundle plain with
    | Ok _ -> failwith "a plain zip opened"
    | Error why -> Assert.Contains("not a Tambour file", why)

[<Fact>]
let ``a save stopped part way leaves the bundle as it was`` () =
    let dir = scratch "bundle-stopped"
    workspace dir
    let file = Path.Combine(scratch "bundle-stopped-out", "stopped.tambour")
    Bundle.pack dir file
    let before = File.ReadAllBytes file

    File.WriteAllText(Path.Combine(dir, "figure.fsx"), "let x = 2\n")

    // A file that cannot be read stops the save after the entries before it are written.
    do
        use _ = new FileStream(Path.Combine(dir, "tables", "trial.csv"), FileMode.Open, FileAccess.ReadWrite, FileShare.None)
        Assert.ThrowsAny<IOException>(fun () -> Bundle.pack dir file) |> ignore

    Assert.Equal<byte[]>(before, File.ReadAllBytes file)
    Assert.Equal<string[]>([| "stopped.tambour" |], Directory.GetFiles(Path.GetDirectoryName file) |> Array.map Path.GetFileName)

[<Fact>]
let ``a bundle is written only when what is saved in it changed`` () =
    let dir = scratch "bundle-sync"
    workspace dir
    let file = Path.Combine(scratch "bundle-sync-out", "sync.tambour")
    Bundle.pack dir file
    let written = File.GetLastWriteTimeUtc file

    let opened =
        match Bundle.openBundle file with
        | Ok o -> o
        | Error why -> failwith why

    Assert.False(Bundle.sync file)
    Assert.Equal(written, File.GetLastWriteTimeUtc file)

    File.WriteAllText(Path.Combine(opened.Directory, "figure.fsx"), "let x = 3\n")
    Assert.True(Bundle.sync file)
    Assert.False(Bundle.sync file)
    Bundle.close file

    match Bundle.openBundle file with
    | Ok again ->
        Assert.Equal("let x = 3\n", File.ReadAllText(Path.Combine(again.Directory, "figure.fsx")))
        Bundle.close file
    | Error why -> failwith why

[<Fact>]
let ``a cache left behind is written back, unless its bundle changed since`` () =
    let dir = scratch "bundle-left"
    workspace dir
    let file = Path.Combine(scratch "bundle-left-out", "left.tambour")
    Bundle.pack dir file

    // Saved into the cache, then the run ends without closing it.
    let first =
        match Bundle.openBundle file with
        | Ok o -> o
        | Error why -> failwith why

    File.WriteAllText(Path.Combine(first.Directory, "other.fsx"), "let y = 20\n")
    Bundle.abandon file

    match Bundle.openBundle file with
    | Ok again ->
        Assert.True again.WrittenBack
        Assert.Equal("let y = 20\n", File.ReadAllText(Path.Combine(again.Directory, "other.fsx")))
        File.WriteAllText(Path.Combine(again.Directory, "other.fsx"), "let y = 30\n")
        Bundle.abandon file
    | Error why -> failwith why

    // Someone else writes the bundle meanwhile: theirs is kept, the cache dropped.
    File.WriteAllText(Path.Combine(dir, "other.fsx"), "let y = 40\n")
    Bundle.pack dir file

    match Bundle.openBundle file with
    | Ok again ->
        Assert.False again.WrittenBack
        Assert.Equal("let y = 40\n", File.ReadAllText(Path.Combine(again.Directory, "other.fsx")))
        Bundle.close file
    | Error why -> failwith why

[<Fact>]
let ``a bundle another process has open does not open here`` () =
    let file = Path.Combine(scratch "bundle-locked", "locked.tambour")
    let cache = Bundle.cacheOf file
    Directory.CreateDirectory cache |> ignore

    do
        use _ = new FileStream(Path.Combine(cache, ".lock"), FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.None)

        match Bundle.openBundle file with
        | Ok _ -> failwith "opened while locked"
        | Error why -> Assert.Contains("open in another Tambour", why)

    match Bundle.openBundle file with
    | Ok opened ->
        // Not there yet: an empty workspace, which its first sync writes out.
        Assert.Empty(files opened.Directory)
        File.WriteAllText(Path.Combine(opened.Directory, "figure.fsx"), "")
        Assert.True(Bundle.sync file)
        Assert.True(File.Exists file)
        Bundle.close file
    | Error why -> failwith why

[<Fact>]
let ``an entry that would land outside the workspace is refused`` () =
    let file = Path.Combine(scratch "bundle-slip", "slip.tambour")

    do
        use zip = ZipFile.Open(file, ZipArchiveMode.Create)

        do
            use s = zip.CreateEntry("mimetype", CompressionLevel.NoCompression).Open()
            s.Write(Encoding.ASCII.GetBytes Bundle.MimeType)

        use s = zip.CreateEntry("../outside.fsx").Open()
        s.Write(Encoding.ASCII.GetBytes "let z = 1\n")

    match Bundle.openBundle file with
    | Ok _ -> failwith "opened"
    | Error why -> Assert.Contains("outside the workspace", why)

    Assert.False(File.Exists(Path.Combine(Path.GetDirectoryName(Bundle.cacheOf file), "outside.fsx")))

[<Fact>]
let ``export writes a folder that opens as a workspace with the same scripts and tables`` () =
    let dir = scratch "bundle-export"
    workspace dir
    File.WriteAllBytes(Path.Combine(dir, Bundle.Thumbnail), [| 1uy |])
    let target = scratch "bundle-exported"
    Bundle.export dir target

    Assert.True(Project.isWorkspace target)
    Assert.True(File.Exists(Path.Combine(target, Prelude.FileName)))
    Assert.False(File.Exists(Path.Combine(target, Bundle.Thumbnail)))
    Assert.Equal<string[]>(Project.scripts dir, Project.scripts target)
    let names (d: string) = fst (Tables.load d) |> List.map (fun s -> s.Table)
    Assert.Equal<Table list>(names dir, names target)
