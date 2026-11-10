// The source a script's names are defined in, for the editor's go to definition: Marey's and
// FSharp.Core's, each at the commit its package was built from, as one zip the app carries.
//
//   dotnet fsi tools/sources.fsx <out.zip> <NuGet package root> <Marey version> <FSharp.Core version>
//
// Each package's nuspec names its repository and commit. Marey's sources come from a tarball of
// that commit; FSharp.Core's, the files its project compiles, one by one, since its repository is
// the whole of .NET. Downloads are kept under the user's cache, so a commit is fetched once. Where
// a download fails the zip is written without that library, and the editor says it has no source.
//
// The zip holds `marey/src/...` and `fsharp-core/src/FSharp.Core/...` and `sources.json`, naming
// each library's commit.

#r "System.Formats.Tar"

open System
open System.Formats.Tar
open System.IO
open System.IO.Compression
open System.Net.Http
open System.Text.RegularExpressions
open System.Xml.Linq

let args = fsi.CommandLineArgs |> Array.skip 1

if args.Length <> 4 then
    eprintfn "usage: dotnet fsi tools/sources.fsx <out.zip> <NuGet package root> <Marey version> <FSharp.Core version>"
    exit 2

let out, packages, mareyVersion, coreVersion = args.[0], args.[1], args.[2], args.[3]

/// A package's repository and commit, as its nuspec names them.
let repositoryOf (id: string) (version: string) =
    let nuspec = Path.Combine(packages, id, version.ToLowerInvariant(), id + ".nuspec")
    let doc = XDocument.Load nuspec
    let repo = doc.Descendants() |> Seq.find (fun e -> e.Name.LocalName = "repository")
    let url = repo.Attribute(XName.Get "url").Value.TrimEnd('/')
    let url = if url.EndsWith ".git" then url.Substring(0, url.Length - 4) else url
    let commit = repo.Attribute(XName.Get "commit").Value
    // https://github.com/owner/name
    let ownerName = Uri(url).AbsolutePath.Trim('/')
    ownerName, commit

let cache =
    let root =
        match Environment.GetEnvironmentVariable "XDG_CACHE_HOME" with
        | null
        | "" when OperatingSystem.IsWindows() -> Environment.GetFolderPath Environment.SpecialFolder.LocalApplicationData
        | null
        | "" -> Path.Combine(Environment.GetFolderPath Environment.SpecialFolder.UserProfile, ".cache")
        | d -> d

    Path.Combine(root, "tambour-build", "sources")

let http = new HttpClient(Timeout = TimeSpan.FromMinutes 5.0)
http.DefaultRequestHeaders.UserAgent.ParseAdd "tambour-build"

/// A file fetched once and kept in the cache.
let fetch (url: string) (keep: string) =
    if not (File.Exists keep) then
        Directory.CreateDirectory(Path.GetDirectoryName keep) |> ignore
        let bytes = http.GetByteArrayAsync(url).Result
        let part = keep + ".part"
        File.WriteAllBytes(part, bytes)
        File.Move(part, keep, true)

    keep

let isSource (name: string) = name.EndsWith ".fs" || name.EndsWith ".fsi"

/// Marey's `src`, its F# files and project files, by path from the repository's root.
let marey () =
    let ownerName, commit = repositoryOf "marey.core" mareyVersion
    let tarball = fetch $"https://codeload.github.com/{ownerName}/tar.gz/{commit}" (Path.Combine(cache, $"marey-{commit}.tar.gz"))
    use gz = new GZipStream(File.OpenRead tarball, CompressionMode.Decompress)
    use tar = new TarReader(gz)
    let files = ResizeArray<string * byte[]>()

    let rec next () =
        match tar.GetNextEntry() with
        | null -> ()
        | entry ->
            // The tarball's paths begin with `<name>-<commit>/`.
            let slash = entry.Name.IndexOf '/'
            let path = if slash >= 0 then entry.Name.Substring(slash + 1) else entry.Name

            if entry.EntryType = TarEntryType.RegularFile
               && path.StartsWith "src/"
               && (isSource path || path.EndsWith ".fsproj")
               && not (isNull entry.DataStream) then
                use m = new MemoryStream()
                entry.DataStream.CopyTo m
                files.Add(path, m.ToArray())

            next ()

    next ()
    commit, List.ofSeq files

/// FSharp.Core's project and the files it compiles, by path from its project's directory.
let fsharpCore () =
    let ownerName, commit = repositoryOf "fsharp.core" coreVersion
    // In .NET's unified repository F# is under src/fsharp; in F#'s own, at the root.
    let roots = [ "src/fsharp/src/FSharp.Core"; "src/FSharp.Core" ]
    let raw (root: string) (file: string) = $"https://raw.githubusercontent.com/{ownerName}/{commit}/{root}/{file}"
    let keep (file: string) = Path.Combine(cache, $"fsharp-core-{commit}", file)

    let root, project =
        roots
        |> List.pick (fun root ->
            try
                Some(root, File.ReadAllText(fetch (raw root "FSharp.Core.fsproj") (keep "FSharp.Core.fsproj")))
            with _ ->
                None)

    let compiled =
        Regex.Matches(project, "<Compile\\s+Include=\"([^\"$]+)\"")
        |> Seq.map (fun m -> m.Groups.[1].Value.Replace('\\', '/'))
        |> Seq.filter isSource
        |> List.ofSeq

    let files =
        ("FSharp.Core.fsproj", File.ReadAllBytes(keep "FSharp.Core.fsproj"))
        :: [ for file in compiled -> file, File.ReadAllBytes(fetch (raw root file) (keep file)) ]

    commit, files

let attempt (name: string) (f: unit -> string * (string * byte[]) list) =
    try
        Some(f ())
    with e ->
        eprintfn "warning: no %s source in this build: %s" name (e.GetBaseException().Message)
        None

let mareyFiles = attempt "Marey" marey
let coreFiles = attempt "FSharp.Core" fsharpCore

Directory.CreateDirectory(Path.GetDirectoryName(Path.GetFullPath out)) |> ignore
let part = out + ".part"

do
    use zip = ZipFile.Open(part, ZipArchiveMode.Create)

    let add (prefix: string) (files: (string * byte[]) list) =
        for path, bytes in files do
            let entry = zip.CreateEntry(prefix + "/" + path, CompressionLevel.SmallestSize)
            // Fixed times, so the same commits make the same zip.
            entry.LastWriteTime <- DateTimeOffset(2000, 1, 1, 0, 0, 0, TimeSpan.Zero)
            use s = entry.Open()
            s.Write(bytes, 0, bytes.Length)

    mareyFiles |> Option.iter (snd >> add "marey")
    coreFiles |> Option.iter (fun (_, files) -> add "fsharp-core/src/FSharp.Core" files)

    let commit = Option.map fst >> Option.defaultValue ""

    let manifest =
        $"{{\"marey\": {{\"version\": \"{mareyVersion}\", \"commit\": \"{commit mareyFiles}\"}}, \"fsharpCore\": {{\"version\": \"{coreVersion}\", \"commit\": \"{commit coreFiles}\"}}}}\n"

    let entry = zip.CreateEntry("sources.json")
    entry.LastWriteTime <- DateTimeOffset(2000, 1, 1, 0, 0, 0, TimeSpan.Zero)
    use w = new StreamWriter(entry.Open())
    w.Write manifest

File.Move(part, out, true)

// A zip short of a library is written dated long ago, so the next build tries again.
if mareyFiles.IsNone || coreFiles.IsNone then
    File.SetLastWriteTimeUtc(out, DateTime(2000, 1, 1, 0, 0, 0, DateTimeKind.Utc))

printfn "%s: Marey %s, FSharp.Core %s" out (if mareyFiles.IsSome then "yes" else "no") (if coreFiles.IsSome then "yes" else "no")
