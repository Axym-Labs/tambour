/// A workspace as one file of Tambour's own, a `.tambour`: a zip of the workspace's directory whose
/// first entry, stored, is `mimetype`, so that a file manager knows it by its content. It is opened
/// by unpacking it into a directory of the user's cache, which from then on is a workspace
/// directory like any other, and it is written back from there: to a temporary file beside it,
/// then moved over it, so that a save stopped at any point leaves the file as it was or as it is
/// after, never neither.
///
/// The cache holds only what has been saved, as a workspace directory does. One left by a run that
/// ended without closing it is written back at the next open if its file has not changed since,
/// and dropped if it has. A lock file in it keeps a second process from opening the same file.
module Tambour.Bundle

open System
open System.Collections.Generic
open System.IO
open System.IO.Compression
open System.Security.Cryptography
open System.Text
open System.Text.Json

[<Literal>]
let Extension = ".tambour"

[<Literal>]
let MimeType = "application/x-tambour"

/// The figure last shown, kept on Save for a thumbnail.
[<Literal>]
let Thumbnail = "thumbnail.png"

[<Literal>]
let private MimeEntry = "mimetype"

[<Literal>]
let private LockFile = ".lock"

[<Literal>]
let private StateFile = ".bundle.json"

/// Whether a path names a `.tambour`.
let isBundle (path: string) =
    path.EndsWith(Extension, StringComparison.OrdinalIgnoreCase)

/// Where bundles are unpacked: `open/` in the user's cache unless set otherwise, as the tests do.
let mutable Root: string = null

let private root () =
    match Root with
    | null -> Path.Combine(Paths.cacheDirectory (), "open")
    | dir -> dir

/// The cache directory of a bundle: its name, and a hash of its full path so two of one name do not
/// meet.
let cacheOf (file: string) =
    let full = Path.GetFullPath file
    let key = if OperatingSystem.IsWindows() then full.ToUpperInvariant() else full
    let hash = Convert.ToHexString(SHA256.HashData(Encoding.UTF8.GetBytes key)).Substring(0, 12).ToLowerInvariant()
    Path.Combine(root (), $"{Path.GetFileNameWithoutExtension full}-{hash}")

/// What the cache knew of its bundle when it last unpacked or wrote it: the file's time and length
/// then, and the stamp of the cache's own files. Public, as System.Text.Json reads no private
/// record.
type State =
    { Bundle: string
      Written: int64
      Length: int64
      Stamp: string }

let private json = JsonSerializerOptions(WriteIndented = true)

/// The files a bundle holds, by their path from its directory with `/` between: everything but
/// the cache's own files and the prelude, which is written again from the tables at every open.
let private contents (dir: string) =
    Directory.GetFiles(dir, "*", SearchOption.AllDirectories)
    |> Array.map (fun f -> Path.GetRelativePath(dir, f).Replace('\\', '/'))
    |> Array.filter (fun f -> f <> LockFile && f <> StateFile && f <> Tambour.Data.Prelude.FileName)
    |> Array.sort

/// The cache's files as they stand: each one's path, length and time of writing.
let private stamp (dir: string) =
    let parts =
        [ for f in contents dir do
              let info = FileInfo(Path.Combine(dir, f))
              $"{f}\t{info.Length}\t{info.LastWriteTimeUtc.Ticks}" ]

    Convert.ToHexString(SHA256.HashData(Encoding.UTF8.GetBytes(String.Join("\n", parts))))

let private seen (file: string) =
    if File.Exists file then
        let info = FileInfo file
        info.LastWriteTimeUtc.Ticks, info.Length
    else
        0L, -1L

let private readState (dir: string) =
    let path = Path.Combine(dir, StateFile)

    try
        if File.Exists path then
            match JsonSerializer.Deserialize<State>(File.ReadAllText path, json) with
            | s when isNull (box s) -> None
            | s -> Some s
        else
            None
    with _ ->
        None

let private writeState (dir: string) (file: string) =
    let written, length = seen file

    let state =
        { Bundle = Path.GetFullPath file
          Written = written
          Length = length
          Stamp = stamp dir }

    File.WriteAllText(Path.Combine(dir, StateFile), JsonSerializer.Serialize(state, json))

/// Writes a workspace's directory as a bundle: to a temporary file beside it, then moved over it.
let pack (dir: string) (file: string) =
    let file = Path.GetFullPath file
    let temporary = Path.Combine(Path.GetDirectoryName file, $".{Path.GetFileName file}.{Guid.NewGuid():N}.tmp")

    try
        do
            use stream = new FileStream(temporary, FileMode.CreateNew, FileAccess.Write)
            use zip = new ZipArchive(stream, ZipArchiveMode.Create)

            do
                use entry = zip.CreateEntry(MimeEntry, CompressionLevel.NoCompression).Open()
                entry.Write(Encoding.ASCII.GetBytes MimeType)

            for name in contents dir do
                zip.CreateEntryFromFile(Path.Combine(dir, name), name, CompressionLevel.Optimal) |> ignore

        File.Move(temporary, file, true)
    with _ ->
        try
            File.Delete temporary
        with _ ->
            ()

        reraise ()

/// Whether a file is a bundle by its content: its first entry `mimetype`, naming Tambour's.
let private check (zip: ZipArchive) =
    match Seq.tryHead zip.Entries with
    | Some first when first.FullName = MimeEntry ->
        use reader = new StreamReader(first.Open(), Encoding.ASCII)
        reader.ReadToEnd() = MimeType
    | _ -> false

/// Unpacks a bundle into a directory, which is emptied first but for its lock. An entry that would
/// land outside it is refused.
let private unpackInto (file: string) (dir: string) =
    for f in Directory.GetFiles dir do
        if Path.GetFileName f <> LockFile then File.Delete f

    for d in Directory.GetDirectories dir do
        Directory.Delete(d, true)

    if File.Exists file then
        use zip = ZipFile.OpenRead file

        if not (check zip) then
            failwith "not a Tambour file"

        let inside = Path.GetFullPath dir + string Path.DirectorySeparatorChar

        for entry in zip.Entries do
            if entry.FullName <> MimeEntry && not (entry.FullName.EndsWith "/") then
                let target = Path.GetFullPath(Path.Combine(dir, entry.FullName))

                if not (target.StartsWith(inside, StringComparison.Ordinal)) then
                    failwith $"an entry outside the workspace: {entry.FullName}"

                Directory.CreateDirectory(Path.GetDirectoryName target) |> ignore
                entry.ExtractToFile(target, true)

/// The bundles this process has open: each one's cache and the lock held on it.
let private held = Dictionary<string, string * FileStream>()

/// What opening a bundle came to: its cache directory, and whether what a run before had saved
/// there was written back into it first.
type Opened = { Directory: string; WrittenBack: bool }

/// Opens a bundle: unpacked into its cache, or the cache this process already has of it. A bundle
/// that does not exist yet opens as an empty directory, which its first `sync` writes out.
let openBundle (file: string) : Result<Opened, string> =
    let file = Path.GetFullPath file

    match held.TryGetValue file with
    | true, (dir, _) -> Ok { Directory = dir; WrittenBack = false }
    | _ ->
        let dir = cacheOf file
        Directory.CreateDirectory dir |> ignore

        let lock =
            try
                Some(new FileStream(Path.Combine(dir, LockFile), FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.None))
            with :? IOException ->
                None

        match lock with
        | None -> Error $"{Path.GetFileName file} is open in another Tambour"
        | Some lock ->
            try
                // Left by a run that did not close it: what it saved is written back, if no one
                // has written the bundle since.
                let writtenBack =
                    match readState dir with
                    | Some s when s.Bundle = file && (s.Written, s.Length) = seen file && s.Stamp <> stamp dir ->
                        pack dir file
                        true
                    | _ -> false

                unpackInto file dir
                writeState dir file
                held.[file] <- (dir, lock)
                Ok { Directory = dir; WrittenBack = writtenBack }
            with e ->
                lock.Dispose()
                Error e.Message

/// Writes a bundle back from its cache if what is saved there changed since; whether it did.
let sync (file: string) : bool =
    let file = Path.GetFullPath file

    match held.TryGetValue file with
    | true, (dir, _) ->
        match readState dir with
        | Some s when s.Stamp = stamp dir && File.Exists file -> false
        | _ ->
            pack dir file
            writeState dir file
            true
    | _ -> false

/// Closes a bundle: written back if it changed, its lock given up and its cache removed.
let close (file: string) =
    let file = Path.GetFullPath file

    match held.TryGetValue file with
    | true, (dir, lock) ->
        sync file |> ignore
        held.Remove file |> ignore
        lock.Dispose()

        try
            Directory.Delete(dir, true)
        with _ ->
            ()
    | _ -> ()

/// The bundle a directory is the cache of, if this process has it open.
let ofDirectory (dir: string) : string option =
    let dir = Path.GetFullPath dir

    held
    |> Seq.tryPick (fun kv -> if Path.GetFullPath(fst kv.Value) = dir then Some kv.Key else None)

/// A bundle's thumbnail, read from the file without unpacking it.
let thumbnail (file: string) : byte[] option =
    try
        use zip = ZipFile.OpenRead file

        match zip.GetEntry Thumbnail with
        | null -> None
        | entry ->
            use stream = entry.Open()
            use copy = new MemoryStream()
            stream.CopyTo copy
            Some(copy.ToArray())
    with _ ->
        None

/// Writes a workspace's directory out as a plain folder: its files but the thumbnail, and the
/// prelude, so the scripts read and run outside Tambour too.
let export (dir: string) (target: string) =
    for name in Array.append (contents dir |> Array.filter ((<>) Thumbnail)) [| Tambour.Data.Prelude.FileName |] do
        let source = Path.Combine(dir, name)

        if File.Exists source then
            let destination = Path.Combine(target, name)
            Directory.CreateDirectory(Path.GetDirectoryName destination) |> ignore
            File.Copy(source, destination, true)

/// Lets go of a bundle as a run that ends without closing it does: its lock given up, its cache
/// left as it is. For the tests of what the next open makes of that.
let abandon (file: string) =
    let file = Path.GetFullPath file

    match held.TryGetValue file with
    | true, (_, lock) ->
        held.Remove file |> ignore
        lock.Dispose()
    | _ -> ()
