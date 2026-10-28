/// Where the app writes. Never next to the executable, which a package may install read-only:
/// the XDG directories on Linux, the user's application data on Windows (sockets: its temporary
/// directory).
module Tambour.Paths

open System
open System.IO

/// Unix sockets name a path of at most 108 bytes, its terminator included.
[<Literal>]
let private MaxSocketPath = 107

let private privateDirectory (path: string) =
    if OperatingSystem.IsWindows() then
        Directory.CreateDirectory path |> ignore
    else
        Directory.CreateDirectory(path, UnixFileMode.UserRead ||| UnixFileMode.UserWrite ||| UnixFileMode.UserExecute)
        |> ignore

    path

/// The directory for this user's sockets: `$XDG_RUNTIME_DIR/tambour`, or a directory of the
/// user's own under the temporary one where that is unset; `%TEMP%\Tambour\run` on Windows. Not
/// under `%LOCALAPPDATA%`: a packaged (MSIX) process, or one started by a packaged app, has its
/// writes there redirected into the package's own directory, where a socket binds but cannot be
/// connected to by its path (WSAEINVAL). The temporary directory is not redirected.
let runtimeDirectory () : string =
    if OperatingSystem.IsWindows() then
        privateDirectory (Path.Combine(Path.GetTempPath(), "Tambour", "run"))
    else
        match Environment.GetEnvironmentVariable "XDG_RUNTIME_DIR" with
        | dir when not (String.IsNullOrEmpty dir) && Directory.Exists dir -> privateDirectory (Path.Combine(dir, "tambour"))
        | _ -> privateDirectory (Path.Combine(Path.GetTempPath(), $"tambour-{Environment.UserName}"))

/// A fresh path for one worker's socket, short enough for the socket to be bound.
let socketPath (name: string) : string =
    let path = Path.Combine(runtimeDirectory (), name + ".sock")

    if Text.Encoding.UTF8.GetByteCount path <= MaxSocketPath then
        path
    else
        Path.Combine(Path.GetTempPath(), name + ".sock")

/// The directory for this user's settings: `$XDG_CONFIG_HOME/tambour` (`~/.config/tambour` where
/// that is unset), `%APPDATA%\Tambour` on Windows. Made when a setting is first written.
let configDirectory () : string =
    if OperatingSystem.IsWindows() then
        Path.Combine(Environment.GetFolderPath Environment.SpecialFolder.ApplicationData, "Tambour")
    else
        match Environment.GetEnvironmentVariable "XDG_CONFIG_HOME" with
        | dir when not (String.IsNullOrEmpty dir) -> Path.Combine(dir, "tambour")
        | _ -> Path.Combine(Environment.GetFolderPath Environment.SpecialFolder.UserProfile, ".config", "tambour")
