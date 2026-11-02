/// Telling the OS that Tambour opens `.tambour` files, for this user only: on Linux at every start,
/// on Windows when asked, from Preferences. On Linux: the MIME type (by its extension and by the
/// `mimetype` entry a bundle begins with), a `.desktop` entry naming the app, the app's icon for
/// both, and a thumbnailer that takes out the figure a bundle keeps; all under `$XDG_DATA_HOME`. The
/// desktop entry is also how a Wayland shell finds a window's icon. On Windows: a file class under
/// `HKCU\Software\Classes` with the app's icon. Windows draws no thumbnail of its own for one: that
/// would take a shell extension.
module Tambour.Association

open System
open System.Diagnostics
open System.IO
open System.Runtime.InteropServices

/// The app as the OS is to start it: the AppImage where it runs from one, else its executable.
let executable () =
    match Environment.GetEnvironmentVariable "APPIMAGE" with
    | path when not (String.IsNullOrEmpty path) && File.Exists path -> path
    | _ -> Environment.ProcessPath

/// `$XDG_DATA_HOME`, `~/.local/share` where that is unset.
let dataDirectory () =
    match Environment.GetEnvironmentVariable "XDG_DATA_HOME" with
    | dir when not (String.IsNullOrEmpty dir) -> dir
    | _ -> Path.Combine(Environment.GetFolderPath Environment.SpecialFolder.UserProfile, ".local", "share")

let private mimeXml =
    $"""<?xml version="1.0" encoding="UTF-8"?>
<mime-info xmlns="http://www.freedesktop.org/standards/shared-mime-info">
  <mime-type type="{Bundle.MimeType}">
    <comment>Tambour workspace</comment>
    <sub-class-of type="application/zip"/>
    <glob pattern="*{Bundle.Extension}"/>
    <magic priority="60">
      <match type="string" offset="30" value="mimetype{Bundle.MimeType}"/>
    </magic>
    <icon name="tambour"/>
  </mime-type>
</mime-info>
"""

/// A path as a `.desktop` file's Exec takes it: quoted, its `"`, `` ` ``, `$` and `\` escaped.
let private quoted (path: string) =
    let escaped = path.Replace("\\", "\\\\").Replace("\"", "\\\"").Replace("`", "\\`").Replace("$", "\\$")
    $"\"{escaped}\""

let private desktopEntry (exe: string) =
    $"""[Desktop Entry]
Type=Application
Name=Tambour
Comment=Measured data, figures and F# scripting
Exec={quoted exe} %%f
Icon=tambour
MimeType={Bundle.MimeType};
Categories=Science;Education;Development;
Terminal=false
StartupWMClass=tambour
"""

/// The thumbnailer: the figure a bundle keeps, taken out of it with whichever of unzip, bsdtar or
/// Python the system has. Not by the app itself: GNOME runs a thumbnailer in a sandbox that sees
/// only /usr, where an app in the home directory, an AppImage or /opt is not.
let private thumbnailer =
    let take tool = $"{tool} > \"$2\" 2>/dev/null && [ -s \"$2\" ]"

    let script =
        String.concat
            " || "
            [ take $"unzip -p \"$1\" {Bundle.Thumbnail}"
              take $"bsdtar -xOf \"$1\" {Bundle.Thumbnail}"
              take $"python3 -I -c \"import sys, zipfile; sys.stdout.buffer.write(zipfile.ZipFile(sys.argv[1]).read(sys.argv[2]))\" \"$1\" {Bundle.Thumbnail}" ]

    $"""[Thumbnailer Entry]
TryExec=sh
Exec=sh -c '{script}' tambour %%i %%o
MimeType={Bundle.MimeType};
"""

/// Runs a desktop database's tool on what was written, where it is installed; nothing where not.
let private refresh (tool: string) (dir: string) =
    try
        use p = Process.Start(ProcessStartInfo(tool, [| dir |], UseShellExecute = false, CreateNoWindow = true))
        p.WaitForExit 10000 |> ignore
    with _ ->
        ()

/// What `linux` writes, from the data directory.
let linuxFiles =
    [ "mime/packages/tambour.xml"
      "applications/tambour.desktop"
      "thumbnailers/tambour.thumbnailer"
      "icons/hicolor/256x256/apps/tambour.png"
      "icons/hicolor/256x256/mimetypes/application-x-tambour.png" ]

/// Writes the association under a data directory, the icon a 256 px PNG, each file only where it
/// is missing or differs; with `databases`, the MIME and desktop databases are brought up to date
/// where their files changed. Whether anything was written.
let linux (data: string) (exe: string) (icon: byte[]) (databases: bool) =
    let wrote (relative: string) (bytes: byte[]) =
        let path = Path.Combine(data, relative)

        if File.Exists path && File.ReadAllBytes path = bytes then
            false
        else
            Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
            File.WriteAllBytes(path, bytes)
            true

    let utf8 (s: string) = Text.Encoding.UTF8.GetBytes s

    match linuxFiles with
    | [ mime; desktop; thumb; appIcon; typeIcon ] ->
        let types = wrote mime (utf8 mimeXml)
        let entry = wrote desktop (utf8 (desktopEntry exe))
        // Each written, whether or not the one before was.
        let others = [ wrote thumb (utf8 thumbnailer); wrote appIcon icon; wrote typeIcon icon ]

        if databases && types then refresh "update-mime-database" (Path.Combine(data, "mime"))
        if databases && entry then refresh "update-desktop-database" (Path.Combine(data, "applications"))
        types || entry || List.contains true others
    | _ -> false

[<DllImport("shell32.dll")>]
extern void SHChangeNotify(int eventId, uint32 flags, nativeint item1, nativeint item2)

/// Writes the file class under the user's own classes; Explorer is told the associations changed.
let windows (exe: string) =
    if OperatingSystem.IsWindows() then
        use classes = Microsoft.Win32.Registry.CurrentUser.CreateSubKey @"Software\Classes"

        do
            use ext = classes.CreateSubKey Bundle.Extension
            ext.SetValue("", "Tambour.Workspace")
            ext.SetValue("Content Type", Bundle.MimeType)

        use kind = classes.CreateSubKey "Tambour.Workspace"
        kind.SetValue("", "Tambour workspace")

        do
            use icon = kind.CreateSubKey "DefaultIcon"
            icon.SetValue("", $"\"{exe}\",0")

        use command = kind.CreateSubKey @"shell\open\command"
        command.SetValue("", $"\"{exe}\" \"%%1\"")
        // SHCNE_ASSOCCHANGED, with no items.
        SHChangeNotify(0x08000000, 0u, 0n, 0n)

/// Registers the association for this OS: what it came to, as Preferences shows it.
let register (icon: byte[]) : Result<string, string> =
    try
        let exe = executable ()

        if OperatingSystem.IsWindows() then
            windows exe
            Ok "Explorer opens .tambour files with Tambour."
        elif OperatingSystem.IsLinux() then
            linux (dataDirectory ()) exe icon true |> ignore
            Ok "The file manager opens .tambour files with Tambour, and shows their figures."
        else
            Error "not on this OS"
    with e ->
        Error e.Message
