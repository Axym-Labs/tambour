# Tambour

A desktop application for measured data, built on [Marey](https://github.com/Axym-Labs/marey): a
table whose columns carry units, a figure window, and F# scripting underneath both. Every action
taken in the interface is written down as script, so a figure is always reproducible from its
source, and the source runs unchanged in plain `dotnet fsi`.

> Named for the **Marey tambour**, the air capsule Étienne-Jules Marey used to turn a pulse, a
> breath or a muscle twitch into the movement of a recording stylus.

## Status

The first cut, script and figure, is built; the table is next, and nothing is released yet. A
window with an F# script on the left and its figure on the right: F5 or Ctrl+Enter runs the
script in a worker, and the figure zooms, pans, reads out under the pointer and carries a live
figure's controls. The editor checks the script as it is typed, with completion, tooltips, the
call's overloads, renaming, and the unit error of a column plotted under the wrong unit. A panel of
GUI actions (title, labels, log axis, domain, theme, legend, colour, facet, fit, a layer) edits
the script rather than the figure, and Ctrl+Z takes an action back like typed text. Scripts and
project folders open and save, and a figure exports as PNG, PDF or SVG through Marey's own
backends. It runs on Linux and Windows, with no .NET installed.

## Shape

- **Shell:** Avalonia, in F#. Figures are painted by Marey's Skia painter, the same code that
  writes its PNG and PDF exports.
- **Scripts run out of process.** A worker holds an F# interactive session and sends each
  figure's scene back in Marey's wire format. Cancelling a script ends the worker; a warm spare
  takes its place.
- **The table reaches a script as a typed record**, regenerated when the columns change, so
  plotting a column under the wrong unit is a compile error.
- **Marey is referenced from NuGet at an exact version**, and only through its public API.

## Using it

```bash
tambour                        # the example script
tambour figure.fsx             # a script
tambour my-project/            # a folder of scripts, with the project.json Tambour keeps there
```

A script may take `open Marey`, `open Marey.UnitsOfMeasure` and the SI unit symbols for granted;
everything else it states itself, and it runs as it is in `dotnet fsi` with Marey referenced.
`#r "nuget: …"` works where a .NET SDK is installed, and says what to install where none is.

## Building

The .NET 10 SDK (`global.json`), and a checkout of [Marey](https://github.com/Axym-Labs/marey)
beside this one. Until Marey 1.0.0 is on nuget.org, Tambour builds against a commit of Marey's
`main`, packed into a local feed; `Directory.Packages.props` pins which.

```bash
tools/marey-feed.sh            # or tools/marey-feed.ps1; packs the pinned commit of ../marey
dotnet build Tambour.slnx
dotnet test Tambour.slnx
dotnet run --project src/Tambour -- --self-check
```

`--self-check` opens a window, paints a figure in it and exits, with 0 if it painted. To move to a
newer Marey, `tools/marey-feed.sh ../marey main` packs it and prints the two lines to pin.

Three more checks run the app as a user meets it, each exiting 0 when it holds:

- `tambour --check-pane [--scaling 2]`: the figure pane against Marey's PNG export on both of its
  paint paths, and a million points from their bytes to the screen;
- `tambour --check-analysis <script>`: the language service, cold, to a script's first error;
- `tambour --check-package`: a script opened, drawn, its unit error shown and taken back, an
  action applied and taken back, and a PNG exported that `renderPng` draws pixel for pixel.

## Packages

`tools/package.sh [version]` writes to `artifacts/package` a self-contained Linux tarball (and an
AppImage of it where `appimagetool` is on the path) and a Windows zip. Nothing is written next to
the executable: settings, caches and the workers' sockets go to the XDG directories on Linux and to
the user's application data on Windows (the sockets to its temporary directory), so the same directory can also be wrapped as a Flatpak or
an AUR package. `tools/no-dotnet.sh` runs a command in a bubblewrap sandbox that hides every .NET
installation:

```bash
tools/no-dotnet.sh path/to/tambour-<version>-linux-x64/tambour --check-package
```

[Third-party notices](THIRD-PARTY-NOTICES.md) list what a package carries and under which licences.

## Licence

[MPL-2.0](LICENSE). Changes to Tambour's own files stay open; using it, bundling it or building
on it does not put anything else under the same terms.
