# Tambour

A desktop application for measured data, built on [Marey](https://github.com/Axym-Labs/marey): a
table whose columns carry units, a figure window, and F# scripting underneath both. Every action
taken in the interface is written down as script, so a figure is always reproducible from its
source, and the source runs unchanged in plain `dotnet fsi`.

> Named for the **Marey tambour**, the air capsule Étienne-Jules Marey used to turn a pulse, a
> breath or a muscle twitch into the movement of a recording stylus.

## Status

Early. What builds today is a window that paints a Marey figure; there is nothing to use yet. The
first release is planned to be an editor, a worker process that evaluates scripts, and a figure
pane with Marey's live controls, followed by the table.

## Shape

- **Shell:** Avalonia, in F#. Figures are painted by Marey's Skia painter, the same code that
  writes its PNG and PDF exports.
- **Scripts run out of process.** A worker holds an F# interactive session and sends each
  figure's scene back in Marey's wire format. Cancelling a script ends the worker; a warm spare
  takes its place.
- **The table reaches a script as a typed record**, regenerated when the columns change, so
  plotting a column under the wrong unit is a compile error.
- **Marey is referenced from NuGet at an exact version**, and only through its public API.

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

## Licence

[MPL-2.0](LICENSE). Changes to Tambour's own files stay open; using it, bundling it or building
on it does not put anything else under the same terms.
