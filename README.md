# Tambour

A desktop application for measured data, built on [Marey](https://github.com/Axym-Labs/marey): a
table whose columns carry units, a figure window, and F# scripting underneath both. Every action
taken in the interface is written down as script, so a figure is always reproducible from its
source, and the source runs unchanged in plain `dotnet fsi`.

> Named for the **Marey tambour**, the air capsule Étienne-Jules Marey used to turn a pulse, a
> breath or a muscle twitch into the movement of a recording stylus.

## Status

Planning. There is nothing to build or run yet. The first release is planned to be an editor, a
worker process that evaluates scripts, and a figure pane with Marey's live controls, followed by
the table.

## Shape

- **Shell:** Avalonia, in F#. Figures are painted by Marey's Skia painter, the same code that
  writes its PNG and PDF exports.
- **Scripts run out of process.** A worker holds an F# interactive session and sends each
  figure's scene back in Marey's wire format. Cancelling a script ends the worker; a warm spare
  takes its place.
- **The table reaches a script as a typed record**, regenerated when the columns change, so
  plotting a column under the wrong unit is a compile error.
- **Marey is referenced from NuGet at an exact version**, and only through its public API.

## Requirements

The .NET 10 SDK (`global.json`).
