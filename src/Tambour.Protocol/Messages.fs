namespace Tambour.Protocol

/// The text settings a worker measures and exports with, Marey's `SkiaOptions` in the part a
/// project states.
type SkiaSettings =
    { /// Pixels per millimetre.
      Dpi: float
      /// Font files in priority order. Empty resolves the theme's families against the machine.
      FontFiles: string[] }

/// What a worker is started with, before any script.
type Init =
    { /// Assemblies the session references as `-r:`, beyond Marey's own.
      References: string[]
      /// Evaluated once, before the first script: the opens a script may take for granted.
      Prelude: string
      Skia: SkiaSettings }

/// A script to run.
type Evaluate =
    { Text: string
      /// Where the script is on disk, for `__SOURCE_DIRECTORY__`, `#load` and messages. Null for
      /// one never saved.
      Path: string }

/// A reader's message to the figure on screen, in the form Marey's `LiveMsg.toWire` gives it.
type LiveInput = { Kind: string; Args: float[] }

/// The figure on screen written out at a size, through Marey's own backends.
type Export =
    { /// `png`, `pdf` or `svg`.
      Format: string
      Width: float
      Height: float }

/// What the app sends a worker.
[<RequireQualifiedAccess>]
type Request =
    | Init of Init
    | Evaluate of Evaluate
    /// Which of the figures the last run found is on screen, by its place in `Evaluated.Figures`.
    | Select of figure: int
    | Live of LiveInput
    | Export of Export

/// A worker started and ran its prelude.
type Ready =
    { /// Milliseconds from the process starting to the prelude evaluated.
      Elapsed: float
      /// The managed heap afterwards, in bytes: the baseline a worker is recycled against.
      Heap: int64 }

/// A compiler message about the script, with its range in the script's own lines and columns
/// (lines from 1, columns from 0, as FCS gives them).
type Diagnostic =
    { /// `error`, `warning` or `info`.
      Severity: string
      Code: string
      Message: string
      StartLine: int
      StartColumn: int
      EndLine: int
      EndColumn: int }

/// A figure a run left bound: its name in the script, and how many controls it has (none for a
/// static figure).
type FigureInfo = { Name: string; Controls: int }

/// Milliseconds spent on each part of a run.
type Timings =
    { Evaluate: float
      Layout: float
      Encode: float }

/// What a run came to.
type Evaluated =
    { /// False when the script did not compile or threw. The worker then keeps the figures it
      /// had, which `Figures` lists.
      Ok: bool
      /// The figures the worker holds, in the order the script bound them, with `it` last.
      Figures: FigureInfo[]
      /// The figure on screen, or -1 for none.
      Selected: int
      Diagnostics: Diagnostic[]
      /// The exception's message when the script threw, otherwise null.
      Exception: string
      Timings: Timings
      /// The managed heap after the run, in bytes.
      Heap: int64 }

/// One control of the figure on screen. `Kind` is `slider`, `select`, `checkbox` or `time`, and
/// says which of the other fields mean something: a slider's and a time control's range and
/// step, a time control's samples and loop, a select's options.
type ControlInfo =
    { Label: string
      Kind: string
      Lo: float
      Hi: float
      Step: float
      Samples: int
      Loop: bool
      Options: string[]
      Init: float
      Value: float
      /// The value as the reader reads it (`300 K`, `on`, an option's name), formatted in the
      /// worker, where the control's formatting lives.
      Shown: string }

/// The controls of the figure on screen, sent with each new figure and after each change.
type ControlsState = { Figure: int; Controls: ControlInfo[] }

/// What the pointer reads, and where: the plot rectangle it is over, in the figure's px, and
/// whether the readout belongs in its top-right corner rather than its top-left.
type ReadoutState =
    { Figure: int
      /// False when the pointer is off every plot; nothing else means anything then.
      Visible: bool
      X0: float
      Y0: float
      X1: float
      Y1: float
      Right: bool
      Lines: string[] }

/// Text the script wrote, as it wrote it.
type Output =
    { /// `out` or `err`.
      Stream: string
      Text: string }

/// What a worker sends back.
[<RequireQualifiedAccess>]
type Reply =
    | Ready of Ready
    | Evaluated of Evaluated
    /// A whole scene, as Marey's `Wire.encodeWith` writes it.
    | Scene of figure: int * bytes: byte[]
    /// A change to the last scene sent, as `Wire.encodePatchWith` writes it.
    | Patch of figure: int * bytes: byte[]
    | Controls of ControlsState
    | Readout of ReadoutState
    | Output of Output
    /// The bytes of an export, in the format asked for.
    | Exported of figure: int * bytes: byte[]
    /// A request the worker could not carry out, in a sentence.
    | Failed of message: string
