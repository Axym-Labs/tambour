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
      Height: float
      /// The project's text settings, or null for the worker's own.
      Skia: SkiaSettings }

/// A table's columns, as an Arrow IPC file's bytes, for the worker to hold by name.
[<NoComparison; CustomEquality>]
type TableBytes =
    { Name: string
      Bytes: byte[] }

    override this.Equals(other) =
        match other with
        | :? TableBytes as t -> t.Name = this.Name && System.Linq.Enumerable.SequenceEqual(t.Bytes, this.Bytes)
        | _ -> false

    override this.GetHashCode() = hash (this.Name, this.Bytes.Length)

/// Points, as pairs one after the other: `x0, y0, x1, y1, …`. A picked point off every plot is
/// clamped onto the first plot's rectangle where `Clamp` is set, as an area's corners are, and is
/// no datum otherwise.
type Points = { Values: float[]; Clamp: bool }

/// What the app sends a worker.
[<RequireQualifiedAccess>]
type Request =
    | Init of Init
    | Evaluate of Evaluate
    /// Which of the figures the last run found is on screen, by its place in `Evaluated.Figures`.
    | Select of figure: int
    | Live of LiveInput
    | Export of Export
    /// A table's columns, replacing any of its name.
    | Table of TableBytes
    /// Points in the figure on screen, in its px, to be read back as data.
    | Pick of Points
    /// Data, in each axis's own unit, to be placed in the figure on screen, in its px.
    | Place of Points

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
      Heap: int64
      /// Whether the run evaluated the project's prelude, which it does only when it changed.
      Prelude: bool }

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

/// Bytes for one figure, with the size in px they were laid out at: a scene a resize crossed
/// on its way is drawn at the size it was made for.
[<NoComparison; CustomEquality>]
type Frame =
    { Figure: int
      Width: float
      Height: float
      Bytes: byte[] }

    override this.Equals(other) =
        match other with
        | :? Frame as f ->
            f.Figure = this.Figure
            && f.Width.Equals this.Width
            && f.Height.Equals this.Height
            && System.Linq.Enumerable.SequenceEqual(f.Bytes, this.Bytes)
        | _ -> false

    override this.GetHashCode() = hash (this.Figure, this.Width, this.Height, this.Bytes.Length)

/// Points answered for the figure on screen.
type Located = { Figure: int; Values: float[] }

/// What a worker sends back.
[<RequireQualifiedAccess>]
type Reply =
    | Ready of Ready
    | Evaluated of Evaluated
    /// A whole scene, as Marey's `Wire.encodeWith` writes it.
    | Scene of Frame
    /// A change to the last scene sent, as `Wire.encodePatchWith` writes it.
    | Patch of Frame
    | Controls of ControlsState
    | Readout of ReadoutState
    | Output of Output
    /// The bytes of an export, in the format asked for, at the size asked for.
    | Exported of Frame
    /// The data at each point picked, both NaN for a point off every plot.
    | Picked of Located
    /// Where each datum placed is in the figure, in its px, both NaN for one it cannot place.
    | Placed of Located
    /// A request the worker could not carry out, in a sentence.
    | Failed of message: string

// ---- The language service -------------------------------------------------------------------

/// What the language service is started with: the references and the prelude a script is
/// checked against, as the worker runs it.
type AnalysisInit = { References: string[]; Prelude: string }

/// A script as the editor holds it. `Id` numbers requests, so a reply can be matched to its
/// request and a late one to an older text set aside.
type Source = { Id: int; Text: string; Path: string }

/// A place in a script: lines from 1, columns from 0, as FCS counts them.
type At =
    { Id: int
      Text: string
      Path: string
      Line: int
      Column: int }

/// An entry of the completion list at a place, by the name the list shows.
type Entry =
    { Id: int
      Text: string
      Path: string
      Line: int
      Column: int
      Name: string }

/// A stretch of a script, counted as `At` is.
type Span =
    { StartLine: int
      StartColumn: int
      EndLine: int
      EndColumn: int }

/// One completion: what is shown, what is inserted, what kind of thing it is (`method`,
/// `property`, `field`, `type`, `module`, `namespace`, `value`, `keyword`, …), and the glyph FCS
/// gives it, by the name of its `FSharpGlyph` case (`Class`, `Method`, `EnumMember`, …).
type Completion =
    { Name: string
      Insert: string
      Kind: string
      Glyph: string }

/// A GUI action: its kind, by the name the panel shows (`title`, `x label`, `y label`, `log y`,
/// `y domain`, `theme`, `legend`, `colour by`, `facet`, `fit`, `layer`), and its argument: text
/// for a label, a theme, a placement, a column or a geom, and the ends of a domain.
type ActionSpec =
    { Kind: string
      Text: string
      Lo: float
      Hi: float }

/// A figure in a script, by the name the worker reported (`it` for the last expression).
type FigureIn =
    { Id: int
      Text: string
      Path: string
      Figure: string }

/// An action to do to a figure.
type ActionOn =
    { Id: int
      Text: string
      Path: string
      Figure: string
      Action: ActionSpec }

[<RequireQualifiedAccess>]
type AnalysisRequest =
    | Start of AnalysisInit
    | Check of Source
    | Complete of At
    | Hover of At
    /// The signature and documentation of a completion list's entry, answered as a hover.
    | Describe of Entry
    | Signature of At
    | Uses of At
    /// What each action would come to for this figure, and the columns its rows have.
    | Actions of FigureIn
    | Act of ActionOn
    /// What a figure's plot block plots: its rows and its x and y fields.
    | Plotted of FigureIn

/// A stretch of one line the compiler classifies, by the semantic token type the language server
/// protocol names (`namespace`, `type`, `class`, `struct`, `enum`, `interface`, `typeParameter`,
/// `parameter`, `variable`, `property`, `enumMember`, `event`, `function`, `method`, `operator`,
/// `keyword`), and whether what it names is mutable. Counted as `At` is; no two overlap.
type Token =
    { Line: int
      StartColumn: int
      EndColumn: int
      Kind: string
      Mutable: bool }

/// What one check of a script found: its problems, and its tokens in order.
type Checked =
    { Id: int
      Diagnostics: Diagnostic[]
      Tokens: Token[] }

type Completions = { Id: int; Items: Completion[] }

/// The tooltip at a place: the signature, as text, and the documentation of what it names; each
/// empty where there is none.
type Hovered = { Id: int; Text: string; Doc: string }

/// The overloads of the call the place is in, each as one line, the one most likely meant, and
/// which of its parameters the place is at.
type Signatures =
    { Id: int
      Methods: string[]
      Active: int
      Parameter: int }

/// Where the symbol at a place is used in the script, its own name, and whether it can be renamed
/// there (a symbol the script defines).
type Used =
    { Id: int
      Name: string
      Spans: Span[]
      Renamable: bool }

/// What an action came to: `structural` (done in a shape the app recognises), `fallback` (done
/// after an expression it does not) or `refused`, and a sentence saying so.
type ActionOutcome = { Kind: string; Outcome: string; Note: string }

type ActionsFor =
    { Id: int
      Outcomes: ActionOutcome[]
      Columns: string[] }

/// The script after an action, and what the action came to.
type Acted =
    { Id: int
      Text: string
      Outcome: ActionOutcome }

/// A figure's rows and the fields its x and y read, all empty where it is not a plot block.
type PlottedFields =
    { Id: int
      Rows: string
      X: string
      Y: string }

[<RequireQualifiedAccess>]
type AnalysisReply =
    /// Started, in this many milliseconds from the process starting.
    | Started of elapsed: float
    | Checked of Checked
    | Completions of Completions
    | Hovered of Hovered
    | Signatures of Signatures
    | Used of Used
    | Actions of ActionsFor
    | Acted of Acted
    | Plotted of PlottedFields
    | Failed of id: int * message: string
