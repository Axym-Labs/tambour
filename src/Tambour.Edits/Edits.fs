/// GUI actions as edits to a figure's script. The figure's binding is found in FCS's untyped tree
/// by the name the worker reported, and text is spliced only at the ranges the tree names, so every
/// other byte, comments and layout among them, stays as it was. Nothing is pretty-printed.
module Tambour.Edits.Edits

open System
open System.Globalization
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

/// What a GUI action asks of a figure.
type Action =
    | Title of string
    | XLabel of string
    | YLabel of string
    | YLog
    | YDomain of lo: float * hi: float
    /// A theme by its name in `Theme`: `publication`, `modern`, `presentation`, …
    | ThemeIs of string
    /// A legend placement by its case: `LegendTopLeft`, …
    | Legend of string
    /// Colour by a column of the plotted rows.
    | ColorBy of string
    /// One panel per value of a column.
    | Facet of string
    | Fit
    /// A layer by its geom: `GeomLine`, …
    | AddLayer of string

/// What an action came to.
type Outcome =
    /// Done in a shape the app recognises; what was done.
    | Structural of string
    /// Done after an expression the app does not recognise: a stage at the expression's own column,
    /// which applies to the whole of it. Correct, but it reads ambiguously, so the app says so.
    | Fallback of string
    /// Not done, and why.
    | Refused of string

// ---- Parsing ------------------------------------------------------------------------------------

let private checker = lazy (FSharpChecker.Create())

let private parse (path: string) (source: string) =
    let options =
        { FSharpParsingOptions.Default with
            SourceFiles = [| path |]
            IsInteractive = true
            LangVersionText = "latest" }

    let result = checker.Value.ParseFile(path, SourceText.ofString source, options) |> Async.RunSynchronously
    result.ParseTree

// ---- Positions ----------------------------------------------------------------------------------

let private lineStarts (source: string) =
    let starts = ResizeArray [ 0 ]
    source |> String.iteri (fun i c -> if c = '\n' then starts.Add(i + 1))
    starts.ToArray()

let private offset (starts: int[]) (p: pos) = starts.[p.Line - 1] + p.Column

let private indentOf (source: string) (starts: int[]) (line: int) =
    let s = starts.[line - 1]
    let mutable i = s

    while i < source.Length && source.[i] = ' ' do
        i <- i + 1

    i - s

/// Where a line's text ends, before its newline (and a carriage return before that).
let private endOfLine (source: string) (starts: int[]) (line: int) =
    let e = if line < starts.Length then starts.[line] - 1 else source.Length
    if e > 0 && e <= source.Length && source.[e - 1] = '\r' then e - 1 else e

let private newline (source: string) = if source.Contains "\r\n" then "\r\n" else "\n"

// ---- Shapes -------------------------------------------------------------------------------------

let rec private headName (e: SynExpr) =
    match e with
    | SynExpr.Ident id -> Some id.idText
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) -> Some(ids |> List.map (fun i -> i.idText) |> String.concat ".")
    | SynExpr.App(funcExpr = f; isInfix = false) -> headName f
    | SynExpr.TypeApp(expr = e) -> headName e
    | SynExpr.Paren(expr = e) -> headName e
    | _ -> None

/// `a |> f |> g` as `a` and `[ f; g ]`.
let rec private unpipe (e: SynExpr) =
    match e with
    | SynExpr.App(
        isInfix = false
        funcExpr = SynExpr.App(isInfix = true; funcExpr = SynExpr.LongIdent(longDotId = SynLongIdent(id = [ op ])); argExpr = a)
        argExpr = f) when op.idText = "op_PipeRight" ->
        let head, stages = unpipe a
        head, stages @ [ f ]
    | _ -> e, []

/// The operations of `plot data { … }`, in order.
let private blockOps (head: SynExpr) =
    match head with
    | SynExpr.App(funcExpr = SynExpr.App(funcExpr = SynExpr.Ident p); argExpr = SynExpr.ComputationExpr(expr = body)) when p.idText = "plot" ->
        let rec items e =
            match e with
            | SynExpr.Sequential(expr1 = a; expr2 = b) -> items a @ items b
            | e -> [ e ]

        Some(items body)
    | _ -> None

/// A figure's expression in the script.
type private Site =
    { /// The line its declaration starts on: the `let`'s, or the expression's own for `it`.
      DeclarationLine: int
      Expr: SynExpr
      Head: SynExpr
      Stages: SynExpr list
      Ops: SynExpr list option }

let private siteOf (declarationLine: int) (e: SynExpr) =
    let head, stages = unpipe e

    { DeclarationLine = declarationLine
      Expr = e
      Head = head
      Stages = stages
      Ops = blockOps head }

/// The figure bound to `name` at the top of the script, the last such binding; for `it`, the last
/// expression the script ends in.
let private find (name: string) (tree: ParsedInput) =
    match tree with
    | ParsedInput.ImplFile(ParsedImplFileInput(contents = modules)) ->
        let decls = [ for SynModuleOrNamespace(decls = decls) in modules do yield! decls ]

        if name = "it" then
            decls
            |> List.tryLast
            |> Option.bind (function
                | SynModuleDecl.Expr(expr = e) -> Some(siteOf e.Range.StartLine e)
                | _ -> None)
        else
            [ for d in decls do
                  match d with
                  | SynModuleDecl.Let(bindings = bindings) ->
                      for b in bindings do
                          match b with
                          | SynBinding(headPat = SynPat.Named(ident = SynIdent(id, _)); expr = e) when id.idText = name ->
                              yield siteOf b.RangeOfBindingWithRhs.StartLine e
                          | SynBinding(headPat = SynPat.Typed(pat = SynPat.Named(ident = SynIdent(id, _))); expr = e) when id.idText = name ->
                              yield siteOf b.RangeOfBindingWithRhs.StartLine e
                          | _ -> ()
                  | _ -> () ]
            |> List.tryLast
    | _ -> None

// ---- Edits ----------------------------------------------------------------------------------------

let private quote (s: string) =
    "\"" + s.Replace("\\", "\\\\").Replace("\"", "\\\"").Replace("\n", "\\n").Replace("\r", "\\r") + "\""

let private number (x: float) =
    let s = x.ToString("R", CultureInfo.InvariantCulture)
    let s = if s.Contains "." || s.Contains "E" || s.Contains "N" || s.Contains "I" then s else s + ".0"
    if x < 0.0 then "(" + s + ")" else s

/// An identifier as F# accepts it after `r.`: a name that needs them is quoted in double backticks.
let private member' (name: string) =
    if name |> Seq.forall (fun c -> Char.IsLetterOrDigit c || c = '_') && not (Char.IsDigit name.[0]) then name
    else "``" + name + "``"

/// A `|> Chart.with…` stage: its function's name and its text.
let private stageFor action =
    match action with
    | Title t -> Some("Chart.withTitle", "Chart.withTitle " + quote t)
    | XLabel t -> Some("Chart.withXLabel", "Chart.withXLabel " + quote t)
    | YLabel t -> Some("Chart.withYLabel", "Chart.withYLabel " + quote t)
    | YLog -> Some("Chart.withYLog", "Chart.withYLog")
    | YDomain(lo, hi) -> Some("Chart.withYDomain", $"Chart.withYDomain {number lo} {number hi}")
    | ThemeIs t -> Some("Chart.withTheme", "Chart.withTheme Theme." + t)
    | Legend p -> Some("Chart.withLegend", "Chart.withLegend " + p)
    | ColorBy _
    | Facet _
    | Fit
    | AddLayer _ -> None

/// An operation inside `plot { }`: its name, its text, and whether a second replaces the first.
let private opFor action =
    match action with
    | ColorBy c -> Some("color", $"color (fun r -> r.{member' c})", true)
    | Facet c -> Some("facetWrap", $"facetWrap (fun r -> r.{member' c})", true)
    | Fit -> Some("fit", "fit", true)
    | AddLayer g -> Some("layer", "layer " + g, false)
    | ThemeIs t -> Some("theme", "theme Theme." + t, true)
    | _ -> None

/// Whether doing the action again changes nothing.
let private settles action =
    match action with
    | AddLayer _ -> false
    | _ -> true

let private splice (source: string) (a: int) (b: int) (text: string) = source.Substring(0, a) + text + source.Substring b

/// The action applied to the figure bound to `figure` (or `it`): the new script and what was done.
/// A refused action returns the script unchanged.
let apply (path: string) (figure: string) (action: Action) (source: string) : string * Outcome =
    let path = if String.IsNullOrEmpty path then "script.fsx" else path
    let tree = parse path source
    let starts = lineStarts source
    let nl = newline source

    match find figure tree with
    | None -> source, Refused $"the script binds no figure named {figure}"
    | Some site ->
        let replace (r: range) text = splice source (offset starts r.Start) (offset starts r.End) text

        let insertLineAfter (line: int) (indent: int) text =
            let at = endOfLine source starts line
            splice source at at (nl + String(' ', indent) + text)

        // An operation the block already has, of an action the block can hold, is edited in the
        // block: a figure states its theme once.
        let inBlock =
            match site.Ops, opFor action with
            | Some ops, Some(name, text, replaces) ->
                match ops |> List.tryFind (fun o -> headName o = Some name), stageFor action with
                | Some o, _ when replaces ->
                    if action = Fit then Some(source, Structural "the figure already fits")
                    else Some(replace o.Range text, Structural $"replaced {name} in the plot block")
                | None, Some _ -> None // no theme in the block: a stage states it
                | _ ->
                    let last = List.last ops
                    Some(insertLineAfter last.Range.EndLine (indentOf source starts last.Range.StartLine) text, Structural $"added {name} to the plot block")
            | _ -> None

        match inBlock with
        | Some result -> result
        | None ->
            match stageFor action with
            | None -> source, Refused "needs a plot { } block, and the figure is not one"
            | Some(name, text) ->
                let recognised =
                    site.Ops.IsSome
                    || (match headName site.Head with
                        | Some h -> h.StartsWith "Chart."
                        | None -> false)

                let onOneLine = site.Expr.Range.StartLine = site.DeclarationLine

                // A stage aligns with the expression's first column: under a one-line binding,
                // anywhere else is a parse error.
                let column =
                    match site.Stages with
                    | s :: _ -> indentOf source starts s.Range.StartLine
                    | [] when onOneLine -> site.Expr.Range.StartColumn
                    | [] -> indentOf source starts site.Expr.Range.StartLine

                match site.Stages |> List.tryFind (fun s -> headName s = Some name) with
                | Some _ when action = YLog -> source, Structural "the y axis is already logarithmic"
                | Some s -> replace s.Range text, Structural $"replaced {name}"
                | None when recognised -> insertLineAfter site.Expr.Range.EndLine column ("|> " + text), Structural $"added {name}"
                | None ->
                    // Not a shape the app owns, so nothing inside it is touched.
                    let column = if onOneLine then site.Expr.Range.StartColumn else indentOf source starts site.Expr.Range.StartLine
                    insertLineAfter site.Expr.Range.EndLine column ("|> " + text), Fallback $"added {name} after an expression the app does not recognise"

/// The eleven actions, each with a stand-in argument, by the names the app shows them under.
let kinds: (string * Action) list =
    [ "title", Title "title"
      "x label", XLabel "x"
      "y label", YLabel "y"
      "log y", YLog
      "y domain", YDomain(0.0, 1.0)
      "theme", ThemeIs "publication"
      "legend", Legend "LegendTopRight"
      "colour by", ColorBy "column"
      "facet", Facet "column"
      "fit", Fit
      "layer", AddLayer "GeomLine" ]

/// For each kind of action, what doing it to this figure would come to: what the panel shows as
/// available, greyed out with a reason, or marked as the fallback.
let outcomes (path: string) (figure: string) (source: string) : (string * Outcome) list =
    kinds |> List.map (fun (name, action) -> name, snd (apply path figure action source))

/// Whether doing an action twice is doing it once, for the actions where it should be.
let settlesTwice (path: string) (figure: string) (action: Action) (source: string) =
    if not (settles action) then
        true
    else
        let once, _ = apply path figure action source
        let twice, _ = apply path figure action once
        once = twice

// ---- Columns --------------------------------------------------------------------------------------

/// The range of the figure's `plot data { … }` block, if it is one.
let blockRange (path: string) (figure: string) (source: string) : range option =
    let path = if String.IsNullOrEmpty path then "script.fsx" else path

    find figure (parse path source)
    |> Option.bind (fun site -> site.Ops |> Option.map (fun _ -> site.Head.Range))

/// The columns a figure's rows have: the fields of the record its block reads with `r.Field`,
/// in their declared order. Empty when the block reads none, or the rows are not a record.
let columns (results: FSharpCheckFileResults) (block: range) : string list =
    results.GetAllUsesOfAllSymbolsInFile()
    |> Seq.filter (fun u -> Range.rangeContainsRange block u.Range)
    |> Seq.tryPick (fun u ->
        match u.Symbol with
        | :? FSharp.Compiler.Symbols.FSharpField as f ->
            f.DeclaringEntity |> Option.filter (fun e -> e.IsFSharpRecord)
        | _ -> None)
    |> Option.map (fun e -> e.FSharpFields |> Seq.map (fun f -> f.Name) |> List.ofSeq)
    |> Option.defaultValue []

// ---- What a block plots ---------------------------------------------------------------------------

/// The field a row's accessor reads: `fun r -> r.Field`, or `float r.Field`, or in parentheses.
let rec private fieldOf (e: SynExpr) =
    match e with
    | SynExpr.Lambda(body = body) -> fieldOf body
    | SynExpr.Paren(expr = inner) -> fieldOf inner
    | SynExpr.App(argExpr = arg) -> fieldOf arg
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when ids.Length >= 2 -> Some (List.last ids).idText
    | SynExpr.DotGet(longDotId = SynLongIdent(id = ids)) -> ids |> List.tryLast |> Option.map (fun i -> i.idText)
    | _ -> None

/// What a figure's `plot <rows> { x …; y … }` block plots: the rows' binding and the fields its x
/// and y accessors read. None for a figure that is not such a block.
let plotted (path: string) (figure: string) (source: string) : (string * string * string) option =
    let path = if String.IsNullOrEmpty path then "script.fsx" else path

    match find figure (parse path source) with
    | Some { Head = SynExpr.App(funcExpr = SynExpr.App(argExpr = rows)); Ops = Some ops } ->
        let accessor name =
            ops
            |> List.tryFind (fun o -> headName o = Some name)
            |> Option.bind (function
                | SynExpr.App(argExpr = lambda) -> fieldOf lambda
                | _ -> None)

        match headName rows, accessor "x", accessor "y" with
        | Some r, Some x, Some y -> Some(r, x, y)
        | _ -> None
    | _ -> None
