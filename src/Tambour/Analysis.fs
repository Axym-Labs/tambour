/// `tambour --analysis <socket> <app pid>`: the language service. An `FSharpChecker` over the
/// script as the worker runs it, the prelude before it, against the assemblies this app ships
/// with, so it needs no SDK. A process of its own, so that ending a worker never ends completion.
module Tambour.Analysis

open System
open System.Diagnostics
open System.IO
open System.Net.Sockets
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Text
open FSharp.Compiler.Tokenization
open Tambour.Protocol

let private shorten = Evaluation.shorten

let private kindOf (glyph: FSharpGlyph) =
    match glyph with
    | FSharpGlyph.Class
    | FSharpGlyph.Struct
    | FSharpGlyph.Union
    | FSharpGlyph.Type
    | FSharpGlyph.Typedef
    | FSharpGlyph.Interface
    | FSharpGlyph.Exception
    | FSharpGlyph.Enum
    | FSharpGlyph.Delegate -> "type"
    | FSharpGlyph.Module -> "module"
    | FSharpGlyph.NameSpace -> "namespace"
    | FSharpGlyph.Method
    | FSharpGlyph.OverridenMethod
    | FSharpGlyph.ExtensionMethod -> "method"
    | FSharpGlyph.Property -> "property"
    | FSharpGlyph.Field -> "field"
    | FSharpGlyph.Event -> "event"
    | FSharpGlyph.EnumMember
    | FSharpGlyph.Constant -> "constant"
    | FSharpGlyph.Variable -> "value"
    | _ -> "other"

let private text (tagged: TaggedText[]) = tagged |> Array.map (fun t -> t.Text) |> String.concat ""

/// The assemblies a script is checked against: the runtime's own, FSharp.Core and Marey from
/// this app's directory, and whatever else the app states. The runtime's are its implementation
/// assemblies, as FSI compiles against them; no reference pack is needed.
let private referencesFor (stated: string[]) =
    let runtime = Path.GetDirectoryName(typeof<obj>.Assembly.Location)

    [| for f in Directory.GetFiles(runtime, "*.dll") do
           let n = Path.GetFileName f

           if (n.StartsWith "System." || n = "netstandard.dll" || n = "mscorlib.dll") && not (n.Contains "Native") then
               f
       Path.Combine(AppContext.BaseDirectory, "FSharp.Core.dll")
       yield! Directory.GetFiles(AppContext.BaseDirectory, "Marey*.dll") |> Array.sort
       yield! stated |]

/// A plain identifier: what a theme, a placement or a geom is named by.
let private identifier = Text.RegularExpressions.Regex(@"^[A-Za-z_][A-Za-z0-9_]*$")

/// The edit an action names, or why there is none.
let actionOf (spec: ActionSpec) : Result<Tambour.Edits.Edits.Action, string> =
    let named (make: string -> Tambour.Edits.Edits.Action) =
        if not (isNull spec.Text) && identifier.IsMatch spec.Text then Ok(make spec.Text)
        else Error $"{spec.Kind} wants a name, not {spec.Text}"

    let text (make: string -> Tambour.Edits.Edits.Action) = Ok(make (if isNull spec.Text then "" else spec.Text))

    match spec.Kind with
    | "title" -> text Tambour.Edits.Edits.Title
    | "x label" -> text Tambour.Edits.Edits.XLabel
    | "y label" -> text Tambour.Edits.Edits.YLabel
    | "log y" -> Ok Tambour.Edits.Edits.YLog
    | "y domain" when Double.IsFinite spec.Lo && Double.IsFinite spec.Hi && spec.Lo < spec.Hi ->
        Ok(Tambour.Edits.Edits.YDomain(spec.Lo, spec.Hi))
    | "y domain" -> Error "a domain wants two finite ends, the lower first"
    | "theme" -> named Tambour.Edits.Edits.ThemeIs
    | "legend" -> named Tambour.Edits.Edits.Legend
    | "colour by" -> named Tambour.Edits.Edits.ColorBy
    | "facet" -> named Tambour.Edits.Edits.Facet
    | "fit" -> Ok Tambour.Edits.Edits.Fit
    | "layer" -> named Tambour.Edits.Edits.AddLayer
    | other -> Error $"no action is called {other}"

let private outcomeOf (kind: string) (outcome: Tambour.Edits.Edits.Outcome) : ActionOutcome =
    match outcome with
    | Tambour.Edits.Edits.Structural note -> { Kind = kind; Outcome = "structural"; Note = note }
    | Tambour.Edits.Edits.Fallback note -> { Kind = kind; Outcome = "fallback"; Note = note }
    | Tambour.Edits.Edits.Refused note -> { Kind = kind; Outcome = "refused"; Note = note }

/// The language service for one prelude.
type Service(init: AnalysisInit) =
    let checker = FSharpChecker.Create()

    let prelude =
        match init.Prelude with
        | null -> ""
        | p when p.EndsWith "\n" -> p
        | p -> p + "\n"

    /// The lines the prelude puts before the script.
    let offset = prelude.Split('\n').Length - 1
    let references = referencesFor init.References |> Array.map (fun r -> "-r:" + r)

    let mutable options: (string * string * FSharpProjectOptions) option = None

    /// The options for a script, made again only when its path or its directives change.
    let optionsFor (path: string) (full: string) =
        let directives =
            full.Split('\n') |> Array.filter (fun l -> l.TrimStart().StartsWith "#") |> String.concat "\n"

        match options with
        | Some(p, d, o) when p = path && d = directives -> o
        | _ ->
            let o, _ =
                checker.GetProjectOptionsFromScript(
                    path,
                    SourceText.ofString full,
                    assumeDotNetFramework = false,
                    useSdkRefs = false,
                    otherFlags = [| "--targetprofile:netcore"; "--simpleresolution"; "--noframework" |]
                )
                |> Async.RunSynchronously

            let o = { o with OtherOptions = Array.append o.OtherOptions references }
            options <- Some(path, directives, o)
            o

    let pathOf (path: string) = if String.IsNullOrEmpty path then Path.Combine(Environment.CurrentDirectory, "script.fsx") else path

    let check (path: string) (script: string) =
        let path = pathOf path
        let full = prelude + script
        let parse, answer =
            checker.ParseAndCheckFileInProject(path, full.GetHashCode(), SourceText.ofString full, optionsFor path full)
            |> Async.RunSynchronously

        match answer with
        | FSharpCheckFileAnswer.Succeeded results -> path, full, parse, results
        | FSharpCheckFileAnswer.Aborted -> failwith "the check was aborted"

    let lineOf (full: string) (line: int) =
        let lines = full.Split('\n')
        if line >= 1 && line <= lines.Length then lines.[line - 1].TrimEnd('\r') else ""

    /// The identifier at a column, as FCS wants it: its names and the column just past it.
    let island (lineText: string) (column: int) =
        QuickParse.GetCompleteIdentifierIsland false lineText column
        |> Option.orElse (if column > 0 then QuickParse.GetCompleteIdentifierIsland false lineText (column - 1) else None)
        |> Option.map (fun (ident, columnEnd, _) -> List.ofArray (ident.Split '.'), columnEnd)

    member _.Offset = offset

    member _.Check(source: Source) : Checked =
        let path, _, _, results = check source.Path source.Text

        let diagnostics =
            results.Diagnostics
            |> Array.filter (fun d -> String.Equals(Path.GetFullPath d.FileName, Path.GetFullPath path, StringComparison.Ordinal))
            |> Array.map (fun d ->
                let inPrelude = d.StartLine <= offset

                { Severity =
                    match d.Severity with
                    | FSharpDiagnosticSeverity.Error -> "error"
                    | FSharpDiagnosticSeverity.Warning -> "warning"
                    | _ -> "info"
                  Code = d.ErrorNumberText
                  Message = (if inPrelude then "in the prelude: " else "") + shorten d.Message
                  StartLine = (if inPrelude then 1 else d.StartLine - offset)
                  StartColumn = (if inPrelude then 0 else d.StartColumn)
                  EndLine = (if inPrelude then 1 else d.EndLine - offset)
                  EndColumn = (if inPrelude then 0 else d.EndColumn) })

        { Id = source.Id; Diagnostics = diagnostics }

    member _.Complete(at: At) : Completions =
        let _, full, parse, results = check at.Path at.Text
        let line = at.Line + offset
        let lineText = lineOf full line
        let partial = QuickParse.GetPartialLongNameEx(lineText, at.Column - 1)
        let decls = results.GetDeclarationListInfo(Some parse, line, lineText, partial)

        { Id = at.Id
          Items =
            decls.Items
            |> Array.map (fun i -> { Name = i.NameInList; Insert = i.NameInCode; Kind = kindOf i.Glyph }) }

    member _.Hover(at: At) : Hovered =
        let _, full, _, results = check at.Path at.Text
        let line = at.Line + offset
        let lineText = lineOf full line

        let shown =
            match island lineText at.Column with
            | None -> ""
            | Some(names, columnEnd) ->
                let (ToolTipText elements) = results.GetToolTip(line, columnEnd, lineText, names, FSharpTokenTag.Identifier)

                elements
                |> List.collect (function
                    | ToolTipElement.Group items -> items |> List.map (fun i -> text i.MainDescription)
                    | ToolTipElement.CompositionError e -> [ e ]
                    | ToolTipElement.None -> [])
                |> String.concat "\n"

        { Id = at.Id; Text = shorten shown }

    member _.Signature(at: At) : Signatures =
        let _, full, _, results = check at.Path at.Text
        let line = at.Line + offset
        let lineText = lineOf full line

        // The unclosed parenthesis before the place, and the commas between them at its depth.
        let mutable depth = 0
        let mutable commas = 0
        let mutable opening = -1
        let mutable i = min (at.Column - 1) (lineText.Length - 1)

        while opening < 0 && i >= 0 do
            match lineText.[i] with
            | ')' -> depth <- depth + 1
            | '(' when depth = 0 -> opening <- i
            | '(' -> depth <- depth - 1
            | ',' when depth = 0 -> commas <- commas + 1
            | _ -> ()

            i <- i - 1

        let before = if opening > 0 then lineText.Substring(0, opening).TrimEnd().Length else 0

        match (if before > 0 then island lineText (before - 1) else None) with
        | None ->
            { Id = at.Id
              Methods = [||]
              Active = 0
              Parameter = 0 }
        | Some(names, columnEnd) ->
            let group = results.GetMethods(line, columnEnd, lineText, Some names)

            let shown =
                group.Methods
                |> Array.map (fun m ->
                    let parameters = m.Parameters |> Array.map (fun p -> text p.Display) |> String.concat ", "
                    shorten $"{group.MethodName}({parameters}) : {text m.ReturnTypeText}")

            let active =
                group.Methods
                |> Array.tryFindIndex (fun m -> m.Parameters.Length > commas)
                |> Option.defaultValue 0

            { Id = at.Id
              Methods = shown
              Active = active
              Parameter = commas }

    member _.Uses(at: At) : Used =
        let path, full, _, results = check at.Path at.Text
        let line = at.Line + offset
        let lineText = lineOf full line

        let none =
            { Id = at.Id
              Name = ""
              Spans = [||]
              Renamable = false }

        match island lineText at.Column with
        | None -> none
        | Some(names, columnEnd) ->
            match results.GetSymbolUseAtLocation(line, columnEnd, lineText, names) with
            | None -> none
            | Some symbolUse ->
                let symbol = symbolUse.Symbol
                let name = symbol.DisplayName

                let spans =
                    results.GetUsesOfSymbolInFile symbol
                    |> Array.map (fun u -> u.Range)
                    |> Array.filter (fun r -> r.StartLine > offset && r.StartLine = r.EndLine)
                    // A use spelled `Module.name` covers the whole path; the name is its end.
                    |> Array.map (fun r ->
                        { StartLine = r.StartLine - offset
                          StartColumn = max r.StartColumn (r.EndColumn - name.Length)
                          EndLine = r.EndLine - offset
                          EndColumn = r.EndColumn })
                    |> Array.distinct

                let renamable =
                    match symbol.DeclarationLocation with
                    | Some r -> String.Equals(Path.GetFullPath r.FileName, Path.GetFullPath path, StringComparison.Ordinal) && r.StartLine > offset
                    | None -> false

                { Id = at.Id
                  Name = name
                  Spans = spans
                  Renamable = renamable }

    /// What each action would come to for a figure, and the columns its rows have.
    member _.Actions(f: FigureIn) : ActionsFor =
        let outcomes =
            Tambour.Edits.Edits.outcomes f.Path f.Figure f.Text
            |> List.map (fun (kind, outcome) -> outcomeOf kind outcome)
            |> Array.ofList

        // The block is found in the text the checker saw, the prelude before the script.
        let path, full, _, results = check f.Path f.Text

        let columns =
            Tambour.Edits.Edits.blockRange path f.Figure full
            |> Option.map (Tambour.Edits.Edits.columns results)
            |> Option.defaultValue []
            |> Array.ofList

        { Id = f.Id
          Outcomes = outcomes
          Columns = columns }

    /// An action done to a figure: the new script, or the old one where it was refused.
    member _.Act(a: ActionOn) : Acted =
        match actionOf a.Action with
        | Error why ->
            { Id = a.Id
              Text = a.Text
              Outcome = { Kind = a.Action.Kind; Outcome = "refused"; Note = why } }
        | Ok action ->
            let text, outcome = Tambour.Edits.Edits.apply a.Path a.Figure action a.Text

            { Id = a.Id
              Text = text
              Outcome = outcomeOf a.Action.Kind outcome }

/// Connects to the app's socket and answers it until it closes or the app's process `app` ends.
let run (socketPath: string) (app: int) : int =
    Worker.outlive app
    use socket = new Socket(AddressFamily.Unix, SocketType.Stream, ProtocolType.Unspecified)
    socket.Connect(UnixDomainSocketEndPoint socketPath)
    use connection = new Connection(new NetworkStream(socket, true))
    let send (reply: AnalysisReply) = connection.Send(Codec.encodeAnalysisReply reply)
    let mutable service: Service option = None

    let idOf (request: AnalysisRequest) =
        match request with
        | AnalysisRequest.Start _ -> 0
        | AnalysisRequest.Check s -> s.Id
        | AnalysisRequest.Complete a
        | AnalysisRequest.Hover a
        | AnalysisRequest.Signature a
        | AnalysisRequest.Uses a -> a.Id
        | AnalysisRequest.Actions f -> f.Id
        | AnalysisRequest.Act a -> a.Id

    let handle (request: AnalysisRequest) =
        match request, service with
        | AnalysisRequest.Start init, _ ->
            let s = Service init
            // The first check compiles the checker's own path; done here, before the editor asks.
            s.Check { Id = 0; Text = "let __tambour = 1\n"; Path = null } |> ignore
            service <- Some s
            send (AnalysisReply.Started (DateTime.Now - Process.GetCurrentProcess().StartTime).TotalMilliseconds)
        | _, None -> send (AnalysisReply.Failed(idOf request, "the language service has not been started"))
        | AnalysisRequest.Check s, Some service -> send (AnalysisReply.Checked(service.Check s))
        | AnalysisRequest.Complete a, Some service -> send (AnalysisReply.Completions(service.Complete a))
        | AnalysisRequest.Hover a, Some service -> send (AnalysisReply.Hovered(service.Hover a))
        | AnalysisRequest.Signature a, Some service -> send (AnalysisReply.Signatures(service.Signature a))
        | AnalysisRequest.Uses a, Some service -> send (AnalysisReply.Used(service.Uses a))
        | AnalysisRequest.Actions f, Some service -> send (AnalysisReply.Actions(service.Actions f))
        | AnalysisRequest.Act a, Some service -> send (AnalysisReply.Acted(service.Act a))

    let rec loop () =
        match connection.Receive() with
        | None -> 0
        | Some bytes ->
            match (try Ok(Codec.decodeAnalysisRequest bytes) with ProtocolException m -> Error m) with
            | Error message -> send (AnalysisReply.Failed(0, message))
            | Ok request ->
                try
                    handle request
                with e ->
                    send (AnalysisReply.Failed(idOf request, e.Message))

            loop ()

    try
        loop ()
    with :? IOException ->
        0
