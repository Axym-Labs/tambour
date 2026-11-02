/// Each screen's scaling on a Wayland desktop, found at start. Avalonia draws through X11 there, under
/// XWayland, and X11 has no scaling per screen to tell it: it reads only its own and Qt's environment
/// variables and `Xft.dpi`, so with none set it draws at 1× everywhere. The compositor knows each
/// screen's size in its own logical pixels, and X11 how many of its pixels the screen spans; their
/// ratio is the scaling Avalonia needs, handed to it as `AVALONIA_SCREEN_SCALE_FACTORS`. A
/// compositor that shows X11 clients unscaled pixels (Hyprland's `force_zero_scaling`) gives the
/// screen's scale, one that scales them itself gives 1, so nothing is scaled twice.
module Tambour.Scaling

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Net.Sockets
open System.Runtime.InteropServices
open System.Text

/// A screen as the compositor tells it: its connector's name, its current mode in pixels, its size in
/// logical pixels where `xdg_output` tells it, its whole-number scale, and its transform (odd ones
/// turn it on its side).
type Output =
    { Name: string
      Mode: (int * int) option
      Logical: (int * int) option
      Scale: int
      Transform: int }

/// A screen's width in the compositor's logical pixels: as told, or its mode's over its scale.
let private logicalWidth (o: Output) =
    match o.Logical, o.Mode with
    | Some(w, _), _ when w > 0 -> Some(float w)
    | _, Some(w, h) when o.Scale > 0 -> Some(float (if o.Transform % 2 = 1 then h else w) / float o.Scale)
    | _ -> None

/// Each screen's factor, X11's width of it over its logical width, for the screens both know.
let factors (outputs: Output list) (xWidths: Map<string, int>) =
    [ for o in outputs do
          match logicalWidth o, xWidths.TryFind o.Name with
          | Some logical, Some x when x > 0 -> o.Name, Math.Round(float x / logical, 2)
          | _ -> () ]

/// The factors as Avalonia reads them: `name=factor` joined by `;`.
let variable (factors: (string * float) list) =
    factors |> List.map (fun (name, f) -> name + "=" + f.ToString(CultureInfo.InvariantCulture)) |> String.concat ";"

/// Whether the user has set a scaling of their own, which Avalonia reads and this leaves alone.
let setByUser (get: string -> string) =
    [ "AVALONIA_SCREEN_SCALE_FACTORS"
      "AVALONIA_GLOBAL_SCALE_FACTOR"
      "AVALONIA_USE_PHYSICAL_DPI"
      "QT_SCREEN_SCALE_FACTORS"
      "QT_SCALE_FACTOR" ]
    |> List.exists (fun name -> not (String.IsNullOrEmpty(get name)))

// ---- The compositor, over the Wayland wire protocol -------------------------------------------------

let private u32 (b: byte[]) (i: int) = BitConverter.ToUInt32(b, i)
let private i32 (b: byte[]) (i: int) = BitConverter.ToInt32(b, i)

/// A string argument: its length with the NUL, its bytes, padded to four.
let private text (b: byte[]) (i: int) =
    let n = int (u32 b i)
    (if n = 0 then null else Encoding.UTF8.GetString(b, i + 4, n - 1)), i + 4 + ((n + 3) &&& ~~~3)

let private request (id: uint32) (opcode: int) (args: byte[] list) =
    let body = Array.concat args
    Array.concat [ BitConverter.GetBytes id; BitConverter.GetBytes(uint32 ((body.Length + 8) <<< 16 ||| opcode)); body ]

let private word (v: uint32) = BitConverter.GetBytes v

let private stringArg (s: string) =
    let bytes = Encoding.UTF8.GetBytes s
    Array.concat [ word (uint32 (bytes.Length + 1)); bytes; Array.zeroCreate (((bytes.Length + 1 + 3) &&& ~~~3) - bytes.Length) ]

/// The screens a compositor listening at `path` has. Fails where it does not answer in a second.
let outputs (path: string) : Output list =
    use socket = new Socket(AddressFamily.Unix, SocketType.Stream, ProtocolType.Unspecified)
    socket.ReceiveTimeout <- 1000
    socket.SendTimeout <- 1000
    socket.Connect(UnixDomainSocketEndPoint path)
    use stream = new NetworkStream(socket)
    let send (bytes: byte[]) = stream.Write(bytes, 0, bytes.Length)

    let read (count: int) =
        let b = Array.zeroCreate count
        let mutable got = 0

        while got < count do
            let r = stream.Read(b, got, count - got)
            if r = 0 then raise (EndOfStreamException "the compositor closed the connection")
            got <- got + r

        b

    let mutable last = 1u

    let fresh () =
        last <- last + 1u
        last

    /// Events until the compositor has done all that was asked before: a sync's callback is done.
    let roundTrip (handle: uint32 -> int -> byte[] -> unit) =
        let callback = fresh ()
        send (request 1u 0 [ word callback ])
        let mutable finished = false

        while not finished do
            let header = read 8
            let id, w = u32 header 0, u32 header 4
            let body = read (int (w >>> 16) - 8)
            let opcode = int (w &&& 0xFFFFu)

            if id = callback && opcode = 0 then finished <- true
            elif id = 1u && opcode = 0 then failwith $"the compositor refused: {fst (text body 8)}"
            else handle id opcode body

    let registry = fresh ()
    send (request 1u 1 [ word registry ])
    let globals = ResizeArray<uint32 * string * uint32>()

    roundTrip (fun id opcode body ->
        if id = registry && opcode = 0 then
            let iface, j = text body 4
            globals.Add(u32 body 0, iface, u32 body j))

    let bind (name: uint32, iface: string, version: uint32) (most: uint32) =
        let id = fresh ()
        send (request registry 0 [ word name; stringArg iface; word (min version most); word id ])
        id

    let found = Dictionary<uint32, Output>()

    for g in globals do
        match g with
        | _, "wl_output", _ -> found.[bind g 4u] <- { Name = null; Mode = None; Logical = None; Scale = 1; Transform = 0 }
        | _ -> ()

    // xdg_output's logical size, the one that holds a fractional scale.
    let xdg = Dictionary<uint32, uint32>()

    match globals |> Seq.tryFind (fun (_, iface, _) -> iface = "zxdg_output_manager_v1") with
    | Some g ->
        let manager = bind g 3u

        for o in List.ofSeq found.Keys do
            let id = fresh ()
            send (request manager 1 [ word id; word o ])
            xdg.[id] <- o
    | None -> ()

    roundTrip (fun id opcode body ->
        match found.TryGetValue id with
        | true, o ->
            match opcode with
            | 0 ->
                let _, j = text body 20
                let _, k = text body j
                found.[id] <- { o with Transform = i32 body k }
            | 1 when u32 body 0 &&& 1u <> 0u -> found.[id] <- { o with Mode = Some(i32 body 4, i32 body 8) }
            | 3 -> found.[id] <- { o with Scale = i32 body 0 }
            | 4 -> found.[id] <- { o with Name = fst (text body 0) }
            | _ -> ()
        | _ ->
            match xdg.TryGetValue id with
            | true, o ->
                match opcode with
                | 1 -> found.[o] <- { found.[o] with Logical = Some(i32 body 0, i32 body 4) }
                | 3 when isNull found.[o].Name -> found.[o] <- { found.[o] with Name = fst (text body 0) }
                | _ -> ()
            | _ -> ())

    [ for o in found.Values do
          if not (isNull o.Name) then o ]

// ---- X11, as XWayland shows the screens --------------------------------------------------------------

[<Struct; StructLayout(LayoutKind.Sequential)>]
type private MonitorInfo =
    val Name: nativeint
    val Primary: int
    val Automatic: int
    val Outputs: int
    val X: int
    val Y: int
    val Width: int
    val Height: int
    val MillimetresWide: int
    val MillimetresHigh: int
    val OutputList: nativeint

module private X11 =
    [<DllImport("libX11.so.6")>]
    extern nativeint XOpenDisplay(nativeint name)

    [<DllImport("libX11.so.6")>]
    extern nativeint XDefaultRootWindow(nativeint display)

    [<DllImport("libX11.so.6")>]
    extern nativeint XGetAtomName(nativeint display, nativeint atom)

    [<DllImport("libX11.so.6")>]
    extern int XFree(nativeint data)

    [<DllImport("libX11.so.6")>]
    extern int XCloseDisplay(nativeint display)

    [<DllImport("libXrandr.so.2")>]
    extern nativeint XRRGetMonitors(nativeint display, nativeint window, int active, int& count)

    [<DllImport("libXrandr.so.2")>]
    extern void XRRFreeMonitors(nativeint monitors)

/// Each screen's width in X11's pixels, by its name, as Avalonia reads them.
let monitors () : Map<string, int> =
    let display = X11.XOpenDisplay 0n

    if display = 0n then
        Map.empty
    else
        try
            let mutable count = 0
            let list = X11.XRRGetMonitors(display, X11.XDefaultRootWindow display, 1, &count)

            if list = 0n then
                Map.empty
            else
                try
                    let size = Marshal.SizeOf<MonitorInfo>()

                    [ for i in 0 .. count - 1 do
                          let m = Marshal.PtrToStructure<MonitorInfo>(list + nativeint (i * size))
                          let name = X11.XGetAtomName(display, m.Name)

                          if name <> 0n then
                              let text = Marshal.PtrToStringAnsi name
                              X11.XFree name |> ignore
                              text, m.Width ]
                    |> Map.ofList
                finally
                    X11.XRRFreeMonitors list
        finally
            X11.XCloseDisplay display |> ignore

/// Where the compositor listens, on a Wayland desktop.
let private socketPath () =
    match Environment.GetEnvironmentVariable "WAYLAND_DISPLAY" with
    | null
    | "" -> None
    | display when Path.IsPathRooted display -> Some display
    | display ->
        match Environment.GetEnvironmentVariable "XDG_RUNTIME_DIR" with
        | null
        | "" -> None
        | dir -> Some(Path.Combine(dir, display))

/// What the compositor and X11 say of the screens, and the factors from them, printed.
let check () =
    match socketPath () with
    | None ->
        printfn "no Wayland compositor (WAYLAND_DISPLAY is not set)"
        1
    | Some path ->
        let outputs = outputs path
        let xs = monitors ()

        for o in outputs do
            printfn $"Wayland {o.Name}: mode {o.Mode}, logical {o.Logical}, scale {o.Scale}, transform {o.Transform}"

        for KeyValue(name, width) in xs do
            printfn $"X11 {name}: {width} px wide"

        printfn $"AVALONIA_SCREEN_SCALE_FACTORS={variable (factors outputs xs)}"
        if setByUser Environment.GetEnvironmentVariable then printfn "(a scaling set by the user is in force, and is kept)"
        0

/// Hands Avalonia each screen's scaling on a Wayland desktop on Linux, unless the user set one; what
/// it handed, if anything. Anything that does not answer leaves Avalonia as it was.
let apply () =
    if not (OperatingSystem.IsLinux()) || setByUser Environment.GetEnvironmentVariable then
        None
    else
        match socketPath () with
        | None -> None
        | Some path ->
            try
                match factors (outputs path) (monitors ()) with
                | [] -> None
                | found ->
                    let value = variable found
                    Environment.SetEnvironmentVariable("AVALONIA_SCREEN_SCALE_FACTORS", value)
                    Some value
            with _ ->
                None
