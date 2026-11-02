/// Each screen's scaling under XWayland, found at start rather than given by hand: the factor is
/// how many of X11's pixels a logical pixel of the compositor's spans, so that a compositor that
/// leaves X11 clients unscaled gets them drawn at its scale, and one that scales them itself does
/// not get them scaled twice.
module Tambour.Tests.ScalingTests

open System
open System.IO
open System.Net.Sockets
open System.Text
open System.Threading
open System.Threading.Tasks
open Tambour
open Xunit

let private output name mode logical scale transform : Scaling.Output =
    { Name = name; Mode = mode; Logical = logical; Scale = scale; Transform = transform }

[<Fact>]
let ``the factor is X11's pixels over the compositor's logical ones`` () =
    let outputs =
        [ output "DP-1" (Some(2560, 1440)) (Some(1280, 720)) 2 0
          output "DP-3" (Some(1920, 1080)) (Some(1920, 1080)) 1 0 ]

    // X11 shown unscaled pixels (Hyprland's force_zero_scaling): each screen at its scale.
    Assert.Equal<(string * float) list>([ "DP-1", 2.0; "DP-3", 1.0 ], Scaling.factors outputs (Map.ofList [ "DP-1", 2560; "DP-3", 1920 ]))
    // X11 shown logical pixels, the compositor scaling the windows itself: nothing more.
    Assert.Equal<(string * float) list>([ "DP-1", 1.0; "DP-3", 1.0 ], Scaling.factors outputs (Map.ofList [ "DP-1", 1280; "DP-3", 1920 ]))
    // A fractional scale.
    Assert.Equal<(string * float) list>([ "eDP-1", 1.5 ], Scaling.factors [ output "eDP-1" (Some(2880, 1800)) (Some(1920, 1200)) 2 0 ] (Map.ofList [ "eDP-1", 2880 ]))
    // A screen turned on its side, no logical size told: the mode's height is its width.
    Assert.Equal<(string * float) list>([ "DP-2", 2.0 ], Scaling.factors [ output "DP-2" (Some(2560, 1440)) None 2 1 ] (Map.ofList [ "DP-2", 1440 ]))
    // A screen X11 does not have, or one with no size, is left out.
    Assert.Empty(Scaling.factors [ output "HDMI-1" (Some(1920, 1080)) None 1 0; output "DP-9" None None 1 0 ] (Map.ofList [ "DP-9", 1920 ]))
    Assert.Equal("DP-1=2;DP-3=1;eDP-1=1.5", Scaling.variable [ "DP-1", 2.0; "DP-3", 1.0; "eDP-1", 1.5 ])

[<Fact>]
let ``a scaling the user set is left as it is`` () =
    let env (pairs: (string * string) list) = fun name -> pairs |> List.tryFind (fst >> (=) name) |> Option.map snd |> Option.toObj
    Assert.False(Scaling.setByUser (env [ "WAYLAND_DISPLAY", "wayland-1" ]))

    for name in [ "AVALONIA_SCREEN_SCALE_FACTORS"; "AVALONIA_GLOBAL_SCALE_FACTOR"; "AVALONIA_USE_PHYSICAL_DPI"; "QT_SCREEN_SCALE_FACTORS"; "QT_SCALE_FACTOR" ] do
        Assert.True(Scaling.setByUser (env [ name, "2" ]), name)

// ---- A compositor of its own, for the client to ask -----------------------------------------------

let private u32 (b: byte[]) (i: int) = BitConverter.ToUInt32(b, i)

let private str (b: byte[]) (i: int) =
    let n = int (u32 b i)
    Encoding.UTF8.GetString(b, i + 4, n - 1), i + 4 + ((n + 3) &&& ~~~3)

let private event (id: uint32) (opcode: int) (args: byte[] list) =
    let body = Array.concat args
    Array.concat [ BitConverter.GetBytes id; BitConverter.GetBytes(uint32 ((body.Length + 8) <<< 16 ||| opcode)); body ]

let private n (v: int) = BitConverter.GetBytes v

let private s (text: string) =
    let bytes = Encoding.UTF8.GetBytes text
    let padded = (bytes.Length + 1 + 3) &&& ~~~3
    Array.concat [ BitConverter.GetBytes(uint32 (bytes.Length + 1)); bytes; Array.zeroCreate (padded - bytes.Length) ]

[<Fact>]
let ``the outputs are read from a compositor's wl_output and xdg_output`` () =
    // The app asks a compositor only on Linux; elsewhere the protocol is no one's.
    Assert.SkipUnless(OperatingSystem.IsLinux(), "Wayland is Linux's")
    let path = Path.Combine(Path.GetTempPath(), $"tambour-wl-{Guid.NewGuid():N}")
    use listener = new Socket(AddressFamily.Unix, SocketType.Stream, ProtocolType.Unspecified)
    listener.Bind(UnixDomainSocketEndPoint path)
    listener.Listen 1
    let bound = Collections.Generic.Dictionary<uint32, uint32 * uint32>() // global name -> id, version

    let server =
        Task.Run(fun () ->
            use client = listener.Accept()
            use stream = new NetworkStream(client)

            let read (count: int) =
                let b = Array.zeroCreate count
                let mutable got = 0

                while got < count do
                    let r = stream.Read(b, got, count - got)
                    if r = 0 then raise (EndOfStreamException())
                    got <- got + r

                b

            let send (bytes: byte[]) = stream.Write(bytes, 0, bytes.Length)
            let mutable registry = 0u
            let mutable manager = 0u
            let xdg = Collections.Generic.Dictionary<uint32, uint32>() // xdg_output id -> wl_output id
            let mutable syncs = 0

            try
                while syncs < 2 do
                    let header = read 8
                    let id, word = u32 header 0, u32 header 4
                    let body = read (int (word >>> 16) - 8)

                    match id, int (word &&& 0xFFFFu) with
                    | 1u, 1 -> registry <- u32 body 0
                    | 1u, 0 ->
                        let callback = u32 body 0
                        syncs <- syncs + 1

                        if syncs = 1 then
                            for name, iface, version in [ 1, "wl_output", 4; 2, "wl_output", 3; 3, "zxdg_output_manager_v1", 3; 4, "wl_seat", 7 ] do
                                send (event registry 0 [ n name; s iface; n version ])
                        else
                            let a, _ = bound.[1u]
                            let b, _ = bound.[2u]
                            // The first: its own name, a current mode among two, scale 2.
                            send (event a 0 [ n 0; n 0; n 600; n 340; n 0; s "Make"; s "Model"; n 0 ])
                            send (event a 1 [ n 0; n 1920; n 1080; n 60000 ])
                            send (event a 1 [ n 3; n 2560; n 1440; n 200000 ])
                            send (event a 3 [ n 2 ])
                            send (event a 4 [ s "DP-1" ])
                            send (event a 2 [])
                            // The second, bound at version 3: no name of its own, xdg_output's.
                            send (event b 0 [ n 0; n 0; n 500; n 300; n 0; s "Make"; s "Model"; n 0 ])
                            send (event b 1 [ n 1; n 1920; n 1080; n 60000 ])
                            send (event b 2 [])

                            for KeyValue(x, o) in xdg do
                                if o = a then send (event x 1 [ n 1280; n 720 ])
                                else
                                    send (event x 1 [ n 1920; n 1080 ])
                                    send (event x 3 [ s "DP-3" ])

                        send (event callback 0 [ n 7 ])
                    | r, 0 when r = registry ->
                        let name = u32 body 0
                        let iface, j = str body 4
                        let version, newId = u32 body j, u32 body (j + 4)
                        bound.[name] <- (newId, version)
                        if iface = "zxdg_output_manager_v1" then manager <- newId
                    | m, 1 when m = manager -> xdg.[u32 body 0] <- u32 body 4
                    | _ -> ()
            with :? EndOfStreamException ->
                ())

    try
        let outputs = Scaling.outputs path |> List.sortBy (fun o -> o.Name)

        Assert.Equal<Scaling.Output list>(
            [ output "DP-1" (Some(2560, 1440)) (Some(1280, 720)) 2 0
              output "DP-3" (Some(1920, 1080)) (Some(1920, 1080)) 1 0 ],
            outputs
        )

        // wl_output at no more than the version with its name; the seat not asked for.
        Assert.Equal(4u, snd bound.[1u])
        Assert.Equal(3u, snd bound.[2u])
        Assert.False(bound.ContainsKey 4u)
        server.Wait(TimeSpan.FromSeconds 5.0) |> ignore
    finally
        File.Delete path
