module Tambour.Tests.ProtocolTests

open System.IO
open Tambour.Protocol
open Xunit

let private evaluated =
    { Ok = true
      Figures = [| { Name = "fig"; Controls = 2 }; { Name = "it"; Controls = 0 } |]
      Selected = 1
      Diagnostics =
        [| { Severity = "warning"
             Code = "FS0020"
             Message = "ignored"
             StartLine = 3
             StartColumn = 4
             EndLine = 3
             EndColumn = 9 } |]
      Exception = null
      Timings = { Evaluate = 26.5; Layout = 12.25; Encode = 5.0 }
      Heap = 123456789L
      Prelude = true }

let requests: obj[] list =
    [ Request.Init
          { References = [| "/a/b.dll" |]
            Prelude = "open Marey"
            Skia = { Dpi = 96.0 / 25.4; FontFiles = [| "f.ttf" |] } }
      Request.Evaluate { Text = "let x = 1\n"; Path = null }
      Request.Select 2
      Request.Live { Kind = "zoom"; Args = [| 1.0; 2.5; nan; infinity |] }
      Request.Export { Format = "png"; Width = 800.0; Height = 500.0; Skia = { Dpi = 3.0; FontFiles = [||] } }
      Request.Table { Name = "trials λ"; Bytes = Array.init 1000 byte }
      Request.Pick { Values = [| 1.0; 2.0; nan; 4.5 |]; Clamp = true }
      Request.Place { Values = [||]; Clamp = false } ]
    |> List.map (fun r -> [| box r |])

let replies: obj[] list =
    [ Reply.Ready { Elapsed = 2500.0; Heap = 1L <<< 40 }
      Reply.Evaluated evaluated
      Reply.Scene { Figure = 0; Width = 640.0; Height = 400.5; Bytes = [| 1uy; 2uy; 3uy |] }
      Reply.Patch { Figure = 3; Width = 1.0; Height = 2.0; Bytes = [||] }
      Reply.Controls
          { Figure = 0
            Controls =
              [| { Label = "g"
                   Kind = "select"
                   Lo = 0.0
                   Hi = 0.0
                   Step = 0.0
                   Samples = 0
                   Loop = false
                   Options = [| "a"; "b" |]
                   Init = 1.0
                   Value = 0.0
                   Shown = "a" } |] }
      Reply.Readout
          { Figure = 0
            Visible = true
            X0 = 1.0
            Y0 = 2.0
            X1 = 3.0
            Y1 = 4.0
            Right = true
            Lines = [| "x 1 s"; "y 2 m" |] }
      Reply.Output { Stream = "err"; Text = "ünïcode\n" }
      Reply.Exported { Figure = 1; Width = 800.0; Height = 500.0; Bytes = Array.init 300 byte }
      Reply.Failed "no"
      Reply.Picked { Figure = 1; Values = [| 0.5; nan |] }
      Reply.Placed { Figure = 0; Values = [| 10.0; 20.0 |] } ]
    |> List.map (fun r -> [| box r |])

// Structural equality would hold even for a codec that dropped a NaN, so the JSON form of each
// side is compared, which tells NaN from a missing number.
let private same (a: 'a) (b: 'a) =
    Assert.Equal(sprintf "%A" a, sprintf "%A" b)

[<Theory; MemberData(nameof requests)>]
let ``a request comes back from its bytes as it was`` (request: Request) =
    same request (Codec.decodeRequest (Codec.encodeRequest request))

[<Theory; MemberData(nameof replies)>]
let ``a reply comes back from its bytes as it was`` (reply: Reply) =
    same reply (Codec.decodeReply (Codec.encodeReply reply))

[<Fact>]
let ``a reply is not read as a request, nor a request as a reply`` () =
    Assert.Throws<ProtocolException>(fun () -> Codec.decodeRequest (Codec.encodeReply (Reply.Failed "x")) |> ignore) |> ignore
    Assert.Throws<ProtocolException>(fun () -> Codec.decodeReply (Codec.encodeRequest (Request.Select 0)) |> ignore) |> ignore

[<Fact>]
let ``frames come back in order, and the stream's end is told from a cut frame`` () =
    use stream = new MemoryStream()
    let messages = [ [| 1uy |]; Array.init 70000 byte; [| 9uy; 9uy |] ]

    for m in messages do
        Framing.write stream m

    let whole = stream.ToArray()
    stream.Position <- 0L
    let back = List.init 3 (fun _ -> Framing.read stream |> Option.get)
    Assert.Equal<byte[] list>(messages, back)
    Assert.Equal(None, Framing.read stream)

    use cut = new MemoryStream(whole.[.. whole.Length - 2])
    Framing.read cut |> ignore
    Framing.read cut |> ignore
    Assert.Throws<ProtocolException>(fun () -> Framing.read cut |> ignore) |> ignore

[<Fact>]
let ``a frame with a length out of range is refused`` () =
    use stream = new MemoryStream([| 0xFFuy; 0xFFuy; 0xFFuy; 0x7Fuy; 0uy |])
    Assert.Throws<ProtocolException>(fun () -> Framing.read stream |> ignore) |> ignore
