namespace Tambour.Protocol

open System
open System.Buffers.Binary
open System.Text.Json
open System.Text.Json.Serialization

/// A message that is not one: an unknown tag, a body that does not parse, a truncated frame.
exception ProtocolException of string

/// A message's bytes: one tag byte, then its body. A body is JSON, except for a scene, a patch or
/// an export, where it is the figure's index (four bytes), its width and height (eight bytes
/// each), all little-endian, and then the bytes unchanged.
module Codec =

    let private json =
        JsonSerializerOptions(NumberHandling = JsonNumberHandling.AllowNamedFloatingPointLiterals)

    let private tagged (tag: byte) (value: 'a) =
        let body = JsonSerializer.SerializeToUtf8Bytes(value, json)
        let bytes = Array.zeroCreate (1 + body.Length)
        bytes.[0] <- tag
        Buffer.BlockCopy(body, 0, bytes, 1, body.Length)
        bytes

    [<Literal>]
    let private FrameHeader = 21

    let private binary (tag: byte) (frame: Frame) =
        let bytes = Array.zeroCreate (FrameHeader + frame.Bytes.Length)
        bytes.[0] <- tag
        BinaryPrimitives.WriteInt32LittleEndian(Span(bytes, 1, 4), frame.Figure)
        BinaryPrimitives.WriteDoubleLittleEndian(Span(bytes, 5, 8), frame.Width)
        BinaryPrimitives.WriteDoubleLittleEndian(Span(bytes, 13, 8), frame.Height)
        Buffer.BlockCopy(frame.Bytes, 0, bytes, FrameHeader, frame.Bytes.Length)
        bytes

    let private body<'a> (bytes: byte[]) : 'a =
        let value =
            try
                JsonSerializer.Deserialize<'a>(ReadOnlySpan(bytes, 1, bytes.Length - 1), json)
            with :? JsonException as e ->
                raise (ProtocolException $"message {bytes.[0]}: {e.Message}")

        if isNull (box value) then
            raise (ProtocolException $"message {bytes.[0]} has an empty body")

        value

    let private frame (bytes: byte[]) : Frame =
        if bytes.Length < FrameHeader then
            raise (ProtocolException $"message {bytes.[0]} is {bytes.Length} bytes, too short for a frame")

        { Figure = BinaryPrimitives.ReadInt32LittleEndian(ReadOnlySpan(bytes, 1, 4))
          Width = BinaryPrimitives.ReadDoubleLittleEndian(ReadOnlySpan(bytes, 5, 8))
          Height = BinaryPrimitives.ReadDoubleLittleEndian(ReadOnlySpan(bytes, 13, 8))
          Bytes = bytes.[FrameHeader..] }

    let private nonEmpty (bytes: byte[]) =
        if isNull bytes || bytes.Length = 0 then
            raise (ProtocolException "an empty message")

    // Requests from 1, replies from 64, so that a message read in the wrong direction is caught.

    let encodeRequest (request: Request) : byte[] =
        match request with
        | Request.Init m -> tagged 1uy m
        | Request.Evaluate m -> tagged 2uy m
        | Request.Select figure -> tagged 3uy figure
        | Request.Live m -> tagged 4uy m
        | Request.Export m -> tagged 5uy m

    let decodeRequest (bytes: byte[]) : Request =
        nonEmpty bytes

        match bytes.[0] with
        | 1uy -> Request.Init(body bytes)
        | 2uy -> Request.Evaluate(body bytes)
        | 3uy -> Request.Select(body bytes)
        | 4uy -> Request.Live(body bytes)
        | 5uy -> Request.Export(body bytes)
        | tag -> raise (ProtocolException $"no request has the tag {tag}")

    let encodeReply (reply: Reply) : byte[] =
        match reply with
        | Reply.Ready m -> tagged 64uy m
        | Reply.Evaluated m -> tagged 65uy m
        | Reply.Scene f -> binary 66uy f
        | Reply.Patch f -> binary 67uy f
        | Reply.Controls m -> tagged 68uy m
        | Reply.Readout m -> tagged 69uy m
        | Reply.Output m -> tagged 70uy m
        | Reply.Exported f -> binary 71uy f
        | Reply.Failed message -> tagged 72uy message

    let decodeReply (bytes: byte[]) : Reply =
        nonEmpty bytes

        match bytes.[0] with
        | 64uy -> Reply.Ready(body bytes)
        | 65uy -> Reply.Evaluated(body bytes)
        | 66uy -> Reply.Scene(frame bytes)
        | 67uy -> Reply.Patch(frame bytes)
        | 68uy -> Reply.Controls(body bytes)
        | 69uy -> Reply.Readout(body bytes)
        | 70uy -> Reply.Output(body bytes)
        | 71uy -> Reply.Exported(frame bytes)
        | 72uy -> Reply.Failed(body bytes)
        | tag -> raise (ProtocolException $"no reply has the tag {tag}")
