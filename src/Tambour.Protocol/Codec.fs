namespace Tambour.Protocol

open System
open System.Buffers.Binary
open System.Text.Json
open System.Text.Json.Serialization

/// A message that is not one: an unknown tag, a body that does not parse, a truncated frame.
exception ProtocolException of string

/// A message's bytes: one tag byte, then its body. A body is JSON, except for a scene, a patch or
/// an export, where it is the figure's index (four bytes, little-endian) and the bytes unchanged.
module Codec =

    let private json =
        JsonSerializerOptions(NumberHandling = JsonNumberHandling.AllowNamedFloatingPointLiterals)

    let private tagged (tag: byte) (value: 'a) =
        let body = JsonSerializer.SerializeToUtf8Bytes(value, json)
        let bytes = Array.zeroCreate (1 + body.Length)
        bytes.[0] <- tag
        Buffer.BlockCopy(body, 0, bytes, 1, body.Length)
        bytes

    let private binary (tag: byte) (figure: int) (payload: byte[]) =
        let bytes = Array.zeroCreate (5 + payload.Length)
        bytes.[0] <- tag
        BinaryPrimitives.WriteInt32LittleEndian(Span(bytes, 1, 4), figure)
        Buffer.BlockCopy(payload, 0, bytes, 5, payload.Length)
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

    let private figureAndPayload (bytes: byte[]) =
        if bytes.Length < 5 then
            raise (ProtocolException $"message {bytes.[0]} is {bytes.Length} bytes, too short for a figure")

        BinaryPrimitives.ReadInt32LittleEndian(ReadOnlySpan(bytes, 1, 4)), bytes.[5..]

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
        | Reply.Scene(figure, payload) -> binary 66uy figure payload
        | Reply.Patch(figure, payload) -> binary 67uy figure payload
        | Reply.Controls m -> tagged 68uy m
        | Reply.Readout m -> tagged 69uy m
        | Reply.Output m -> tagged 70uy m
        | Reply.Exported(figure, payload) -> binary 71uy figure payload
        | Reply.Failed message -> tagged 72uy message

    let decodeReply (bytes: byte[]) : Reply =
        nonEmpty bytes

        match bytes.[0] with
        | 64uy -> Reply.Ready(body bytes)
        | 65uy -> Reply.Evaluated(body bytes)
        | 66uy -> Reply.Scene(figureAndPayload bytes)
        | 67uy -> Reply.Patch(figureAndPayload bytes)
        | 68uy -> Reply.Controls(body bytes)
        | 69uy -> Reply.Readout(body bytes)
        | 70uy -> Reply.Output(body bytes)
        | 71uy -> Reply.Exported(figureAndPayload bytes)
        | 72uy -> Reply.Failed(body bytes)
        | tag -> raise (ProtocolException $"no reply has the tag {tag}")
