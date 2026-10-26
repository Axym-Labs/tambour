namespace Tambour.Protocol

open System
open System.Buffers.Binary
open System.IO
open System.Threading
open System.Threading.Tasks

/// Messages on a stream: each one its length (four bytes, little-endian), then its bytes.
module Framing =

    /// The largest message either side accepts. A scene of a million points is some tens of MB.
    [<Literal>]
    let MaxLength = 1_073_741_824

    let private lengthOf (header: byte[]) =
        let n = BinaryPrimitives.ReadInt32LittleEndian(ReadOnlySpan header)

        if n <= 0 || n > MaxLength then
            raise (ProtocolException $"a frame of {n} bytes")

        n

    let private header (length: int) =
        let h = Array.zeroCreate 4
        BinaryPrimitives.WriteInt32LittleEndian(Span h, length)
        h

    /// Reads exactly `buffer.Length` bytes. False if the stream ended before the first byte;
    /// an end after it is a truncated frame.
    let private fill (stream: Stream) (buffer: byte[]) =
        let mutable read = 0
        let mutable ended = false

        while not ended && read < buffer.Length do
            let n = stream.Read(buffer, read, buffer.Length - read)
            if n = 0 then ended <- true else read <- read + n

        if ended && read > 0 then
            raise (ProtocolException "the stream ended inside a frame")

        not ended

    let private fillAsync (stream: Stream) (buffer: byte[]) (ct: CancellationToken) =
        task {
            let mutable read = 0
            let mutable ended = false

            while not ended && read < buffer.Length do
                let! n = stream.ReadAsync(buffer.AsMemory(read, buffer.Length - read), ct)
                if n = 0 then ended <- true else read <- read + n

            if ended && read > 0 then
                raise (ProtocolException "the stream ended inside a frame")

            return not ended
        }

    let write (stream: Stream) (message: byte[]) : unit =
        stream.Write(header message.Length, 0, 4)
        stream.Write(message, 0, message.Length)
        stream.Flush()

    let writeAsync (stream: Stream) (message: byte[]) (ct: CancellationToken) : Task =
        task {
            do! stream.WriteAsync(header message.Length, ct)
            do! stream.WriteAsync(message, ct)
            do! stream.FlushAsync ct
        }

    /// The next message, or `None` where the stream ends between messages.
    let read (stream: Stream) : byte[] option =
        let h = Array.zeroCreate 4

        if not (fill stream h) then
            None
        else
            let message = Array.zeroCreate (lengthOf h)

            if not (fill stream message) then
                raise (ProtocolException "the stream ended inside a frame")

            Some message

    let readAsync (stream: Stream) (ct: CancellationToken) : Task<byte[] option> =
        task {
            let h = Array.zeroCreate 4
            let! more = fillAsync stream h ct

            if not more then
                return None
            else
                let message = Array.zeroCreate (lengthOf h)
                let! complete = fillAsync stream message ct

                if not complete then
                    raise (ProtocolException "the stream ended inside a frame")

                return Some message
        }

/// One end of a connection. Sending is safe from any thread, since a worker writes the script's
/// output while it answers requests; receiving is for one reader at a time.
type Connection(stream: Stream) =
    let gate = new SemaphoreSlim(1, 1)

    member _.Stream = stream

    member _.Send(message: byte[]) : unit =
        gate.Wait()

        try
            Framing.write stream message
        finally
            gate.Release() |> ignore

    member _.SendAsync(message: byte[], ct: CancellationToken) : Task =
        task {
            do! gate.WaitAsync ct

            try
                do! Framing.writeAsync stream message ct
            finally
                gate.Release() |> ignore
        }

    member _.Receive() : byte[] option = Framing.read stream

    member _.ReceiveAsync(ct: CancellationToken) : Task<byte[] option> = Framing.readAsync stream ct

    interface IDisposable with
        member _.Dispose() =
            stream.Dispose()
            gate.Dispose()
