module Network

open Gameplay
open Pieces
open System
open System.Net.Sockets

//networking works by one client sending an int of the number of coordinates being sent, or that a pass is being played, then the coordinates of the moves
//if the number of moves being sent is inconsistent with what the receiving client expects, or the moves provided are invalid, 
//the receiving client calls out the sending client as either a cheater or bugged

/// Encodes a piece to a byte, has the form color (1 byte) shape (1 byte) shapeArgs (0 or 8 bytes)
let pieceToBytes (color, shape) =
    [|
        match color with
        | Color.Black -> yield byte 0
        | Color.White -> yield byte 1
        | Color.Neutral -> yield byte 2
        | _ -> failwith "should not be adding a powerup piece this way"

        match shape with
        | Shape.Normal -> yield byte 0
        | Shape.L -> yield byte 1
        | Shape.Big (xSize, ySize) ->
            yield byte 2
            yield! BitConverter.GetBytes xSize
            yield! BitConverter.GetBytes ySize
    |]

let moveToByes move =
    [|
        match move with
            | AddPiece (piece, (x, y)) ->
                        
                yield byte 0
                yield! pieceToBytes piece
                yield! BitConverter.GetBytes x
                yield! BitConverter.GetBytes y
            | RemovePiece (x, y) ->
                yield byte 1
                yield! BitConverter.GetBytes x
                yield! BitConverter.GetBytes y
            | MarkDead pieces ->
                yield byte 2
                yield! BitConverter.GetBytes (List.length pieces)
                yield!
                    [|
                        for (x, y) in pieces do
                            yield! BitConverter.GetBytes x
                            yield! BitConverter.GetBytes y
                    |]
            | Pass ->
                yield byte 3
            | Move.Conway ->
                yield byte 4
            | Move.Shuffle percent ->
                yield byte 5
                yield! BitConverter.GetBytes percent
    |]

/// encodes a list of moves as an array of bytes for sending to the other player
let encode moves =
    let moveBytes =
        [|
            for i in moves do
                yield! i
        |]

    [|
        yield! BitConverter.GetBytes (Array.length moveBytes)
        yield! moveBytes
    |]
   
///decodes a piece and returns a tuple of bytesRead * piece
let decodePiece (bytes : byte []) startIndex =
    let col =
        match bytes.[startIndex] with
        | 0uy -> Color.Black
        | 1uy -> Color.White
        | 2uy -> Color.Neutral
        | _ -> failwith "invalid byte for matching a color to"
    let shape =
        match bytes.[startIndex + 1] with
        | 0uy -> Shape.Normal
        | 1uy -> Shape.L
        | 2uy ->
            let xSize = BitConverter.ToInt32 (bytes, startIndex + 2)
            let ySize = BitConverter.ToInt32 (bytes, startIndex + 6)
            Shape.Big (xSize, ySize)
        | _ -> failwith "invalid byte for matching a shape to"
    let bytesRead =
        match shape with
        | Big _ -> 10
        | _ -> 2
    (bytesRead, (col, shape))

/// Takes a message and returns a list of moves that is the other player's turn
let decode signal =
    ()

type SignalArgs (message) =
    member this.Message = message

let signalReceived = new Event<SignalArgs> ()

let listen (client : TcpClient) =
    let buffer = Array.zeroCreate 1024
    let stream = client.GetStream ()
    stream.Read (buffer, 0, Array.length buffer)

