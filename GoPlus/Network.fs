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
        | Color.Black -> yield 0uy
        | Color.White -> yield 1uy
        | Color.Neutral -> yield 2uy
        | _ -> failwith "should not be adding a powerup piece this way"

        match shape with
        | Shape.Normal -> yield 0uy
        | Shape.L -> yield 1uy
        | Shape.Big (xSize, ySize) ->
            yield 2uy
            yield! BitConverter.GetBytes xSize
            yield! BitConverter.GetBytes ySize
    |]

/// Encodes a move as bytes
let moveToBytes move =
    [|
        match move with
            | AddPiece (piece, (x, y)) ->
                        
                yield 0uy
                yield! pieceToBytes piece
                yield! BitConverter.GetBytes x
                yield! BitConverter.GetBytes y
            | RemovePiece (x, y) ->
                yield 1uy
                yield! BitConverter.GetBytes x
                yield! BitConverter.GetBytes y
            | MarkDead pieces ->
                yield 2uy
                yield! BitConverter.GetBytes (List.length pieces)
                yield!
                    [|
                        for (x, y) in pieces do
                            yield! BitConverter.GetBytes x
                            yield! BitConverter.GetBytes y
                    |]
            | Pass ->
                yield 3uy
            | Move.Conway ->
                yield 4uy
            | Move.Shuffle percent ->
                yield 5uy
                yield! BitConverter.GetBytes percent
    |]

/// encodes a list of moves as an array of bytes for sending to the other player
let encode (moves : Move list) =
    [|
        for i in moves do
            yield! moveToBytes i
    |]
   
///decodes a piece and returns a tuple of bytesRead * piece
let decodePiece startIndex (bytes : byte []) =
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

///decodes a move and returns a tuple of bytesRead * move
let decodeMove startIndex (bytes : byte []) = 
    match bytes.[startIndex] with
    | 0uy -> //addPiece
        let (pieceOffset, piece) = decodePiece (startIndex + 1) bytes
        let x = BitConverter.ToInt32 (bytes, startIndex + 1 + pieceOffset)
        let y = BitConverter.ToInt32 (bytes, startIndex + 5 + pieceOffset)
        (9 + pieceOffset, Move.AddPiece (piece, (x, y)))
    | 1uy -> //removePiece
        let x = BitConverter.ToInt32 (bytes, startIndex + 1)
        let y = BitConverter.ToInt32 (bytes, startIndex + 5)
        (9, Move.RemovePiece (x, y))
    | 2uy -> //markDead
        let length = BitConverter.ToInt32 (bytes, startIndex + 1)
        let rec convertCoords curList offset =
            if List.length curList < length then
                let x = BitConverter.ToInt32 (bytes, offset)
                let y = BitConverter.ToInt32 (bytes, offset + 4)
                convertCoords ((x, y) :: curList) (offset + 8)
            else
                List.rev curList
        let deadPieces = convertCoords [] (startIndex + 5)
        (2 + 8 * (List.length deadPieces), Move.MarkDead deadPieces)
    | 3uy -> //pass
        (1, Move.Pass)
    | 4uy -> //conway
        (1, Move.Conway)
    | 5uy -> //shuffle
        let percent = BitConverter.ToInt32 (bytes, startIndex + 1)
        (5, Move.Shuffle percent)
    | _ -> failwith "invalid byte for matching a shape to"
        
/// Takes a message and returns a list of moves that is the other player's turn
let decode signal =
    let rec decode curMoves pos =
        if pos < Array.length signal then
            let (offset, nextMove) = decodeMove pos signal
            decode (nextMove :: curMoves) (pos + offset)
        else
            List.rev curMoves
    decode [] 0

type SignalArgs (message) =
    member this.Message = message

let signalReceived = new Event<SignalArgs> ()

let listen (client : TcpClient) =
    let buffer = Array.zeroCreate 1024
    let stream = client.GetStream ()
    stream.Read (buffer, 0, Array.length buffer)

