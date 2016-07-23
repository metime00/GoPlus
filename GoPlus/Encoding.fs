module Encoding

open Gameplay
open GameOptions
open Pieces
open System
open System.Net.Sockets

type GameMessage =
    | Moves of (int * int) list
    | Pass
    | Undo
    | Revert

/// encodes all the info necessary to generate a game as a 10 byte message
let gameInfoToBytes (gameSize : int) genOption powerOp (seed : int) =
    [|
        yield! BitConverter.GetBytes gameSize
        match genOption.NeutralGen with
        | false -> yield 0uy
        | true -> yield 1uy
        match powerOp with
        | PowerOption.Vanilla -> yield 0uy
        | PowerOption.Low -> yield 1uy
        | PowerOption.Medium -> yield 2uy
        | PowerOption.High -> yield 3uy
        | PowerOption.Guaranteed -> yield 4uy
        yield! BitConverter.GetBytes seed
    |]

let decodeGameInfo (bytes : byte []) =
    let gameSize = BitConverter.ToInt32 (bytes, 0)
    let neutralGen =
        match bytes.[4] with
        | 0uy -> false
        | 1uy -> true
        | _ -> failwith "unsupported value for finding neutralGen"
    let powerOp =
        match bytes.[5] with
            | 0uy -> PowerOption.Vanilla
            | 1uy -> PowerOption.Low
            | 2uy -> PowerOption.Medium
            | 3uy -> PowerOption.High
            | 4uy -> PowerOption.Guaranteed
            | _ -> failwith "unsupported value for finding powerOp"
    let seed = BitConverter.ToInt32 (bytes, 6)
    (gameSize, { NeutralGen = neutralGen }, powerOp, seed)

/// encodes a list of moves as an array of bytes for sending to the other player
let encode message =
    match message with
    | Pass -> [| 0uy |]
    | Revert -> [| 1uy |]
    | Undo -> [| 2uy |]
    | Moves moves ->
        [|
            yield 3uy
            for (x, y) in moves do
                yield! BitConverter.GetBytes x
                yield! BitConverter.GetBytes y
        |]

/// Takes a message and returns GameMessage that is the other player's turn
let decode (signal : byte []) =
    match signal.[0] with
    | 0uy -> Pass
    | 1uy -> Revert
    | 2uy -> Undo
    | 3uy ->
        let rec decode curMoves pos =
            if pos < Array.length signal then
                let x = BitConverter.ToInt32 (signal, pos)
                let y = BitConverter.ToInt32 (signal, pos + 4)
                decode ((x, y) :: curMoves) (pos + 8)
            else
                List.rev curMoves
        Moves (decode [] 1)
    | _ -> failwith "invalid GameMessage type"

