module Network

open Gameplay
open System
open System.Net.Sockets

//networking works by one client sending an int of the number of coordinates being sent, or that a pass is being played, then the coordinates of the moves
//if the number of moves being sent is inconsistent with what the receiving client expects, or the moves provided are invalid, 
//the receiving client calls out the sending client as either a cheater or bugged

/// encodes a list of moves as an array of bytes for sending to the other player
let encode moves =
    [|
        for i in moves do
            match i with
                | AddPiece (piece, (x, y)) -> ()
                | RemovePiece (x, y) -> ()
                | MarkDead pieces -> ()
                | Pass -> ()
    |]
   
/// recursively pull off one move from the signal bytes and add it to the list of moves
let rec decode signal =
    ()

type SignalArgs (message) =
    member this.Message = message

let signalReceived = new Event<SignalArgs> ()

let listen (client : TcpClient) =
    let buffer = Array.zeroCreate 1024
    let stream = client.GetStream ()
    stream.Read (buffer, 0, Array.length buffer)

