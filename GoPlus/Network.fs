module Network

open Gameplay
open GameOptions
open Pieces
open Encoding
open System
open System.Net.Sockets

//networking works by one client sending an int of the number of coordinates being sent, or that a pass is being played, then the coordinates of the moves
//if the number of moves being sent is inconsistent with what the receiving client expects, or the moves provided are invalid, 
//the receiving client calls out the sending client as either a cheater or bugged

type NetworkArgs =
    {
        Client : TcpClient
        PlayerColor : Color
    }

type SignalArgs (moveCoords) =
    member this.MoveCoords : (int * int) list = moveCoords

/// Method for a listener thread, that waits for a move to be sent and then triggers the event for receiving a signal
let listen (client : TcpClient) (event : Event<SignalArgs>) () =
    let sizeBuffer = Array.zeroCreate 4
    let stream = client.GetStream ()
    if stream.Read (sizeBuffer, 0, Array.length sizeBuffer) <> Array.length sizeBuffer then
        failwith "couldn't read enough bytes to find the byte length of the message"
    let buffer = Array.zeroCreate (BitConverter.ToInt32 (sizeBuffer, 0))
    if stream.Read (buffer, 0, Array.length buffer) <> Array.length buffer then
        failwith "couldn't read enough bytes to read the whole message"
    let moves = decode buffer
    event.Trigger (new SignalArgs (moves))

let sendMoves (client : TcpClient) moves =
    let stream = client.GetStream ()
    let message = encode moves
    stream.Write (message, 0, Array.length message)