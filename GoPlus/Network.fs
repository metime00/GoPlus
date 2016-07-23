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

type SignalArgs (message) =
    inherit EventArgs ()
    member this.Message = message

/// Method for a listener thread, that loops and waits for a move to be sent and then triggers the event for receiving a signal
let listen (client : TcpClient) (event : Event<_>) () =
    while true do
        printfn "I'm waiting for moves"
        let sizeBuffer = Array.zeroCreate 4
        let stream = client.GetStream ()
        if stream.Read (sizeBuffer, 0, Array.length sizeBuffer) <> Array.length sizeBuffer then
            failwith "couldn't read enough bytes to find the byte length of the message"
        let message =
            let x = BitConverter.ToInt32 (sizeBuffer, 0)
            let buffer = Array.zeroCreate (x)
            if stream.Read (buffer, 0, Array.length buffer) <> Array.length buffer then
                failwith "couldn't read enough bytes to read the whole message"
            printfn "just received the other player's move, %i bytes" x
            decode buffer
        let moves = match message with | Moves moves -> moves | _ -> failwith "shouldn't send game message with anything other than moves yet"
        event.Trigger (new SignalArgs (moves))

let sendMoves (client : TcpClient) moves =
    printfn "I'm sending my moves"
    let stream = client.GetStream ()
    let message = encode (Moves moves)
    let messageLengthBytes = BitConverter.GetBytes (Array.length message)
    stream.Write (messageLengthBytes, 0, Array.length messageLengthBytes)
    stream.Write (message, 0, Array.length message)