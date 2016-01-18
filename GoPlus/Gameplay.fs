module Gameplay

// Consolidation of all game logic, so any user interface's interaction with game logic is requesting actions by game and having them either accepted or rejected
// Game will check for validity of actions and carry them out itself, so the ui will have no business logic in it and be completely replaceable and still have the same functioning game
open Util
open Pieces
open Board
open GameOptions
open BoardGen
open Player
/// An error checking type that is returned by all commands to the game
type ActionResponse =
    | Accept
    | Reject of string

/// A type corresponding to any stage of the game. If you're given a state, you have enough info to play a game from that. If you have a list of consecutive states, you have the entire history of a game
type State = {
    seed : System.Random;
    black : Player;
    white : Player;
    board : Option<Piece>[,];
    powerups : PowerOption;
    nextToMove : Color } //next move says who is allowed to play a move next turn

/// A type corresponding to different actions that can be taken, such as placing a piece, removing a piece, passing, and removing all the stones deemed dead during scoring
type Move =
    | AddPiece of (Color * Shape) * (int * int) // piece, location
    | RemovePiece of (int * int)
    | Composite of Move list
    | MarkDead of (int * int) list
    | Pass

/// Returns the cells of a potential board after a piece is placed
let potentialBoard piece color (x, y) board =
    let preCheck = addPieces board [ (piece, (x, y)) ] // board with potential piece added before captures
    let postCheck = // board after the potential piece is placed
        preCheck
        |> genCells
        |> checkDead color // checks using the color placed so that if the piece placed is dead, it isn't taken out here before the next check for dead pieces of that color
        |> removePieces preCheck // if the placing of this piece would capture stones, the stones are captured here, so the piece placed isn't counted as dead if it captures a group
        |> genCells
    postCheck

let rec apply move state =
    printfn "%A" move
    match move with
    | AddPiece (piece, (x, y)) ->
        let temp = addPieces state.board [ (piece, (x, y)) ]
        let numDead =
            temp
            |> genCells
            |> checkDead state.nextToMove
        let newBoard =
            numDead
            |> List.filter (fun (x, y) -> state.board.[x,y] <> None)
            |> removePieces temp
        let addedScore = List.length numDead
        let newBlack = 
            if state.nextToMove = Color.Black then
                { color = Color.Black; score = state.black.score + addedScore; powerup = state.black.powerup }
            else
                state.black
        let newWhite = 
            if state.nextToMove = Color.White then
                { color = Color.White; score = state.white.score + addedScore; powerup = state.white.powerup }
            else
                state.white
        let next =
            match state.nextToMove with
            | Black -> Color.White
            | White -> Color.Black
        { seed = state.seed; black = newBlack; white = newWhite; board = newBoard; powerups = state.powerups; nextToMove = next }
    | RemovePiece (x, y) ->
        let size = Array2D.length1 state.board
        let mutable newBoard = None
        for i = 0 to size - 1 do
            for j = 0 to size - 1 do
                match state.board.[i,j] with
                | Some (_, shape) -> 
                    match pieceCoords shape (i, j) |> List.tryFind (fun p -> p = (x, y)) with
                    | Option.Some p ->
                        newBoard <- Some (removePieces state.board [ (i, j) ])
                    | Option.None -> ()
                | None -> ()
        let next =
            match state.nextToMove with
            | Black -> Color.White
            | White -> Color.Black
        match newBoard with
        | None -> 
            failwith "No piece at given location"
        | Some newBoard ->
            { seed = state.seed; black = state.black; white = state.white; board = newBoard; powerups = state.powerups; nextToMove = next }
    | Composite moves ->
        let rec recApply (moves : Move list) state =
            match moves.Tail with
            | [] -> apply moves.Head state
            | _ ->
                recApply moves.Tail (apply moves.Head state)
        recApply moves state
    | Pass ->
        let next =
            match state.nextToMove with
            | Black -> Color.White
            | White -> Color.Black
        { seed = state.seed; black = state.black; white = state.white; board = state.board; powerups = state.powerups; nextToMove = next }
        

let rec valid move state prevState =
    match move with
    | AddPiece (piece, (x, y)) ->
        let size = Array2D.length1 state.board
        let cells = genCells state.board
        let bounds =
            if List.filter (fun i -> boundCheck i size size = false) (pieceCoords (snd piece) (x, y)) = [] then Accept
            else Reject "Piece would be out of bounds"
        let existing = function
            | Accept ->
                if List.filter (fun (x, y) -> cells.[x,y] <> Free) (pieceCoords (snd piece) (x, y)) = [] then Accept
                else Reject "Piece already exists there"
            | Reject message -> Reject message
        let optimal = function
            | Accept ->
                let potential = potentialBoard piece state.nextToMove (x, y) state.board
                let lastColorDead =
                    potential
                    |> checkDead Neutral // color parameter is neutral because a neutral piece will never be placed
                    |> List.filter (fun (x, y) -> potential.[x,y] = Cell.Taken state.nextToMove) //only cares about the color who just placed a piece having dead groups
                if lastColorDead = [] then Accept
                else Reject "Placing a piece there would cause that piece to be dead"
            | Reject message -> Reject message
        let ko = function //prevState is the last state after the current player moved. AKA not the last state, but the one before it.
            | Accept ->
                match prevState with
                | None -> Accept
                | Some prevState ->
                    let newBoard = potentialBoard piece state.nextToMove (x, y) state.board
                    let oldBoard = prevState.board |> genCells
                    let diff =
                        [
                            for i = 0 to size - 1 do
                                for j = 0 to size - 1 do
                                    if newBoard.[i,j] <> oldBoard.[i,j] then yield oldBoard.[i,j]
                        ]
                    if diff <> [] then Accept
                    else Reject "Placing that piece would violate the ko rule"
            | Reject message -> Reject message
        bounds |> existing |> optimal |> ko //does a sequence of checks and returns whether or not a problem occured and where
    | RemovePiece (x, y) ->
        let size = Array2D.length1 state.board
        let mutable response = Reject "No piece at given location"
        for i = 0 to size - 1 do
            for j = 0 to size - 1 do
                match state.board.[i,j] with
                | Some (_, shape) -> 
                    match pieceCoords shape (i, j) |> List.tryFind (fun p -> p = (x, y)) with
                    | Option.Some p ->
                        response <- Accept
                    | Option.None -> ()
                | None -> ()
        response
    | Composite moves ->
        let rec recValid (moves : Move list) state prevStates =
            match moves.Tail with
            | [] -> valid moves.Head state prevStates
            | _ ->
                match valid moves.Head state prevState with
                | Accept -> recValid moves.Tail (apply moves.Head state) (Some state)
                | Reject message -> Reject message
        recValid moves state prevState
    | MarkDead coords ->
        Accept
    | Pass ->
        Accept