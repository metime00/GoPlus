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
type ActionResponse<'T> =
    | Accept of 'T
    | Reject of string

/// A type corresponding to any stage of the game. If you're given a state, you have enough info to play a game from that. If you have a list of consecutive states, you have the entire history of a game
type State = {
    seed : int;
    black : Player;
    white : Player;
    board : Option<Piece>[,];
    powerups : PowerOption;
    nextToMove : Color } //next move says who is allowed to play a move next turn

/// A type corresponding to different actions that can be taken, such as placing a piece, removing a piece, passing, and removing all the stones deemed dead during scoring
type Move =
    | AddPiece of (Color * Shape) * (int * int) // piece, location
    | RemovePiece of (int * int)
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

/// Takes a seed and a board and returns a unique hash from them
let genHash seed (board : Option<Piece> [,]) =
    let values = 
        [ 
            for i = 0 to Array2D.length1 board - 1 do 
                for j = 0 to Array2D.length2 board - 1 do
                    match board.[i,j] with
                    | None -> ()
                    | Some x -> yield x.GetHashCode ()
        ]
    List.fold (fun cur next -> (cur ^^^ next) >>> 1) seed values

let rec perform (moves : Move list) state = 
    let nextState =
        match moves.Head with
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
            let newPowerup =
                let coord = List.tryFind (fun (x, y) -> match state.board.[x, y] with Some (Pickup _, Normal) -> true | _ -> false) numDead
                match coord with
                | Some (x, y) -> 
                    match Option.get state.board.[x,y] |> fst with
                    | Pickup power -> Some power
                    | _ -> failwith "not given a powerup"
                | None -> None
            let newBlack = 
                if state.nextToMove = Color.Black then
                    { color = Color.Black; score = state.black.score + addedScore; powerup = newPowerup }
                else
                    state.black
            let newWhite = 
                if state.nextToMove = Color.White then
                    { color = Color.White; score = state.white.score + addedScore; powerup = newPowerup }
                else
                    state.white
            { seed = state.seed; black = newBlack; white = newWhite; board = newBoard; powerups = state.powerups; nextToMove = state.nextToMove }
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
            match newBoard with
            | None -> 
                failwith "No piece at given location"
            | Some newBoard ->
                { seed = state.seed; black = state.black; white = state.white; board = newBoard; powerups = state.powerups; nextToMove = state.nextToMove }
        | Pass ->
            { seed = state.seed; black = state.black; white = state.white; board = state.board; powerups = state.powerups; nextToMove = state.nextToMove }
        | MarkDead pieces ->
            let mutable blackScoreDelta = 0
            let mutable whiteScoreDelta = 0
            for (x, y) in pieces do
                let (color, shape) =
                    match state.board.[x,y] with
                    | Some piece -> piece
                    | None -> failwith "piece expected"
                match color with
                | White ->
                    blackScoreDelta <- blackScoreDelta + (List.length (pieceCoords shape (x, y)))
                | Black ->
                    whiteScoreDelta <- whiteScoreDelta + (List.length (pieceCoords shape (x, y)))
                | _ -> ()
            let newBoard = removePieces state.board pieces
            let newBlack = { color = Color.Black; score = state.black.score + blackScoreDelta; powerup = state.black.powerup }
            let newWhite = { color = Color.White; score = state.white.score + whiteScoreDelta; powerup = state.white.powerup }
            { seed = state.seed; black = newBlack; white = newWhite; board = newBoard; powerups = state.powerups; nextToMove = Color.Neutral }
    match moves.Tail with
    | [] -> nextState
    | tail -> perform moves.Tail nextState

let rec valid (moves : Move list) state prevState =
    let response =
        match moves.Head with
        | AddPiece (piece, (x, y)) ->
            let size = Array2D.length1 state.board
            let cells = genCells state.board
            let bounds =
                if List.filter (fun i -> boundCheck i size size = false) (pieceCoords (snd piece) (x, y)) = [] then Accept ()
                else Reject "Piece would be out of bounds"
            let existing = function
                | Accept () ->
                    if List.filter (fun (x, y) -> cells.[x,y] <> Free) (pieceCoords (snd piece) (x, y)) = [] then Accept ()
                    else Reject "Piece already exists there"
                | Reject message -> Reject message
            let optimal = function
                | Accept () ->
                    if x = 1 && y = 2 && fst piece = Color.Neutral then
                        printf "ahhhhh"
                    let enclosingColor =
                        match state.nextToMove with
                        | Black -> White
                        | White -> Black
                    let conditionalColor =
                        if fst piece = Neutral then
                            Neutral //if placing a neutral piece for testing, check for dead pieces that are neutral
                        else
                            state.nextToMove //only cares about the color who just placed a piece having dead groups
                    let potential = potentialBoard piece conditionalColor (x, y) state.board
                    let lastColorDead =
                        potential
                        |> checkDead enclosingColor // color parameter is the opposite of the current mover
                        |> List.filter (fun (x, y) -> potential.[x,y] = Cell.Taken conditionalColor)
                    if lastColorDead = [] then Accept ()
                    else Reject "Placing a piece there would cause that piece to be dead"
                | Reject message -> Reject message
            let ko = function //prevState is the last state after the current player moved. AKA not the last state, but the one before it.
                | Accept () ->
                    match prevState with
                    | None -> Accept ()
                    | Some prevState ->
                        let newBoard = potentialBoard piece state.nextToMove (x, y) state.board
                        let oldBoard = prevState.board |> genCells
                        let diff =
                            [
                                for i = 0 to size - 1 do
                                    for j = 0 to size - 1 do
                                        if newBoard.[i,j] <> oldBoard.[i,j] then yield oldBoard.[i,j]
                            ]
                        if diff <> [] then Accept ()
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
                            response <- Accept ()
                        | Option.None -> ()
                    | None -> ()
            response
        | MarkDead coords ->
            Accept ()
        | Pass ->
            Accept ()
    match response with
    | Accept () ->
        match moves.Tail with
        | [] -> Accept ()
        | tail ->
            valid tail (perform [moves.Head] state) prevState
    | Reject message->
        Reject message

let apply (moves : Move list) state =
    printfn "%A" moves
    let newState = perform moves state
    let nextColor =
        match state.nextToMove with
        | Black -> Color.White
        | White -> Color.Black
        | Neutral -> Color.Neutral
    
    //do random powerup generation, using the propogated seed
    let randy = new System.Random (genHash state.seed state.board)
    let powerupedBoard =
        let threshold =
            match state.powerups with
            | Vanilla -> 0.0
            | Low -> 0.029513
            | Medium -> 0.058155
            | High -> 0.112928
        if threshold = 0.0 || randy.NextDouble () >= threshold then
            newState.board
        else
            let emptySquares =
                [ for i = 0 to Array2D.length1 newState.board - 1 do for j = 0 to Array2D.length1 newState.board - 1 do yield (i, j) ] 
                |> List.filter (fun (x, y) -> valid [ AddPiece ((Neutral, Normal), (x, y)) ] newState None = Accept () ) //only choose from the coordinates that it's possible to place a piece on
            if emptySquares <> [] then
                // pick in a way that favors the center of the board, but if there are 15 iterations without succeeding in placing the piece with the normal distribution around the center
                // then just pick the next generated coordinate to place the powerup
                let rec pickCoord coords curStep =
                    let (x, y) = List.nth coords (randy.Next (List.length coords))
                    let size = (Array2D.length1 newState.board) / 2
                    if randy.NextDouble () < normal (float (x - size)) || randy.NextDouble () < normal (float (y - size)) then
                        (x, y)
                    elif curStep > 15 then
                        (x, y)
                    else
                        pickCoord coords (curStep + 1)
                addPieces newState.board [ ((Pickup (powerupChoose randy), Normal), pickCoord emptySquares 0) ]
            else
                newState.board

    { seed = newState.seed; black = newState.black; white = newState.white; board = powerupedBoard; powerups = newState.powerups; nextToMove = nextColor }