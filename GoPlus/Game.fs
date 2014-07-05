module Game
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

type Game (size, genop, powerop) =
    let oldBoards = new System.Collections.Generic.List<Piece[,]>()
    let mutable board = generate genop size
    let mutable cells = genCells board
    let playerBlack = new Player (Black)
    let playerWhite = new Player (White)
    /// The function to advance the board to the next state, should be the only way the board is changed
    let changeBoard newBoard =
        oldBoards.Add board
        board <- newBoard
        cells <- genCells board

    /// Returns the game's board for displaying
    member this.Board = board

    /// Adds a piece to this location if it's valid, then checks for dead pieces using the given color, returning an ActionResponse to signal success or failure
    member this.AddPiece piece color (x, y) =
        // add bounds checking, existing piece checking, optimality checking, and ko rule
        let bounds =
            if List.filter (fun i -> boundCheck i size size = false) (pieceCoords piece (x, y)) = [] then Accept
            else Reject "Piece would be out of bounds"
        let existing = function
            | Accept ->
                if List.filter (fun (x, y) -> cells.[y,x] <> Free) (pieceCoords piece (x, y)) = [] then Accept
                else Reject "Piece already exists there"
            | Reject message -> Reject message
        let optimal = function
            | Accept ->
                let potential =
                    addPieces board [ (piece, (x, y)) ]
                    |> genCells
                let lastColorDead = 
                    potential
                    |> checkDead Neutral //color parameter is neutral because a neutral piece will never be placed
                    |> List.filter (fun (x, y) -> potential.[y,x] = Cell.Taken color) //only cares about the color who just placed a piece having dead groups
                if lastColorDead = [] then Accept
                else Reject "Placing a piece there would cause that piece to be dead"
            | Reject message -> Reject message
        let ko = function
            | Accept ->
                let newBoard = addPieces board [ (piece, (x, y)) ]
                let diff =
                    [
                        for i = 0 to size - 1 do
                            for j = 0 to size - 1 do
                                if newBoard.[j,i] <> board.[j,i] then yield board.[j,i]
                    ]
                if diff <> [] then Accept
                else Reject "Placing that piece would violate the ko rule"
            | Reject message -> Reject message
        let success = function
            | Accept -> //only performs modifications if all checks succeeded
                let temp = addPieces board [ (piece, (x, y)) ]
                temp
                |> genCells
                |> checkDead color
                |> List.filter (fun (x, y) -> board.[y,x] <> None)
                |> removePieces temp
                |> changeBoard
                Accept
            | Reject message -> Reject message
        bounds |> existing |> optimal |> ko |> success //does a sequence of checks and returns whether or not a problem occured and where

    /// Removes a piece if it exists at the given location
    member this.RemovePiece (x, y) =
        let mutable response = Reject "No piece at given location"
        for i = 0 to size - 1 do
            for j = 0 to size - 1 do
                match pieceCoords board.[j,i] (i, j) |> List.tryFind (fun p -> p = (x, y)) with
                    | Option.Some p ->
                        changeBoard (removePieces board [ (i, j) ])
                        response <- Accept
                    | Option.None -> ()
        response