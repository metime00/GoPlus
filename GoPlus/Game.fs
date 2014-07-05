module Game
// Consolidation of all game logic, so any user interface's interaction with game logic is requesting actions by game and having them either accepted or rejected
// Game will check for validity of actions and carry them out itself, so the ui will have no business logic in it and be completely replaceable and still have the same functioning game
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
    let mutable board = generate genop size
    let playerBlack = new Player (Black)
    let playerWhite = new Player (White)
    /// Returns the game's board for displaying
    member this.Board = board
    /// Adds a piece to this location if it's valid, then checks for dead pieces, returning an ActionResponse to signal success or failure
    member this.AddPiece piece (x, y) =
        // put bounds checking, existing piece checking, optimality checking, and ko rule
        board <- addPieces board [ (piece, (x, y)) ]
        Accept
    member this.RemovePiece (x, y) =
        // check for if the coordinate selected has a piece that extends to it
        board <- removePieces board [ (x, y) ]
        Accept