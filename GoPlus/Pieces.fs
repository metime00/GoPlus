module Pieces

type Powerup =
    | Big of int * int
    | Remove of int //how many pieces that can be removed
    | Multiple of int //how many pieces you can place
    | L
    | Conway //simulates one step of conway's game of life
    | Shuffle of int //move X% of the pieces in a random direction

type Color =
    | Black
    | White
    | Neutral
    | Pickup of Powerup

type Cell =
    | Taken of Color
    | Free

type Shape =
    | Normal
    | Big of int * int
    | L

/// A piece in the game of GoPlus, where the int in some pieces is extension out from the center
type Piece = Color * Shape