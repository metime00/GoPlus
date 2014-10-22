module Pieces

type Powerup =
    | Big
    | Remove of int //how many pieces that can be removed
    | Vertical
    | Horizontal
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
    | Vertical of int
    | Horizontal of int
    | Big of int
    | L

/// A piece in the game of GoPlus, where the int in some pieces is extension out from the center
type Piece = Color * Shape