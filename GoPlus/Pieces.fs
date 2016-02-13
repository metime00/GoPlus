module Pieces

type Powerup =
    | Big of (int * int)
    | Remove of int //how many pieces that can be removed
    /// the boolean is true when you place pieces of your own color and false when you place pieces of the opposing color
    | Multiple of int * bool //how many pieces you can place
    | L
    | Conway //simulates one step of conway's game of life
    /// move X% of the pieces in a random direction
    | Shuffle of int

and Color =
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