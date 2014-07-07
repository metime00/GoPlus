module Pieces

type Color =
    | Black
    | White
    | Neutral

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