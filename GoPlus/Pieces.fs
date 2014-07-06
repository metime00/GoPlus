module Pieces

type Color =
    | Black
    | White
    | Neutral

type Cell =
    | Taken of Color
    | Free

/// A piece in the game of GoPlus, where the int in some pieces is extension out from the center
type Piece =
    | Normal of Color
    | Vertical of Color * int
    | Horizontal of Color * int
    | Big of Color * int
    | L of Color
    | None