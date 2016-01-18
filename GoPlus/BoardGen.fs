module BoardGen

open Pieces
open Board
open GameOptions

let generate seed genop powerop size =
    board size