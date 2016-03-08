module Powerup

open Pieces
open Board

//let powerupChoose (rando : System.Random) =
//    let randNum = rando.NextDouble ()
//    match randNum with
//    | _ when randNum > 0.80 -> Powerup.L
//    | _ when randNum > 0.65 -> Powerup.Big (1, 1)
//    | _ when randNum > 0.45 -> Powerup.Multiple ((rando.Next (2, 5)), true)
//    | _ when randNum > 0.40 -> Powerup.Multiple ((rando.Next (1, 4)), false)
//    | _ when randNum > 0.15 -> Powerup.Shuffle (rando.Next (1, 11))
//    | _ -> Powerup.Conway

let powerupChoose (rando : System.Random) =
    Powerup.Big (1, 1)

/// Simulates one tick of Conway's Game of Life for multiple color cells
let conway board =
    
    board

/// Shuffles some percent of the pieces
let shuffle percent board =

    board