module Player
open Util

type Player (color) =
    
    let mutable score = 0

    member this.Color = color

    member this.Score = score
    /// Adds captured pieces to the player's score
    member this.AddScore points = score <- score + points