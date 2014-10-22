module Player
open Util
open Pieces

type Player (color) =
    
    let mutable score = 0

    let maxPowerups = 5

    let powerups : Option<Powerup>[] = Array.create maxPowerups None

    member this.Color = color

    member this.Score = score
    /// Adds captured pieces to the player's score
    member this.AddScore points = score <- score + points
    /// Adds a powerup to the powerups unless it's full
    member this.AddPowerUp power =
        let rec addPower power place =
            if powerups.[place] <> None then powerups.[place] <- Some power
            elif place + 1 < maxPowerups then addPower power (place + 1)
        addPower power 0
    member this.Powerups = powerups