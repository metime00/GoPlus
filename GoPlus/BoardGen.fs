module BoardGen

open Pieces
open Board
open GameOptions
open Util

let powerupChoose (rando : System.Random) =
    let randNum = rando.NextDouble ()
    match randNum with
    | _ when randNum > 0.80 -> Powerup.L
    | _ when randNum > 0.65 -> Powerup.Big (1, 1)
    | _ when randNum > 0.45 -> Powerup.Multiple ((rando.Next (2, 5)), true)
    | _ when randNum > 0.40 -> Powerup.Multiple ((rando.Next (1, 4)), false)
    | _ when randNum > 0.15 -> Powerup.Shuffle (rando.Next (1, 11))
    | _ -> Powerup.Conway

let generate seed genop powerop size =
    let randy = new System.Random (seed)
    let output = board size
    if genop.NeutralGen then
        let neutralGroups = [ for i = 0 to 1 + randy.Next(4) do yield (randy.Next(size), randy.Next(size)) ]
        let neutralSize = randy.Next(5 * (max (size / 10) 1))
        for (x, y) in neutralGroups do
            for i = 0 to neutralSize do
                for j = 0 to neutralSize do
                    if boundCheck (x + (i - neutralSize / 2), y + (j - neutralSize / 2)) size size
                       && randy.NextDouble () < 0.75 * ((float size) / 9.0) * normal (float (i - neutralSize / 2)) then
                        output.[x + (i - neutralSize / 2), y + (j - neutralSize / 2)] <- Some (Neutral, Normal)
    output