module BoardGen

open Pieces
open Board
open GameOptions
open Util

let generate (seed : System.Random) genop powerop size =
    let output = board size
    if genop.NeutralGen then
        let neutralGroups = [ for i = 0 to 1 + seed.Next(4) do yield (seed.Next(size), seed.Next(size)) ]
        let neutralSize = seed.Next(5)
        for (x, y) in neutralGroups do
            for i = 0 to neutralSize do
                for j = 0 to neutralSize do
                    if boundCheck (x + (i - neutralSize / 2), y + (j - neutralSize / 2)) size size
                       && seed.NextDouble () < 0.75 * normal (float (i - neutralSize / 2)) then
                        output.[x + (i - neutralSize / 2), y + (j - neutralSize / 2)] <- Some (Neutral, Normal)
    output