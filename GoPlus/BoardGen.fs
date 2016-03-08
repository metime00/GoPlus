module BoardGen

open Pieces
open Board
open GameOptions
open Util

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