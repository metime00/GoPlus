module Powerup

open Pieces
open Board
open Util

let powerupChoose (rando : System.Random) =
    let randNum = rando.NextDouble ()
    match randNum with
    | _ when randNum > 0.80 -> Powerup.L
    | _ when randNum > 0.65 -> Powerup.Big (1, 1)
    | _ when randNum > 0.45 -> Powerup.Multiple ((rando.Next (2, 5)), true)
    | _ when randNum > 0.40 -> Powerup.Multiple ((rando.Next (1, 4)), false)
    | _ when randNum > 0.15 -> Powerup.Shuffle (rando.Next (5, 21))
    | _ -> Powerup.Conway

/// Simulates one tick of Conway's Game of Life for multiple color cells. Converts all special pieces into normal sized pieces
let conway board =
    let size = Array2D.length1 board
    let cells = genCells board
    let outBoard = Board.board size
    let neighbors (x, y) =
        [
            for i = x - 1 to x + 1 do
                for j = y - 1 to y + 1 do
                    yield (i, j)
        ]
    for i = 0 to size - 1 do
        for j = 0 to size - 1 do
            //all neighbors that are in bounds and not dead cells
            let adjacent = neighbors (i, j) |> List.filter (fun (x, y) -> boundCheck (x, y) size size && cells.[x,y] <> Cell.Free)
            match cells.[i,j] with
            | Cell.Taken col ->
                if List.length adjacent > 1 && List.length adjacent < 4 then
                    //a cell is only alive if it has two or three live neighbors, it keeps its current color
                    outBoard.[i,j] <- Some (col, Shape.Normal)
            | Cell.Free ->
                //if there are exactly three adjacent live cells to a dead cell, it is alive
                if List.length adjacent = 3 then
                    //newly born cell is the color of the majority of its neighbor cells
                    let majorityColor =
                        let adjacentColors =
                            adjacent 
                            |> List.map (fun (x, y) -> cells.[x, y])
                        adjacentColors |> List.maxBy (fun x -> adjacentColors |> List.filter (fun y -> y = x) |> List.length)
                    match majorityColor with
                    | Cell.Taken col ->
                        outBoard.[i,j] <- Some (col, Shape.Normal)
                    | Cell.Free -> failwith "free cells should've been filtered from the neighbors list"
    outBoard

/// Shuffles some percent of the pieces
let shuffle percent board seed =
    let rando = new System.Random (seed)
    let size = Array2D.length1 board
    let pieces =
        [
            for i = 0 to size - 1 do
                for j = 0 to size - 1 do
                    match board.[i,j] with
                    | Some piece -> yield (i, j)
                    | None -> ()
        ]
    let pieceCount = List.length pieces
    let shuffleTimes = max ((pieceCount * percent) / 100) 1
    let rec shuffle (board : Option<Piece> [,]) coords timesLeft =
        // if the algorithm runs out of pieces to shuffle, or it's shuffled as many pieces as it was supposed to, return the board
        if timesLeft = 0 || coords = [] then
            board
        else //if there's still stuff to shuffle and it still has shuffles left, start shuffling
            let (x, y) = List.nth coords (rando.Next (List.length coords))
            let (color, shape) = 
                match board.[x,y] with
                | Some x -> x
                | None -> failwith "empty squares should have been filtered out"
            let candidateShifts =
                [(x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1)]
                |> List.filter (fun z -> boundCheck z size size)
                |> List.filter (fun z -> intersectingPieces (pieceCoords shape z) board = []) //only allow shuffles to locations where the piece wouldn't intersect another
            if candidateShifts = [] then
                shuffle board (List.filter (fun z -> z <> (x, y)) coords) timesLeft
            else
                let newCoord = List.nth candidateShifts (rando.Next (List.length candidateShifts))
                let newBoard =
                    addPieces (removePieces board [ (x, y) ]) [ ((color, shape), newCoord) ]
                shuffle newBoard (List.filter (fun z -> z <> (x, y)) coords) (timesLeft - 1)
            

    shuffle board pieces shuffleTimes