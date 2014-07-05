open Pieces
open GameOptions
open Game
open System

let display board =
    let defaultColor = Console.ForegroundColor
    let size1 = Array2D.length1 board
    let size2 = Array2D.length2 board
    for i in 0 .. size1 - 1 do
        for j in 0 .. size2 - 1 do
            match board.[i,j] with
            | Taken Black -> Console.ForegroundColor <- ConsoleColor.DarkGray
            | Taken White -> Console.ForegroundColor <- ConsoleColor.White
            | Taken Neutral -> Console.ForegroundColor <- ConsoleColor.Gray
            | Free -> Console.ForegroundColor <- ConsoleColor.Black
            printf "O"
        printf "\n"

[<EntryPoint>]
let POO argv = 
    let size = 9
    let mutable testy = board size
    testy <- addPieces testy [ (Normal White, (0, 0)); (Normal White, (1, 0)); (Normal Neutral, (0, 1)); (Normal White, (0, 4)); (Normal Black, (1, 1)); (Normal Black, (2, 0)); (Normal Black, (0, 2)); ]
    let cells = genCells testy
    display cells
    Console.ForegroundColor <- ConsoleColor.White
    printf "%A" (checkDead cells Black)
    Console.ReadLine () |> ignore
    0
