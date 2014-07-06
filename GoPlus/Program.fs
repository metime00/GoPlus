open Pieces
open GameOptions
open Game
open System
open Board

let display board =
    let defaultColor = Console.ForegroundColor
    let size1 = Array2D.length1 board
    let size2 = Array2D.length2 board
    for i = size2 - 1 downto 0 do
        for j = 0 to size1 - 1 do
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
    let game = new Game (size, Regular, Vanilla)

    game.AddPiece (Horizontal (Black, 1)) Black (1, 1) |> printfn "%A"
    game.Board |> genCells |> display
    Console.ReadLine () |> ignore
    Console.ForegroundColor <- ConsoleColor.White

    game.AddPiece (Normal Black) Black (1, 0) |> printfn "%A"
    game.Board |> genCells |> display
    Console.ReadLine () |> ignore
    Console.ForegroundColor <- ConsoleColor.White

    game.AddPiece (Horizontal (White, 1)) White (1, 2) |> printfn "%A"
    game.Board |> genCells |> display
    Console.ReadLine () |> ignore
    Console.ForegroundColor <- ConsoleColor.White

    game.AddPiece (L White) White (4, 0) |> printfn "%A"
    game.Board |> genCells |> display
    Console.ReadLine () |> ignore
    Console.ForegroundColor <- ConsoleColor.White

    game.AddPiece (Normal White) White (3, 1) |> printfn "%A"
    game.Board |> genCells |> display
    Console.ReadLine () |> ignore
    Console.ForegroundColor <- ConsoleColor.White

    game.AddPiece (Normal White) White (0, 0) |> printfn "%A"
    game.Board |> genCells |> display
    Console.ReadLine () |> ignore
    0
