open Pieces
open GameOptions
open Game
open System
open System.Windows.Forms
open Board
open Window

let display board =
    let defaultColor = Console.ForegroundColor
    let size1 = Array2D.length1 board
    let size2 = Array2D.length2 board
    for j = size2 - 1 downto 0 do
        for i = 0 to size1 - 1 do
            match board.[i,j] with
            | Taken Black -> Console.ForegroundColor <- ConsoleColor.DarkGray
            | Taken White -> Console.ForegroundColor <- ConsoleColor.White
            | Taken Neutral -> Console.ForegroundColor <- ConsoleColor.Gray
            | Free -> Console.ForegroundColor <- ConsoleColor.Black
            printf "O"
        printf "\n"

let POO argv = 
    let size = 9
    let game = new Game (size, { NeutralGen = false; PowerupGen = false }, Vanilla)

    game.AddPiece (Black, (Horizontal 1)) Black (1, 1) |> printfn "%A"
    game.Board |> genCells |> display
    Console.ReadLine () |> ignore
    Console.ForegroundColor <- ConsoleColor.White

    game.AddPiece (Black, Normal) Black (1, 0) |> printfn "%A"
    game.Board |> genCells |> display
    Console.ReadLine () |> ignore
    Console.ForegroundColor <- ConsoleColor.White

    game.AddPiece (White, (Horizontal 1)) White (1, 2) |> printfn "%A"
    game.Board |> genCells |> display
    Console.ReadLine () |> ignore
    Console.ForegroundColor <- ConsoleColor.White

    game.AddPiece (White, L) White (4, 0) |> printfn "%A"
    game.Board |> genCells |> display
    Console.ReadLine () |> ignore
    Console.ForegroundColor <- ConsoleColor.White

    game.AddPiece (White, Normal) White (3, 1) |> printfn "%A"
    game.Board |> genCells |> display
    Console.ReadLine () |> ignore
    Console.ForegroundColor <- ConsoleColor.White

    game.AddPiece (White, Normal) White (0, 0) |> printfn "%A"
    game.Board |> genCells |> display
    Console.ReadLine () |> ignore
    Console.ForegroundColor <- ConsoleColor.White

    game.AddPiece (White, Normal) White (6, 6) |> printfn "%A"
    game.Board |> genCells |> display
    Console.ReadLine () |> ignore
    Console.ForegroundColor <- ConsoleColor.White
    
    game.AddPiece (Black, Normal) Black (7, 6) |> printfn "%A"
    game.Board |> genCells |> display
    Console.ReadLine () |> ignore
    Console.ForegroundColor <- ConsoleColor.White

    game.AddPiece (White, Normal) White (5, 7) |> printfn "%A"
    game.Board |> genCells |> display
    Console.ReadLine () |> ignore
    Console.ForegroundColor <- ConsoleColor.White

    game.AddPiece (Black, Normal) Black (6, 7) |> printfn "%A"
    game.Board |> genCells |> display
    Console.ReadLine () |> ignore
    Console.ForegroundColor <- ConsoleColor.White

    game.AddPiece (Black, Normal) Black (6, 5) |> printfn "%A"
    game.Board |> genCells |> display
    Console.ReadLine () |> ignore
    Console.ForegroundColor <- ConsoleColor.White

    game.AddPiece (White, Normal) White (4, 6) |> printfn "%A"
    game.Board |> genCells |> display
    Console.ReadLine () |> ignore
    Console.ForegroundColor <- ConsoleColor.White

    game.AddPiece (White, Normal) White (5, 5) |> printfn "%A"
    game.Board |> genCells |> display
    Console.ReadLine () |> ignore
    Console.ForegroundColor <- ConsoleColor.White

    game.AddPiece (Black, Normal) Black (5, 6) |> printfn "%A"
    game.Board |> genCells |> display
    Console.ReadLine () |> ignore
    Console.ForegroundColor <- ConsoleColor.White

    game.AddPiece (White, Normal) White (6, 6) |> printfn "%A"
    game.Board |> genCells |> display
    Console.ReadLine () |> ignore
    Console.ForegroundColor <- ConsoleColor.White

    game.MarkDead (0, 0) |> printfn "%A"
    game.Board |> genCells |> display
    Console.ReadLine () |> ignore
    Console.ForegroundColor <- ConsoleColor.White

    game.CalulateScore ()
    game.Board |> genCells |> display
    Console.ReadLine () |> ignore
    Console.ForegroundColor <- ConsoleColor.White

    0
[<EntryPoint>]
let windowStart argv =
    let steve = new Window (19, 640, 480)
    steve.Show ()
    while steve.Visible do
        steve.Refresh ()
        Application.DoEvents ()
    0
    //POO ()