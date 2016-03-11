open Pieces
open GameOptions
open Game
open System
open System.Windows.Forms
open Board
open Menu

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
    
[<EntryPoint>]
let windowStart argv =
    let menu = new Menu (640, 480)
    menu.Show ()
    Application.Run ()
    0