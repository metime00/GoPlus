namespace GoPlusTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Pieces
open GameOptions
open Game
open System
open Board

[<TestClass>]
type BoardActions() = 
    [<TestMethod>]
    member x.PlacePiece () = 
        let size = 9
        let game = new Game (size, { NeutralGen = false; PowerupGen = false }, Vanilla)

        Assert.IsTrue((game.AddPiece (Black, (Horizontal 1)) Black (1, 1)) = ActionResponse.Accept)
        Assert.IsTrue((game.Board |> genCells).[1,1] = Cell.Taken Black)