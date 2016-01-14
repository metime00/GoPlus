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
    member this.PlacePiece () = 
        let size = 9
        let game = new Game (size, { NeutralGen = false; PowerupGen = false }, Vanilla)

        Assert.IsTrue((game.AddPiece (Black, Big (3, 3)) Black (4, 4)) = ActionResponse.Accept)
        Assert.IsTrue((game.Board |> genCells).[1,1] = Cell.Taken Black)
    [<TestMethod>]
    member this.CalculateScoreNoScore () = 
        let size = 7
        let game = new Game (size, { NeutralGen = false; PowerupGen = false }, Vanilla)

        Assert.AreEqual(ActionResponse.Accept, game.AddPiece (Black, Big (1, 0)) Black (3, 3))
        Assert.AreEqual(ActionResponse.Accept, game.AddPiece (Black, Normal) Black (2, 4))
        Assert.AreEqual(ActionResponse.Accept, game.AddPiece (Black, Normal) Black (2, 5))
        Assert.AreEqual(ActionResponse.Accept, game.AddPiece (White, Normal) White (4, 4))
        Assert.AreEqual(ActionResponse.Accept, game.AddPiece (White, Big (1, 0)) White (4, 5))
        game.CalulateScore ()
        Assert.AreEqual(0, game.GetScore Black)
        Assert.AreEqual(0, game.GetScore White)

    [<TestMethod>]
    member this.CalculateScoreBlackScore () = 
        let size = 7
        let game = new Game (size, { NeutralGen = false; PowerupGen = false }, Vanilla)

        Assert.AreEqual(ActionResponse.Accept, game.AddPiece (Black, Big (1, 0)) Black (3, 3))
        Assert.AreEqual(ActionResponse.Accept, game.AddPiece (Black, Normal) Black (2, 4))
        Assert.AreEqual(ActionResponse.Accept, game.AddPiece (Black, Normal) Black (2, 5))
        Assert.AreEqual(ActionResponse.Accept, game.AddPiece (Black, Normal) Black (4, 4))
        Assert.AreEqual(ActionResponse.Accept, game.AddPiece (Black, Big (1, 0)) Black (4, 5))
        Assert.AreEqual(ActionResponse.Accept, game.AddPiece (White, Normal) White (0, 0))
        game.CalulateScore ()
        Assert.AreEqual(1, game.GetScore Black)
        Assert.AreEqual(0, game.GetScore White)

    [<TestMethod>]
    member this.Ko () = 
        let size = 5
        let game = new Game (size, { NeutralGen = false; PowerupGen = false }, Vanilla)

        game.AddPiece (Black, Normal) Black (3, 3) |> ignore
        game.AddPiece (Black, Normal) Black (2, 4) |> ignore
        game.AddPiece (Black, Normal) Black (2, 2) |> ignore
        game.AddPiece (White, Normal) White (2, 3) |> ignore
        game.AddPiece (White, Normal) White (0, 3) |> ignore
        game.AddPiece (White, Normal) White (1, 4) |> ignore
        game.AddPiece (White, Normal) White (1, 2) |> ignore
        game.AddPiece (Black, Normal) Black (1, 3) |> ignore
        Assert.AreEqual(ActionResponse.Reject "Placing that piece would violate the ko rule", game.AddPiece (White, Normal) White (2, 3))