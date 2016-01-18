﻿namespace GoPlusTests
// TODO
// 1. make all tests be situations that could really happen in the game


open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Pieces
open GameOptions
open Game
open Gameplay
open System
open Board

[<TestClass>]
type BoardActions() = 
    [<TestMethod>]
    member this.PlacePiece () = 
        let size = 9
        let game = new Game (size, { NeutralGen = false; PowerupGen = false }, Vanilla)

        Assert.IsTrue((game.AddPiece (Black, Big (3, 3)) (4, 4)) = ActionResponse.Accept)
        Assert.IsTrue((game.Board |> genCells).[1,1] = Cell.Taken Black)
    [<TestMethod>]
    member this.CalculateScoreNoScore () = 
        let size = 7
        let game = new Game (size, { NeutralGen = false; PowerupGen = false }, Vanilla)

        Assert.AreEqual(ActionResponse.Accept, game.AddPiece (Black, Big (1, 0)) (3, 3))
        Assert.AreEqual(ActionResponse.Accept, game.AddPiece (Black, Normal) (2, 4))
        Assert.AreEqual(ActionResponse.Accept, game.AddPiece (Black, Normal) (2, 5))
        Assert.AreEqual(ActionResponse.Accept, game.AddPiece (White, Normal) (4, 4))
        Assert.AreEqual(ActionResponse.Accept, game.AddPiece (White, Big (1, 0)) (4, 5))
        let (blackScore, whiteScore) = game.CalulateScore ()
        Assert.AreEqual(0, blackScore)
        Assert.AreEqual(0, whiteScore)

    [<TestMethod>]
    member this.CalculateScoreBlackScore () = 
        let size = 7
        let game = new Game (size, { NeutralGen = false; PowerupGen = false }, Vanilla)

        Assert.AreEqual(ActionResponse.Accept, game.AddPiece (Black, Big (1, 0)) (3, 3))
        Assert.AreEqual(ActionResponse.Accept, game.AddPiece (Black, Normal) (2, 4))
        Assert.AreEqual(ActionResponse.Accept, game.AddPiece (Black, Normal) (2, 5))
        Assert.AreEqual(ActionResponse.Accept, game.AddPiece (Black, Normal) (4, 4))
        Assert.AreEqual(ActionResponse.Accept, game.AddPiece (Black, Big (1, 0)) (4, 5))
        Assert.AreEqual(ActionResponse.Accept, game.AddPiece (White, Normal) (0, 0))
        let (blackScore, whiteScore) = game.CalulateScore ()
        Assert.AreEqual(1, blackScore)
        Assert.AreEqual(0, whiteScore)

    [<TestMethod>]
    member this.Ko () = 
        let size = 5
        let game = new Game (size, { NeutralGen = false; PowerupGen = false }, Vanilla)

        game.AddPiece (Black, Normal) (3, 3) |> ignore
        game.AddPiece (White, Normal) (2, 3) |> ignore
        game.AddPiece (Black, Normal) (2, 4) |> ignore
        game.AddPiece (White, Normal) (0, 3) |> ignore
        game.AddPiece (Black, Normal) (2, 2) |> ignore
        game.AddPiece (White, Normal) (1, 4) |> ignore
        game.AddPiece (Black, Normal) (0, 0) |> ignore
        game.AddPiece (White, Normal) (1, 2) |> ignore
        game.AddPiece (Black, Normal) (1, 3) |> ignore
        Assert.AreEqual(ActionResponse.Reject "Placing that piece would violate the ko rule", game.AddPiece (White, Normal) (2, 3))