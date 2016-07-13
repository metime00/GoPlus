namespace GoPlusTests
// TODO
// 1. make all tests be situations that could really happen in the game


open System
open Encoding
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
    member this.PowerupNotObstruct () = 
        let size = 3
        let game = new Game (size, { NeutralGen = false }, Guaranteed, 0)
        game.Board.[0,0] <- Some (Pickup (Powerup.L), Shape.Normal)
        game.Board.[0,1] <- Some (Black, Shape.Normal)
        game.Board.[0,2] <- Some (White, Shape.Normal)
        game.Board.[2,0] <- Some (Pickup (Powerup.L), Shape.Normal)
        game.Board.[2,2] <- Some (Pickup (Powerup.L), Shape.Normal)
        Assert.AreEqual (game.MakeMoves [ (1, 1) ] , Accept ())
    
    [<TestMethod>]
    member this.IdenticalNeutralGeneration () =
        let size = 9
        let game1 = new Game (size, { NeutralGen = true }, Guaranteed, 0)
        let game2 = new Game (size, { NeutralGen = true }, Guaranteed, 0)
        
        for i = 0 to size - 1 do
            for j = 0 to size - 1 do
                Assert.AreEqual (game1.Board.[i,j], game2.Board.[i,j])

    [<TestMethod>]
    member this.IdenticalPowerupPlacement () =
        let size = 3
        let game1 = new Game (size, { NeutralGen = false }, Guaranteed, 0)
        let game2 = new Game (size, { NeutralGen = false }, Guaranteed, 0)
        
        Assert.AreEqual (game1.MakeMoves [ (1, 1) ] , Accept ())
        Assert.AreEqual (game2.MakeMoves [ (1, 1) ] , Accept ())

        for i = 0 to size - 1 do
            for j = 0 to size - 1 do
                Assert.AreEqual (game1.Board.[i,j], game2.Board.[i,j])

[<TestClass>]
type HelperFunctions() = 
    [<TestMethod>]
    member this.Hash () = 
        let game = new Game (3, { NeutralGen = false }, Vanilla, 0)
        game.Board.[0,0] <- Some (Pickup (Powerup.L), Shape.Normal)
        game.Board.[0,1] <- Some (Black, Shape.Normal)
        game.Board.[0,2] <- Some (White, Shape.Normal)
        game.Board.[2,0] <- Some (Pickup (Powerup.L), Shape.Normal)
        game.Board.[2,2] <- Some (Pickup (Powerup.L), Shape.Normal)
        let seed = 0
        let hash = genHash seed game.Board
        Assert.AreNotEqual(seed, hash)

[<TestClass>]
type EncodingDecoding() = 

    [<TestMethod>]
    member this.EncodingMove () = 
        let moves = [ (0, 5); (7, 2) ]
        Assert.AreEqual(moves, moves |> encode |> decode)

    [<TestMethod>]
    member this.EncodingGameInfo () = 
        let gameSize = 5
        let genop = { NeutralGen = false }
        let powerop = PowerOption.Low
        let seed = 5
        let encodeDecode = gameInfoToBytes gameSize genop powerop seed |> decodeGameInfo
        Assert.AreEqual((gameSize, genop, powerop, seed), encodeDecode)

[<TestClass>]
type GameActions() = 
    [<TestMethod>]
    member this.EnsureScoringMode () = 
        let size = 3
        let game = new Game (size, { NeutralGen = false }, PowerOption.Vanilla, 0)
        Assert.AreEqual (game.MakeMoves [ (1, 1) ], Accept ())
        Assert.AreEqual (game.MakeMoves [ (1, 2) ], Accept ())
        Assert.AreEqual (game.MakeMoves [ (0, 0) ], Accept ())
        Assert.AreEqual (game.Pass (), Accept ())
        Assert.AreEqual (game.Pass (), Accept ())
        Assert.AreEqual (game.Stage, Stage.Scoring)

    [<TestMethod>]
    member this.RevertGameState () = 
        let size = 3
        let game = new Game (size, { NeutralGen = false }, PowerOption.Vanilla, 0)
        Assert.AreEqual (game.MakeMoves [ (1, 1) ], Accept ())
        Assert.AreEqual (game.MakeMoves [ (1, 2) ], Accept ())
        Assert.AreEqual (game.MakeMoves [ (0, 0) ], Accept ())
        Assert.AreEqual (game.Pass (), Accept ())
        let next = game.NextToMove
        Assert.AreEqual (game.Pass (), Accept ())
        Assert.AreEqual (game.Stage, Stage.Scoring)
        game.RevertToPlay ()
        Assert.AreEqual (game.Stage, Stage.Play)
        Assert.AreEqual (game.NextToMove, next)