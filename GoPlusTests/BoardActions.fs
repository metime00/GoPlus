namespace GoPlusTests
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
    member this.PowerupNotObstruct () = 
        let size = 3
        let game = new Game (size, { NeutralGen = false }, Guaranteed, 0)
        game.Board.[0,0] <- Some (Pickup (Powerup.L), Shape.Normal)
        game.Board.[0,1] <- Some (Black, Shape.Normal)
        game.Board.[0,2] <- Some (White, Shape.Normal)
        game.Board.[2,0] <- Some (Pickup (Powerup.L), Shape.Normal)
        game.Board.[2,2] <- Some (Pickup (Powerup.L), Shape.Normal)
        Assert.AreEqual (game.MakeMoves [ (1, 1) ] , Accept ())