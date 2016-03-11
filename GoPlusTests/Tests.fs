namespace GoPlusTests
// TODO
// 1. make all tests be situations that could really happen in the game


open System
open Network
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
type EncodingDecoding() = 
    [<TestMethod>]
    member this.EncodingFullMove () = 
        let moves = [ Move.AddPiece ((Black, Normal), (5, 7)); Move.AddPiece ((Black, Normal), (2, 9)); Move.AddPiece ((Black, Normal), (0, 0)) ]
        let encodeDecode = moves |> encode |> decode
        Assert.AreEqual(moves, encodeDecode)
    
    [<TestMethod>]
    member this.EncodingAddNormalPiece () = 
        let move = Move.AddPiece ((Black, Normal), (5, 7))
        let (_, encodeDecode) = move |> moveToBytes |> decodeMove 0
        Assert.AreEqual(move, encodeDecode)

    [<TestMethod>]
    member this.EncodingAddBigPiece () = 
        let move = Move.AddPiece ((White, Big (1, 1)), (5, 7))
        let (_, encodeDecode) = move |> moveToBytes |> decodeMove 0
        Assert.AreEqual(move, encodeDecode)

    [<TestMethod>]
    member this.EncodingRemovePiece () = 
        let move = Move.RemovePiece (0, 15)
        let (_, encodeDecode) = move |> moveToBytes |> decodeMove 0
        Assert.AreEqual(move, encodeDecode)

    [<TestMethod>]
    member this.EncodingPass () = 
        let move = Move.Pass
        let (_, encodeDecode) = move |> moveToBytes |> decodeMove 0
        Assert.AreEqual(move, encodeDecode)

    [<TestMethod>]
    member this.EncodingMarkDead () = 
        let move = Move.MarkDead [ (0,0); (5, 7); (2, 3); (3, 2) ]
        let (_, encodeDecode) = move |> moveToBytes |> decodeMove 0
        Assert.AreEqual(move, encodeDecode)

    [<TestMethod>]
    member this.EncodingPiece () = 
        let piece = (Black, Normal)
        let (_, encodeDecode) = piece |> pieceToBytes |> decodePiece 0
        Assert.AreEqual(piece, encodeDecode)

    [<TestMethod>]
    member this.EncodingGameInfo () = 
        let gameSize = 5
        let genop = { NeutralGen = false }
        let powerop = PowerOption.Low
        let seed = 5
        let encodeDecode = gameInfoToBytes gameSize genop powerop seed |> decodeGameInfo
        Assert.AreEqual((gameSize, genop, powerop, seed), encodeDecode)