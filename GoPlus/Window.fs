module Window
// TODO
// 0. make powerups actually be used and make them have to be used the turn following a player getting them
// 1. move all game state and current player and move choosing to Game and make Window interact with game only through game.interact and game.pass methods
// game will return an ActionResponse telling Window whether or not the other player is sending bad info or if the current user needs to try their move again
// game will have a method or property to tell Window how many (x, y) coords it needs, then window will collect the user input coords to deliver into the game
// 2. there will be a problem with user feedback for multiple move powerups. The user wants to be able to see the effects after every piece placed if they get to place 3 pieces,
// but the way moves are applied would only do them all at once. Make it so the Window can display intermediate states.

// keep the game stage state in window, it's too messy for the Game class. Maybe make state just be a check if the last two moves were passes

// Way to do this: game object has a function that returns an intermediate state given an incomplete list of move coordinates,
// the window stores clicks as a list of coordinates, updating the visuals of the board after each click with the returned intermediate board state
// once the window has enough coordinates as the game wants next, it will pass in those coords to a game.MakeMove method,
// where the game will apply the coordinates according to what sort of move the next move is based on the current state
// 3. make graphics be images

open Pieces
open Board
open Gameplay
open Game
open GameOptions
open Network
open System
open System.Collections.Generic
open System.Drawing
open System.Windows.Forms

let brushFromColor color =
    match color with
    |  Pieces.Color.Neutral -> Brushes.Gray
    |  Pieces.Color.Black -> Brushes.Black
    |  Pieces.Color.White -> Brushes.White
    |  Pieces.Color.Pickup (_) -> Brushes.Cyan

let transparentBlack = new SolidBrush (Color.FromArgb (128, Color.Black))
let transparentWhite = new SolidBrush (Color.FromArgb (128, Color.White))

let transparentBrushFromColor color =
    match color with
    |  Pieces.Color.Neutral -> Brushes.Gray
    |  Pieces.Color.Black -> transparentBlack :> Brush
    |  Pieces.Color.White -> transparentWhite :> Brush

type Window (gameSize, gen, powerop, width, height, client) as this =
    inherit Form ()

    ///scales a given coordinate by a certain amount and returns it as an int
    let scale coord = (int) ((float coord) * 2.0 / 3.0)

    let mutable (mouseX, mouseY) = (0, 0)

    /// the collection of coordinates of tentative moves
    let mutable curMoves : (int * int) list = [ ]

    let game = new Game (gameSize, gen, powerop, (new Random ()).Next ())

    /// Board for displaying, should be updated every click
    let mutable intermediateBoard = game.Board

    let mutable squareSize = (scale width) / (gameSize)

    let scoreDisplay = new Label ()
    let turnDisplay = new Label ()
    let powerupDisplay = new Label ()
    let endGameButton = new Button ()
    let undoButton = new Button ()

    let undo args =
        ()

    let endGame args =
        match game.Stage with
        | Play ->
            game.Pass () |> ignore
            if game.Stage = Stage.Scoring then
                turnDisplay.Text <- "Scoring Mode"
                endGameButton.Text <- "End Game"
            this.Invalidate ()
        | Scoring ->
            game.MakeMoves curMoves |> ignore
            let (blackScore, whiteScore) = game.CalulateScore ()
            MessageBox.Show(String.Format("black score: {0}, white score: {1}", blackScore, whiteScore)) |> ignore
            this.Close ()

    do
        this.Text <- "GoPlus"
        this.ClientSize <- new Size(width, height)
        this.SetStyle (ControlStyles.AllPaintingInWmPaint, true)
        this.SetStyle (ControlStyles.UserPaint, true)
        this.SetStyle (ControlStyles.OptimizedDoubleBuffer, true)
        this.FormBorderStyle <- FormBorderStyle.Sizable
        this.MaximizeBox <- true
        this.BackColor <- Color.Tan
        scoreDisplay.Text <- (game.GetScore game.NextToMove).ToString ()
        scoreDisplay.Dock <- DockStyle.Right
        scoreDisplay.AutoSize <- true
        turnDisplay.Text <- "Black" //black begins game
        turnDisplay.Size <- new Size(120, 50) //bad hardcoding make it better eventually
        turnDisplay.Location <- new Point(this.ClientSize.Width - turnDisplay.Size.Width, scoreDisplay.Size.Height)
        powerupDisplay.Text <- "No Powerup" //black begins game
        powerupDisplay.Size <- new Size(120, 50) //bad hardcoding make it better eventually
        powerupDisplay.Location <- new Point(this.ClientSize.Width - turnDisplay.Size.Width, scoreDisplay.Size.Height + turnDisplay.Height)
        undoButton.Text <- "Undo"
        undoButton.Dock <- DockStyle.Bottom
        undoButton.AutoSize <- true
        undoButton.Click.Add undo
        endGameButton.Text <- "Pass"
        endGameButton.Dock <- DockStyle.Bottom
        endGameButton.AutoSize <- true
        endGameButton.Click.Add endGame
        this.Controls.AddRange [| scoreDisplay; turnDisplay; powerupDisplay; endGameButton; undoButton |]

    // Window will handle timer and whose turn it is, it will translate ui actions into function calls on Game.
    // It decides when things happen, Game implements them
    override this.OnMouseMove args =
        mouseX <- args.X
        mouseY <- args.Y
        this.Invalidate ()

    override this.OnResize args =
        base.OnResize args
        squareSize <- (scale this.ClientSize.Width ) / gameSize
        turnDisplay.Location <- new Point(this.ClientSize.Width - turnDisplay.Size.Width, scoreDisplay.Size.Height)
        this.Invalidate ()
    
    override this.OnMouseDown args =
        if args.X < scale this.ClientSize.Width && args.Y < scale this.ClientSize.Width then
            let x = args.X / squareSize
            let y = args.Y / squareSize
            let moves = curMoves @ [(x, y)]
            if game.GetMovesNeeded () = List.length moves && game.Stage = Play then
                match game.MakeMoves moves with
                | Accept () ->
                    intermediateBoard <- game.Board
                    curMoves <- []
                | Reject message ->
                    turnDisplay.Text <- message
            else
                match game.CalculateState moves with
                | Accept (_, intermediateState) ->
                    intermediateBoard <- intermediateState.board
                    curMoves <- moves
                | Reject message ->
                    turnDisplay.Text <- message
            this.Invalidate ()

    member this.SignalReceived = signalReceived.Publish

    member this.OnSignalReceived args =
        
        ()
    
    override this.OnPaint args =
        scoreDisplay.Text <-
            match game.Stage with
            | Play -> String.Format("current player's score: {0}", game.GetScore game.NextToMove)
            | Scoring -> ""

        turnDisplay.Text <-
            match game.Stage with
            | Play ->
                match game.NextToMove with
                | Black ->
                    "Black"
                | White ->
                    "White"
            | Scoring ->
                ""

        powerupDisplay.Text <-
            match game.Stage with
            | Play ->
                match game.GetPlayerPowerup game.NextToMove with
                | None ->
                    "No Powerup"
                | Some x ->
                    String.Format("{0}, {1} moves remaining", x.ToString (), game.GetMovesNeeded () - List.length curMoves)
            | Scoring ->
                ""

        let size1 = Array2D.length1 game.Board
        let size2 = Array2D.length2 game.Board
        for i = 0 to size1 - 1 do
            args.Graphics.DrawLine(Pens.Black, 0, i * squareSize + (squareSize / 2), scale this.ClientSize.Width, i * squareSize + (squareSize / 2))
        for j = 0 to size2 - 1 do
            args.Graphics.DrawLine(Pens.Black, j * squareSize + (squareSize / 2), 0, j * squareSize + (squareSize / 2), scale this.ClientSize.Width)

        //draw a ghost piece over where the player would place a piece
        if mouseX < scale this.ClientSize.Width && mouseY < scale this.ClientSize.Width then
            match game.Stage with
            | Play ->
                let x = mouseX / squareSize
                let y = mouseY / squareSize
                args.Graphics.FillEllipse(transparentBrushFromColor(game.NextToMove), x * squareSize, y * squareSize, squareSize, squareSize) 
            | Scoring -> ()

        for i = 0 to size1 - 1 do
            for j = 0 to size2 - 1 do
                match intermediateBoard.[i,j] with
                | Some (color, Normal) ->
                    args.Graphics.FillEllipse(brushFromColor(color), i * squareSize, j * squareSize, squareSize, squareSize)
                | Some (color, Big (xext, yext)) ->
                    args.Graphics.FillEllipse(brushFromColor(color), (i - xext) * squareSize, (j - yext) * squareSize, squareSize * (1 + 2 * xext), squareSize * (1 + 2 * yext))
                | Some (color, L) ->
                    args.Graphics.FillEllipse(brushFromColor(color), (i) * squareSize, (j) * squareSize, squareSize, squareSize * 2)
                    args.Graphics.FillEllipse(brushFromColor(color), (i - 2) * squareSize, (j) * squareSize, squareSize * 2, squareSize)
                | None -> ()