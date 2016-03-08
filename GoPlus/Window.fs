module Window
// TODO

// 1. finish the conway and shuffle powerups

// 2. make big pieces crush any piece in their way (remove any piece that intersects with them)

// 3. make graphics be images

open Pieces
open Board
open Gameplay
open Game
open Util
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

    let mutable errorMessage = ""

    let scoreDisplay = new Label ()
    let turnDisplay = new Label ()
    let powerupDisplay = new Label ()
    let endGameButton = new Button ()
    let undoButton = new Button ()

    let undo args =
        curMoves <- allBut curMoves
        errorMessage <- ""
        match curMoves with
        | [] ->
            intermediateBoard <- game.Board
            this.Invalidate ()
        | _ ->
            match game.CalculateState curMoves with
            | Accept (_, intermediateState) ->
                intermediateBoard <- intermediateState.board
                this.Invalidate ()
            | _ -> failwith "shouldn't be able to fail by removing a move"

    let endGame args =
        match game.Stage with
        | Play ->
            game.Pass () |> ignore
            errorMessage <- ""
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
                    errorMessage <- ""
                | Reject message ->
                    errorMessage <- message
            else
                match game.CalculateState moves with
                | Accept (_, intermediateState) ->
                    intermediateBoard <- intermediateState.board
                    curMoves <- moves
                    errorMessage <- ""
                | Reject message ->
                    errorMessage <- message
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
                match errorMessage with
                | "" ->
                    match game.GetPlayerPowerup game.NextToMove with
                    | None ->
                        "No Powerup"
                    | Some x ->
                        String.Format("{0}, {1} moves remaining", x.ToString (), game.GetMovesNeeded () - List.length curMoves)
                | x -> x
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