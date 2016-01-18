module Window
// TODO
// 1. make graphics be images
// 2. make ghost pieces when mouse is hovering over board to show player what the piece will be like

open Pieces
open Gameplay
open Game
open GameOptions
open System
open System.Collections.Generic
open System.Drawing
open System.Windows.Forms
 
///scales a given coordinate by a certain amount and returns it as an int
let scale coord = (int) ((float coord) * 2.0 / 3.0)

let brushFromColor color =
    match color with
    |  Pieces.Color.Neutral -> Brushes.Gray
    |  Pieces.Color.Black -> Brushes.Black
    |  Pieces.Color.White -> Brushes.White

type Stage =
    | Play
    | Scoring

type Window (gameSize, width, height) as this =
    inherit Form ()

    let mutable turn = Pieces.Color.Black
    let mutable stage = Stage.Play

    let deadGroups = new System.Collections.Generic.List<int * int> ()

    let game = new Game (gameSize, { NeutralGen = false; PowerupGen = false }, Vanilla)

    let squareSize = (scale width) / (gameSize)

    let scoreDisplay = new Label ()
    let turnDisplay = new Label ()
    let endGameButton = new Button ()
    let undoButton = new Button ()

    /// Changes whose turn it is and invalidates the screen
    let changeTurn () =
        match turn with
        | Black ->
            turnDisplay.Text <- "White"
            turn <- Pieces.Color.White
        | White ->
            turnDisplay.Text <- "Black"
            turn <- Pieces.Color.Black
        this.Invalidate ()

    let undo args =
        if deadGroups.Count > 0 then
            for i in game.GetGroup (deadGroups.Item (deadGroups.Count - 1)) do
                if deadGroups.Remove i = false then failwith "tried to remove a piece that doesn't exist"
            this.Invalidate ()

    let endGame args =
        match stage with
        | Play ->
            changeTurn ()
            if game.PrevMoves.Count <> 0 && game.PrevMoves.Item (game.PrevMoves.Count - 1) = Move.Pass then
                stage <- Stage.Scoring
                turnDisplay.Text <- "Scoring Mode"
                endGameButton.Text <- "End Game"
                this.Controls.Add undoButton
            game.Pass () |> ignore
        | Scoring ->
            game.MarkDead (List.ofSeq deadGroups) |> ignore
            let (blackScore, whiteScore) = game.CalulateScore ()
            MessageBox.Show(String.Format("black score: {0}, white score: {1}", blackScore, whiteScore)) |> ignore
            this.Close ()

    let placePiece (args : MouseEventArgs) =
        let x = args.X / squareSize
        let y = args.Y / squareSize
        match turn with
            | Pieces.Color.Black ->
                let result = 
                    match args.Button with
                    | MouseButtons.Left ->
                        game.AddPiece (Pieces.Color.Black, Shape.Normal) (x, y)
                    | MouseButtons.Right ->
                        game.AddPiece (Pieces.Color.Black, Shape.Big (1, 1)) (x, y)
                    | MouseButtons.Middle ->
                        game.AddPiece (Pieces.Color.Black, Shape.Big (0, 2)) (x, y)
                match result with
                | ActionResponse.Accept ->
                    changeTurn ()
                | ActionResponse.Reject message -> turnDisplay.Text <- message
            | Pieces.Color.White ->
                let result = 
                    match args.Button with
                    | MouseButtons.Left ->
                        game.AddPiece (Pieces.Color.White, Shape.Normal) (x, y)
                    | MouseButtons.Right ->
                        game.AddPiece (Pieces.Color.White, Shape.Big (1, 1)) (x, y)
                    | MouseButtons.Middle ->
                        game.AddPiece (Pieces.Color.White, Shape.Big (0, 2)) (x, y)
                match result with
                | ActionResponse.Accept ->
                    changeTurn ()
                | ActionResponse.Reject message -> turnDisplay.Text <- message
    
    let markGroup (args : MouseEventArgs) =
        let x = args.X / squareSize
        let y = args.Y / squareSize
        deadGroups.AddRange (game.GetGroup (x, y))
        this.Invalidate ()

    do
        this.Text <- "GoPlus"
        this.ClientSize <- new Size(width, height)
        this.SetStyle (ControlStyles.AllPaintingInWmPaint, true)
        this.SetStyle (ControlStyles.UserPaint, true)
        this.SetStyle (ControlStyles.OptimizedDoubleBuffer, true)
        this.FormBorderStyle <- FormBorderStyle.Fixed3D
        this.MaximizeBox <- false
        this.BackColor <- Color.Tan
        scoreDisplay.Text <- (game.GetScore turn).ToString ()
        scoreDisplay.Dock <- DockStyle.Right
        scoreDisplay.AutoSize <- true
        turnDisplay.Text <- "Black" //black begins game
        turnDisplay.Size <- new Size(120, 50) //bad hardcoding make it better eventually
        turnDisplay.Location <- new Point(this.ClientSize.Width - turnDisplay.Size.Width, scoreDisplay.Size.Height)
        undoButton.Text <- "Undo"
        undoButton.Dock <- DockStyle.Bottom
        undoButton.AutoSize <- true
        undoButton.Click.Add undo
        endGameButton.Text <- "Pass"
        endGameButton.Dock <- DockStyle.Bottom
        endGameButton.AutoSize <- true
        endGameButton.Click.Add endGame
        this.Controls.AddRange [| scoreDisplay; turnDisplay; endGameButton |]

    // Window will handle timer and whose turn it is, it will translate ui actions into function calls on Game.
    // It decides when things happen, Game implements them
    override this.OnMouseMove args =
        ()
    
    override this.OnMouseDown args =
        if args.X < scale this.ClientSize.Width && args.Y < scale this.ClientSize.Width then
            match stage with
            | Play ->
                placePiece args
            | Scoring ->
                markGroup args

    override this.OnPaint args =
        scoreDisplay.Text <-
            match stage with
            | Play -> String.Format("current player's score: {0}", game.GetScore turn)
            | Scoring -> ""

        let size1 = Array2D.length1 game.Board
        let size2 = Array2D.length2 game.Board
        for i = 0 to size1 - 1 do
            args.Graphics.DrawLine(Pens.Black, 0, i * squareSize + (squareSize / 2), scale this.ClientSize.Width, i * squareSize + (squareSize / 2))
        for j = 0 to size2 - 1 do
            args.Graphics.DrawLine(Pens.Black, j * squareSize + (squareSize / 2), 0, j * squareSize + (squareSize / 2), scale this.ClientSize.Width)
        for i = 0 to size1 - 1 do
            for j = 0 to size2 - 1 do
                if not (deadGroups.Contains (i, j)) then
                    match game.Board.[i,j] with
                    | Some (color, Normal) ->
                        args.Graphics.FillEllipse(brushFromColor(color), i * squareSize, j * squareSize, squareSize, squareSize)
                    | Some (color, Big (xext, yext)) ->
                        args.Graphics.FillEllipse(brushFromColor(color), (i - xext) * squareSize, (j - yext) * squareSize, squareSize * (1 + 2 * xext), squareSize * (1 + 2 * yext))
                    | Some (color, L) ->
                        args.Graphics.FillEllipse(brushFromColor(color), (i) * squareSize, (j) * squareSize, squareSize, squareSize * 2)
                        args.Graphics.FillEllipse(brushFromColor(color), (i - 2) * squareSize, (j) * squareSize, squareSize * 2, squareSize)
                    | None -> ()