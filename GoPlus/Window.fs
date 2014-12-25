module Window

open Pieces
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

type Window (gameSize, width, height) as this =
    inherit Form ()

    let mutable turn = Pieces.Color.Black

    let game = new Game (gameSize, { NeutralGen = false; PowerupGen = false }, Vanilla)

    let squareSize = (scale width) / (gameSize)

    let scoreDisplay = new Label ()
    let turnDisplay = new Label ()
    let endGameButton = new Button ()

    let endGame args =
        game.CalulateScore ()
        MessageBox.Show(String.Format("black score: {0}, white score: {1}", game.GetScore Pieces.Color.Black, game.GetScore Pieces.Color.White)) |> ignore
        ()

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
        endGameButton.Text <- "calculate final scores"
        endGameButton.Dock <- DockStyle.Bottom
        endGameButton.AutoSize <- true
        endGameButton.Click.Add endGame
        this.Controls.AddRange [| scoreDisplay; turnDisplay; endGameButton |]

    // Window will handle timer and whose turn it is, it will translate ui actions into function calls on Game.
    // It decides when things happen, Game implements them
    override this.OnMouseMove args =
        printfn "%i, %i" args.X args.Y
    override this.OnMouseDown args =
        if args.X < scale this.ClientSize.Width && args.Y < scale this.ClientSize.Width then
            match turn with
            | Pieces.Color.Black ->
                let x = args.X / squareSize
                let y = args.Y / squareSize
                let result = game.AddPiece (Pieces.Color.Black, Shape.Normal) Pieces.Color.Black (x, y)
                match result with
                | ActionResponse.Accept ->
                    turnDisplay.Text <- "White"
                    turn <- Pieces.Color.White
                | ActionResponse.Reject message -> turnDisplay.Text <- message
            | Pieces.Color.White ->
                let x = args.X / squareSize
                let y = args.Y / squareSize
                let result = game.AddPiece (Pieces.Color.White, Shape.Normal) Pieces.Color.White (x, y)
                match result with
                | ActionResponse.Accept ->
                    turnDisplay.Text <- "Black"
                    turn <- Pieces.Color.Black
                | ActionResponse.Reject message -> turnDisplay.Text <- message
            this.Invalidate ();
            

    override this.OnPaint args =
        scoreDisplay.Text <- String.Format("current player's score: {0}", game.GetScore turn)

        let size1 = Array2D.length1 game.Board
        let size2 = Array2D.length2 game.Board
        for i = 0 to size1 - 1 do
            args.Graphics.DrawLine(Pens.Black, 0, i * squareSize + (squareSize / 2), scale this.ClientSize.Width, i * squareSize + (squareSize / 2))
        for j = 0 to size2 - 1 do
            args.Graphics.DrawLine(Pens.Black, j * squareSize + (squareSize / 2), 0, j * squareSize + (squareSize / 2), scale this.ClientSize.Width)
        for i = 0 to size1 - 1 do
            for j = 0 to size2 - 1 do
                match game.Board.[i,j] with
                | Some (color, Normal) ->
                    args.Graphics.FillEllipse(brushFromColor(color), i * squareSize, j * squareSize, squareSize, squareSize)
                | None -> ()