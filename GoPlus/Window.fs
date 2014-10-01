module Window

open Pieces
open Game
open GameOptions
open System
open System.Collections.Generic
open System.Drawing
open System.Windows.Forms

let penFromColor color =
    match color with
    |  Pieces.Color.Neutral -> new Pen(Color.Gray)
    |  Pieces.Color.Black -> new Pen(Color.Black)
    |  Pieces.Color.White -> new Pen(Color.Blue)

type Window () as this =
    inherit Form ()

    let mutable turn = Pieces.Color.Black

    let game = new Game (9, { NeutralGen = false; PowerupGen = false }, Vanilla)

    let squareSize = 400 / (Array2D.length1 game.Board)

    let scoreDisplay = new Label ()
    let endGameButton = new Button ()

    let endGame args =
        game.CalulateScore ()
        MessageBox.Show(String.Format("black score: {0}, white score: {1}", game.GetScore Pieces.Color.Black, game.GetScore Pieces.Color.White)) |> ignore
        ()

    do
        this.Text <- "GoPlus"
        this.ClientSize <- new Size(640, 480)
        this.SetStyle (ControlStyles.AllPaintingInWmPaint, true)
        this.SetStyle (ControlStyles.UserPaint, true)
        this.SetStyle (ControlStyles.OptimizedDoubleBuffer, true)
        this.FormBorderStyle <- FormBorderStyle.Fixed3D
        this.MaximizeBox <- false
        this.BackColor <- Color.White
        scoreDisplay.Text <- (game.GetScore turn).ToString ()
        scoreDisplay.Dock <- DockStyle.Right
        scoreDisplay.AutoSize <- true
        endGameButton.Text <- "calculate final scores"
        endGameButton.Dock <- DockStyle.Bottom
        endGameButton.AutoSize <- true
        endGameButton.Click.Add endGame
        this.Controls.Add scoreDisplay
        this.Controls.Add endGameButton

    // Window will handle timer and whose turn it is, it will translate ui actions into function calls on Game.
    // It decides when things happen, Game implements them
    override this.OnMouseMove args =
        printfn "%i, %i" args.X args.Y
    override this.OnMouseDown args =
        if args.X < 400 || args.Y < 400 then
            match turn with
            | Pieces.Color.Black ->
                let x = args.X / squareSize
                let y = args.Y / squareSize
                let result = game.AddPiece (Pieces.Color.Black, Shape.Normal) Pieces.Color.Black (x, y)
                if result = ActionResponse.Accept then turn <- Pieces.Color.White
            | Pieces.Color.White ->
                let x = args.X / squareSize
                let y = args.Y / squareSize
                let result = game.AddPiece (Pieces.Color.White, Shape.Normal) Pieces.Color.White (x, y)
                if result = ActionResponse.Accept then turn <- Pieces.Color.Black
            this.Invalidate ();
            

    override this.OnPaint args =
        scoreDisplay.Text <- String.Format("current player's score: {0}", game.GetScore turn)

        let size1 = Array2D.length1 game.Board
        let size2 = Array2D.length2 game.Board
        for i = 0 to size1 - 1 do
            for j = 0 to size2 - 1 do
                match game.Board.[i,j] with
                | Some (color, Normal) ->
                    args.Graphics.DrawEllipse(penFromColor(color), i * squareSize, j * squareSize, squareSize, squareSize)
                | None -> ()