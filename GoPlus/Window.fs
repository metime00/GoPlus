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

    let game = new Game (3, { NeutralGen = false; PowerupGen = false }, Vanilla)

    let squareSize = 400 / (Array2D.length1 game.Board)

    do
        this.Text <- "GoPlus"
        this.ClientSize <- new Size(640, 480)
        this.SetStyle (ControlStyles.AllPaintingInWmPaint, true)
        this.SetStyle (ControlStyles.UserPaint, true)
        this.SetStyle (ControlStyles.OptimizedDoubleBuffer, true)
        this.FormBorderStyle <- FormBorderStyle.Fixed3D
        this.MaximizeBox <- false
        this.BackColor <- Color.White
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
                game.AddPiece (Pieces.Color.Black, Shape.Normal) Pieces.Color.Black (x, y) |> ignore
                turn <- Pieces.Color.White
            | Pieces.Color.White ->
                let x = args.X / squareSize
                let y = args.Y / squareSize
                game.AddPiece (Pieces.Color.White, Shape.Normal) Pieces.Color.White (x, y) |> ignore
                turn <- Pieces.Color.Black
            this.Invalidate ();
            

    override this.OnPaint args =
        let size1 = Array2D.length1 game.Board
        let size2 = Array2D.length2 game.Board
        for i = 0 to size1 - 1 do
            for j = 0 to size2 - 1 do
                match game.Board.[i,j] with
                | Some (color, Normal) ->
                    args.Graphics.DrawEllipse(penFromColor(color), i * squareSize, j * squareSize, squareSize, squareSize)
                | None -> ()