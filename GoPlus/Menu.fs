module Menu

open Window
open System
open System.Collections.Generic
open System.Drawing
open System.Windows.Forms

type Menu (width, height) as this =
    inherit Form ()

    let gameSizeLabel = new Label ()
    let gameSizeBox = new TextBox ()
    let startGameButton = new Button ()
    let goswips = new Label ()

    do
        this.Text <- "GoPlus"
        this.ClientSize <- new Size(width, height)
        this.SetStyle (ControlStyles.AllPaintingInWmPaint, true)
        this.SetStyle (ControlStyles.UserPaint, true)
        this.SetStyle (ControlStyles.OptimizedDoubleBuffer, true)
        this.FormBorderStyle <- FormBorderStyle.Fixed3D
        this.MaximizeBox <- false
        this.BackColor <- Color.Tan
        gameSizeLabel.Text <- "enter board size"
        gameSizeLabel.Dock <- DockStyle.Top
        gameSizeLabel.AutoSize <- true
        gameSizeBox.Dock <- DockStyle.Top
        gameSizeBox.AutoSize <- true
        goswips.Font <- new Drawing.Font ("Arial", 47.0f)
        goswips.TextAlign <- ContentAlignment.MiddleCenter
        goswips.Text <- "WELCOME TO GoPlus"
        goswips.Dock <- DockStyle.Fill
        startGameButton.Text <- "Start Game"
        startGameButton.Dock <- DockStyle.Bottom
        startGameButton.AutoSize <- true
        startGameButton.Click.Add (fun x ->
            this.Hide ()
            let gameSize = if gameSizeBox.Text = "" then 19 else Convert.ToInt32 gameSizeBox.Text
            let steve = new Window (gameSize, 640, 480)
            steve.Show ()
            this.Close ()
            ())
        this.Controls.AddRange [| gameSizeLabel; gameSizeBox; startGameButton; goswips |]