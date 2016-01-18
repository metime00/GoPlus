module Menu

open Window
open System
open System.Collections.Generic
open System.Drawing
open System.Windows.Forms
open GameOptions

type Menu (width, height) as this =
    inherit Form ()

    let gameSizeLabel = new Label ()
    let gameSizeBox = new TextBox ()
    let powerUpCheck = new CheckBox ()
    let neutralCheck = new CheckBox ()
    let powerUpFreq = 
        [|
            let cases = Microsoft.FSharp.Reflection.FSharpType.GetUnionCases typeof<GameOptions.PowerOption>
            for i in cases do
                let radio = new RadioButton ()
                radio.Text <- i.Name
                radio.AutoSize <- true
                radio.Dock <- DockStyle.Top
                if i.Name = "Vanilla" then radio.Checked <- true
                yield radio
        |]
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

        powerUpCheck.Text <- "enable powerups"
        powerUpCheck.Dock <- DockStyle.Top
        powerUpCheck.AutoSize <- true
        neutralCheck.Text <- "enable neutral stones"
        neutralCheck.Dock <- DockStyle.Top
        neutralCheck.AutoSize <- true
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
            let powerOp =
                let radio = Array.find (fun (x : RadioButton) -> x.Checked) powerUpFreq
                match radio.Text with
                | "Vanilla" -> Vanilla
                | "Low" -> Low
                | "Medium" -> Medium
                | "High" -> High
                | _ -> failwith "radio button had text that isn't supported"
            let steve = new Window (gameSize, { NeutralGen = neutralCheck.Checked; PowerupGen = powerUpCheck.Checked }, powerOp, 640, 480)
            steve.Show ()
            this.Close ()
            ())
        this.Controls.AddRange [| gameSizeLabel; gameSizeBox; startGameButton; goswips; neutralCheck |]
        this.Controls.AddRange [| for i in powerUpFreq do yield i :> Control |]
        this.Controls.Add powerUpCheck