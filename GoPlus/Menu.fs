module Menu

open Window
open Network
open System
open System.Net
open System.Net.Sockets
open System.Collections.Generic
open System.Drawing
open System.Windows.Forms
open GameOptions

let port = 33764

type Menu (width, height) as this =
    inherit Form ()

    let mutable gameStarted = false

    let gameSizeLabel = new Label ()
    let gameSizeBox = new TextBox ()
    let networkLabel = new Label ()
    let networkBox = new TextBox ()
    let powerUpLabel = new Label ()
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
    let hostGameButton = new Button ()
    let goswips = new Label ()

    let startGame host x =
        this.Hide ()
        let client = 
            if host then
                let listener = new TcpListener(IPAddress.Any, port)
                listener.Start ()
                printfn "I am hosting and waiting"
                let client = listener.AcceptTcpClient ()
                printfn "%s" (client.ToString ())
                Some (client)
            elif networkBox.Text <> "" then
                let ipAddress = IPAddress.Parse (networkBox.Text)
                let endpoint = new IPEndPoint (ipAddress, port)
                let requester = new TcpClient ()
                printfn "I am connecting"
                requester.Connect (endpoint)
                printfn "%s" (requester.ToString ())
                Some (requester)
            else
                None
        
        let steve =     
            match client with
            | None ->
                let gameSize = if gameSizeBox.Text = "" then 19 else Convert.ToInt32 gameSizeBox.Text
                let powerOp =
                    let radio = Array.find (fun (x : RadioButton) -> x.Checked) powerUpFreq
                    match radio.Text with
                    | "Vanilla" -> Vanilla
                    | "Low" -> Low
                    | "Medium" -> Medium
                    | "High" -> High
                    | "Guaranteed" -> Guaranteed
                    | _ -> failwith "radio button had text that isn't supported"
                new Window (gameSize, { NeutralGen = neutralCheck.Checked }, powerOp, 640, 480, client, (new Random ()).Next ())
            | Some (client) when host ->
                let gameSize = if gameSizeBox.Text = "" then 19 else Convert.ToInt32 gameSizeBox.Text
                let genOp = { NeutralGen = neutralCheck.Checked }
                let powerOp =
                    let radio = Array.find (fun (x : RadioButton) -> x.Checked) powerUpFreq
                    match radio.Text with
                    | "Vanilla" -> Vanilla
                    | "Low" -> Low
                    | "Medium" -> Medium
                    | "High" -> High
                    | "Guaranteed" -> Guaranteed
                    | _ -> failwith "radio button had text that isn't supported"
                let seed = (new Random ()).Next ()

                // send the game generation info to the other player
                let message = gameInfoToBytes gameSize genOp powerOp seed

                client.GetStream().Write (message, 0, message.Length)
                printfn "i wrote game info to the stream hooray"
                new Window (gameSize, genOp, powerOp, 640, 480, client, seed)
            | Some (client) when not host ->
                // read the gameInfo from the host
                let gameInfo = Array.zeroCreate 10
                if client.GetStream().Read (gameInfo, 0, gameInfo.Length) <> gameInfo.Length then
                    failwith "could not read game info from the host"

                let (gameSize, genOp, powerOp, seed) = decodeGameInfo gameInfo
                printfn "I got this from the stream: %A" (decodeGameInfo gameInfo)
                new Window (gameSize, genOp, powerOp, 640, 480, client, seed)
        steve.Show ()
        gameStarted <- true
        this.Close ()
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

        powerUpLabel.Text <- "powerup frequency"
        powerUpLabel.Dock <- DockStyle.Top
        powerUpLabel.AutoSize <- true
        neutralCheck.Text <- "enable neutral stones"
        neutralCheck.Dock <- DockStyle.Top
        neutralCheck.AutoSize <- true
        gameSizeLabel.Text <- "enter board size"
        gameSizeLabel.Dock <- DockStyle.Top
        gameSizeLabel.AutoSize <- true
        gameSizeBox.Dock <- DockStyle.Top
        gameSizeBox.AutoSize <- true
        networkLabel.Text <- "enter IP address"
        networkLabel.Dock <- DockStyle.Top
        networkLabel.AutoSize <- true
        networkBox.Dock <- DockStyle.Top
        networkBox.AutoSize <- true
        goswips.Font <- new Drawing.Font ("Arial", 47.0f)
        goswips.TextAlign <- ContentAlignment.MiddleCenter
        goswips.Text <- "WELCOME TO GoPlus"
        goswips.Dock <- DockStyle.Fill
        startGameButton.Text <- "Join/Start Game"
        startGameButton.Dock <- DockStyle.Bottom
        startGameButton.AutoSize <- true
        startGameButton.Click.Add (startGame false)
        hostGameButton.Text <- "Host Game"
        hostGameButton.Dock <- DockStyle.Bottom
        hostGameButton.AutoSize <- true
        hostGameButton.Click.Add (startGame true)
        this.Controls.AddRange [| networkLabel; networkBox; gameSizeLabel; gameSizeBox; startGameButton; hostGameButton; goswips; neutralCheck |]
        this.Controls.AddRange [| for i in powerUpFreq do yield i :> Control |]
        this.Controls.Add powerUpLabel

    override this.OnClosed args =
        if gameStarted then
            ()
        else
            Application.Exit ()