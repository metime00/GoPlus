module Game

// Consolidation of all game logic, so any user interface's interaction with game logic is requesting actions by game and having them either accepted or rejected
// Game will check for validity of actions and carry them out itself, so the ui will have no business logic in it and be completely replaceable and still have the same functioning game
open Util
open Pieces
open GameOptions
open BoardGen
open Board
open Player
open Gameplay

type Stage =
    | Play
    | Scoring

type Game (size, genop, powerop, seed) =
    let prevStates = new System.Collections.Generic.List<State>()
    let movesMade = new System.Collections.Generic.List<Move list>()
    let mutable state = 
        let seed = seed
        { //initial state
        seed = seed;
        black = { color = Color.Black; score = 0; powerup = None };
        white = { color = Color.White; score = 0; powerup = None };
        board = generate seed genop powerop size;
        powerups = powerop;
        nextToMove = Color.Black }
    /// The function to advance the board to the next state, should be the only way the board is changed
    let updateState newState =
        match state.nextToMove with
            | Black -> printfn "black's move"
            | White -> printfn "white's move"
        prevStates.Add state
        state <- newState

    /// Returns the game's board for displaying
    member this.Board = state.board

    member this.Pass () =
        let moves = [Move.Pass]
        state |> apply moves |> updateState
        movesMade.Add moves
        Accept ()

    /// calculates total score, assuming all groups are alive, returns the two players' scores as (black, white)
    member this.CalulateScore () =
        let visited = (noVisits size)
        let cells = genCells state.board
        /// Returns the nonempty pieces adjacent
        let findEnclosing (x, y) = 
            let output =
                [(x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1)] 
                |> List.filter (fun (x, y) -> boundCheck (x, y) (size) (size))
                |> List.filter (fun (x, y) -> cells.[x,y] <> Free)
            output
        let mutable blackScore = state.black.score
        let mutable whiteScore = state.white.score
        for i = 0 to size - 1 do
            for j = 0 to size - 1 do
                if not visited.[i,j] && cells.[i,j] = Free then 
                    let group = genGroup (i, j) cells
                    List.iter (fun (x, y) -> true |> Array2D.set visited x y) group
                    let enclosingPieces = [ for p in group do yield! findEnclosing p ]
                    let rec enclosingColor pieces comparePiece =
                        match pieces with
                        | [] -> Some comparePiece
                        | (x, y) :: _ ->
                            if comparePiece = cells.[x,y] || cells.[x,y] = Cell.Taken Neutral then 
                                if cells.[x,y] = Cell.Taken Neutral then
                                    printf "omg im neutral"
                                enclosingColor (List.tail pieces) comparePiece
                            else None
                    if not (List.isEmpty enclosingPieces) then
                        //find the first enclosing color that isn't neutral, since neutral can count for both, but can't get score itself
                        let colorSample = enclosingPieces |> List.filter (fun (x, y) -> cells.[x, y] <> Cell.Taken Neutral) |> List.head
                        match enclosingColor enclosingPieces cells.[fst colorSample, snd colorSample] with
                        | Some (Taken Black) ->
                            blackScore <- blackScore + (List.length group)
                        | Some (Taken White) ->
                            whiteScore <- whiteScore + (List.length group)
                        | Some (Taken Neutral) -> ()
                        | None -> ()
        (float blackScore, (float whiteScore) + 6.5)
    
    /// given a coordinate, returns all the coordinates of the pieces of the group occupying that coordinate, for post game scoring.
    member this.GetGroup coord =
        List.filter (fun (x, y) -> state.board.[x, y] <> None) (genGroup coord (genCells this.Board))

    member this.PrevStates = prevStates

    member this.PrevMoves = movesMade

    /// Returns whether or not the game is in scoring mode or playing mode, based on the past moves
    member this.Stage =
        let expr =
            if movesMade.Count < 2 then
                false
            else
                movesMade.Item (movesMade.Count - 1) = [ Move.Pass ] //last two moves were a pass
                && movesMade.Item (movesMade.Count - 2) = [ Move.Pass ]
        match expr with
        | false -> Stage.Play
        | true -> Stage.Scoring

    member this.NextToMove =
        state.nextToMove

    member this.GetScore color =
        match color with
        | Pieces.Color.Black -> state.black.score
        | Pieces.Color.White -> state.white.score

    member this.GetPlayerPowerup color =
        match color with
        | Pieces.Color.Black -> state.black.powerup
        | Pieces.Color.White -> state.white.powerup

    /// Returns the next state as an ActionRespones<State> given a list of clicked coordinates, but does not update the game state
    member this.CalculateState (coords : (int * int) list) =
        let moves = 
            let powerupToCheck =
                match state.nextToMove with
                | Black ->
                    state.black.powerup
                | White ->
                    state.white.powerup
            match powerupToCheck with
            | _ when this.Stage = Scoring -> [ Move.MarkDead coords ]
            | None -> [ (Move.AddPiece ((state.nextToMove, Pieces.Normal), List.head coords)) ]
            | Some x ->
                match x with
                | Powerup.Big x -> [ Move.AddPiece ((state.nextToMove, Shape.Big x), List.head coords) ]
                | Powerup.Remove _ -> [ for i in coords do yield (Move.RemovePiece i) ]
                | Powerup.Multiple (_, colorBool) -> 
                    let col =
                        match state.nextToMove with
                        | Black when colorBool -> Black
                        | Black when not colorBool -> White
                        | White when colorBool -> White
                        | White when not colorBool -> Black
                        | _ -> failwith "should only have black and white as the colors to move"
                    [ for i in coords do yield (Move.AddPiece ((col, Shape.Normal), i)) ]
                | Powerup.L -> [ for i in coords do yield (Move.AddPiece ((state.nextToMove, Shape.L), i)) ]
                | Powerup.Conway -> [ Move.Conway ]
                | Powerup.Shuffle x ->  [ Move.Shuffle x ]
        let koState = 
            if prevStates.Count < 3 then
                None
            else
                Some (prevStates.Item (prevStates.Count - 1))
        match valid moves state koState with
        | Accept () ->
            Accept (moves, state |> perform moves)
        | Reject message -> Reject message

    /// Given a list of clicked move coordinates, figures out what move is next to make, and uses those coordinates as the input
    /// Updates the state, or returns an error message if the move coordinates are incompatible with the next move to make
    member this.MakeMoves moveCoords =
        let timey = System.Diagnostics.Stopwatch.StartNew ()
        match this.CalculateState moveCoords with
        | Accept (moves, _) ->
            state |> apply moves |> updateState
            movesMade.Add moves
            timey.Stop ()
            printfn "took %i ms to complete move" timey.ElapsedMilliseconds
            Accept ()
        | Reject message ->
            Reject message

    /// Tells the UI how many moves are needed to be passed to it before it has enough to update the game one whole state.
    /// Calculates this based on whether or not the next to move player has a powerup or not, and which kind
    member this.GetMovesNeeded () =
        //if the last two moves were passes, then the next move to play is a markdead move.
        //If the player has a powerup, the next move to play is that powerup, etc.
        let powerupToCheck =
            match state.nextToMove with
            | Black ->
                state.black.powerup
            | White ->
                state.white.powerup
        match powerupToCheck with
        | None -> 1
        | Some x ->
            match x with
            | Powerup.Big _ -> 1
            | Powerup.Remove (num) -> num
            | Powerup.Multiple (num, _) -> num
            | Powerup.L -> 1
            | Powerup.Conway -> 1
            | Powerup.Shuffle _ -> 1