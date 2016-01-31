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

type Game (size, genop, powerop) =
    let prevStates = new System.Collections.Generic.List<State>()
    let movesMade = new System.Collections.Generic.List<Move list>()
    let mutable state = 
        let seed = new System.Random ()
        { //initial state
        seed = seed;
        black = { color = Color.Black; score = 0; powerup = None };
        white = { color = Color.White; score = 0; powerup = None };
        board = generate seed genop powerop size;
        powerups = (powerop, genop.PowerupGen);
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

    /// Adds a piece to this location if it's valid, then checks for dead pieces using the given color (for a situation where white is adding a black piece), returning an ActionResponse to signal success or failure
    member this.AddPiece piece (x, y) =
        let moves = [(Move.AddPiece (piece, (x, y)))]
        let koState = 
            if prevStates.Count < 3 then
                None
            else
                Some (prevStates.Item (prevStates.Count - 1))
        match valid moves state koState with
        | Accept -> 
            state |> apply moves |> updateState
            movesMade.Add moves
            Accept
        | Reject message -> Reject message
    /// Adds a piece to this location if it's valid, then checks for dead pieces using the given color (for a situation where white is adding a black piece), returning an ActionResponse to signal success or failure
    member this.AddPieces pieces =
        let moves = List.map (fun piece -> Move.AddPiece piece) pieces
        let koState = 
            if prevStates.Count < 3 then
                None
            else
                Some (prevStates.Item (prevStates.Count - 1))
        match valid moves state koState with
        | Accept -> 
            state |> apply moves |> updateState
            movesMade.Add moves
            Accept
        | Reject message -> Reject message

    /// Removes a piece if it exists at the given location
    member this.RemovePiece (x, y) =
        let moves = [(Move.RemovePiece (x, y))]
        let koState = 
            if prevStates.Count < 3 then
                None
            else
                Some (prevStates.Item (prevStates.Count - 1))
        match valid moves state koState with
        | Accept -> 
            state |> apply moves |> updateState
            movesMade.Add moves
            Accept
        | Reject message -> Reject message

    member this.Pass () =
        let moves = [Move.Pass]
        state |> apply moves |> updateState
        movesMade.Add moves
        Accept

    member this.MarkDead pieces =
        let moves = [(Move.MarkDead pieces)]
        state |> apply moves |> updateState
        movesMade.Add moves
        Accept

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
                    //find the first enclosing color that isn't neutral, since neutral can count for both, but can't get score itself
                    let colorSample = enclosingPieces |> List.filter (fun (x, y) -> cells.[x, y] <> Cell.Taken Neutral) |> List.head
                    match enclosingColor enclosingPieces cells.[fst colorSample, snd colorSample] with
                    | Some (Taken Black) ->
                        blackScore <- blackScore + (List.length group)
                    | Some (Taken White) ->
                        whiteScore <- whiteScore + (List.length group)
                    | Some (Taken Neutral) -> ()
                    | None -> ()
        (blackScore, whiteScore)
    
    /// given a coordinate, returns all the coordinates of the pieces of the group occupying that coordinate, for post game scoring.
    member this.GetGroup coord =
        List.filter (fun (x, y) -> state.board.[x, y] <> None) (genGroup coord (genCells this.Board))

    member this.PrevStates = prevStates

    member this.PrevMoves = movesMade

    member this.GetScore color =
        match color with
        | Pieces.Color.Black -> state.black.score
        | Pieces.Color.White -> state.white.score