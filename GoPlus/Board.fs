module Board
open Util
open Pieces

/// Returns an empty board of pieces
let board size = Array2D.create size size (None : Option<Piece>)
/// Returns the coordinates occupied by the given piece
let pieceCoords shape (x, y) =
    match shape with
    | Normal ->
        [ (x, y) ]
    | Vertical ext ->
        [ for j = y - ext to y + ext do yield (x, j) ]
    | Horizontal ext ->
        [ for i = x - ext to x + ext do yield (i, y) ]
    | Big ext ->
        [
            for i = x - ext to x + ext do
                for j = y - ext to y + ext do
                    yield (i, j)
        ]
    | L ->
        [ (x, y); (x - 1, y); (x - 2, y); (x, y + 1) ]

/// Returns a list of all pieces in the group at the given coordinates
let genGroup (x, y) (board : Cell[,]) =
    let size = Array2D.length1 board
    
    let visited = noVisits size
    true |> Array2D.set visited x y // have to set initial location to visited, because it won't in the other functions

    /// Gets adjacent unvisited neighboring pieces of the same color, and marks them all as visited
    let findNeighbors (x, y) = 
        let output =
            [(x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1)] 
            |> List.filter (fun (x, y) -> boundCheck (x, y) (size) (size))
            |> List.filter (fun (x2, y2) -> visited.[x2,y2] = false && board.[x2,y2] = board.[x,y])
        List.iter (fun (x, y) -> true |> Array2D.set visited x y) output
        output
    let rec loop group board =
        let newNeighbors = [ for p in group do yield! findNeighbors p ]
        match newNeighbors with
        | [] -> group
        | _ -> (loop newNeighbors board) @ group
    loop [ (x, y) ] board

/// Takes a board of pieces and returns the board with cells instead of pieces
let genCells board =
    let output = Array2D.create (Array2D.length1 board) (Array2D.length2 board) Cell.Free
    /// Populates the output board's color using whatever piece is at the given coordinate
    let piecePop (inputBoard : Option<Piece>[,]) x y outputBoard =
        match inputBoard.[x,y] with
        | Some (col, shape) ->
            match shape with
            | Normal ->
                Array2D.set outputBoard x y (Cell.Taken col)
            | Vertical ext ->
                for (_, j) in pieceCoords shape (x, y) do (Cell.Taken col) |> Array2D.set outputBoard x j
            | Horizontal ext ->
                for (i, _) in pieceCoords shape (x, y) do (Cell.Taken col) |> Array2D.set outputBoard i y
            | Big ext ->
                for (i, j) in pieceCoords shape (x, y) do (Cell.Taken col) |> Array2D.set outputBoard i j
            | L ->
                for (i, j) in pieceCoords shape (x, y) do (Cell.Taken col) |> Array2D.set outputBoard i j
        | None -> ()
    //check every coordinate for a piece, even though some pieces populate multiple tiles
    for i = 0 to Array2D.length1 board - 1 do
        for j = 0 to Array2D.length2 board - 1 do
            piecePop board i j output
    output

/// Adds pieces given to the board and returns the new board
let addPieces (board : Option<Piece>[,]) pieces =
    let output = Array2D.copy board
    List.iter (fun (piece, (x, y)) -> Array2D.set output x y (Some piece)) pieces
    output

/// Takes a board of cells and returns a list of dead cells, with the second argument being the color of the piece that was placed last, that does not get their liberties checked
let checkDead lastColor board =
    /// Running total of dead cells
    let dead = new System.Collections.Generic.List<(int * int)>()
    let size = Array2D.length1 board
    let visited = (noVisits size)
    /// Gets adjacent empty spaces
    let findEmpty (x, y) =
        [(x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1)] 
        |> List.filter (fun (x, y) -> boundCheck (x, y) (size) (size))
        |> List.filter (fun (x, y) -> board.[x,y] = Cell.Free)
    // Iterates through the board, finding out what pieces are dead and adding them to the list of dead pieces
    for i = 0 to size - 1 do
        for j = 0 to size - 1 do
            if not visited.[i,j] then
                match board.[i,j] with
                | Cell.Free -> ()
                | Cell.Taken groupColor when groupColor = lastColor -> ()
                | Cell.Taken groupColor ->
                    let group = genGroup (i,j) board
                    let liberties = [ for p in group do yield! findEmpty p ]
                    if liberties = [] then
                        for (x, y) in group do
                            dead.Add (x, y)
                            true |> Array2D.set visited x y
    [ for p in dead do yield p ]


/// Removes pieces given from the given board and returns the new board with given pieces removed
let removePieces (board : Option<Piece>[,]) pieces =
    let output = Array2D.copy board
    List.iter (fun (x, y) -> Array2D.set output x y None) pieces
    output