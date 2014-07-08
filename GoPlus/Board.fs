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
                    yield (x, y)
        ]
    | L ->
        [ (x, y); (x - 1, y); (x - 2, y); (x, y + 1) ]

/// Returns a list of all pieces in the group at the given coordinates
let genGroup group (board : Cell[,]) =
    let size = Array2D.length1 board
    let visited = noVisits size
    /// Gets adjacent unvisited neighboring pieces of the same color, and marks them all as visited
    let findNeighbors (x, y) = 
        let output =
            [(x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1)] 
            |> List.filter (fun (x, y) -> boundCheck (x, y) (size) (size))
            |> List.filter (fun (x2, y2) -> visited.[y2,x2] = false && board.[y2,x2] = board.[y,x])
        List.iter (fun (x, y) -> true |> Array2D.set visited y x) output
        output
    let rec loop group board =
        let newNeighbors = [ for p in group do yield! findNeighbors p ]
        match newNeighbors with
        | [] -> group
        | _ -> group @ (loop newNeighbors board)
    loop group board

/// Takes a board of pieces and returns the board with cells instead of pieces
let genCells board =
    let output = Array2D.create (Array2D.length1 board) (Array2D.length2 board) Cell.Free
    /// Populates the output board's color using whatever piece is at the given coordinate
    let piecePop (inputBoard : Option<Piece>[,]) x y outputBoard =
        match inputBoard.[y,x] with
        | Some (col, shape) ->
            match shape with
            | Normal ->
                Array2D.set outputBoard y x (Cell.Taken col)
            | Vertical ext ->
                for (_, j) in pieceCoords shape (x, y) do (Cell.Taken col) |> Array2D.set outputBoard j x
            | Horizontal ext ->
                for (i, _) in pieceCoords shape (x, y) do (Cell.Taken col) |> Array2D.set outputBoard y i
            | Big ext ->
                for (i, j) in pieceCoords shape (x, y) do (Cell.Taken col) |> Array2D.set outputBoard j i
            | L ->
                for (i, j) in pieceCoords shape (x, y) do (Cell.Taken col) |> Array2D.set outputBoard j i
        | None -> ()
    //check every coordinate for a piece, even though some pieces populate multiple tiles
    for i = 0 to Array2D.length2 board - 1 do
        for j = 0 to Array2D.length1 board - 1 do
            piecePop board i j output
    output

/// Adds pieces given to the board and returns the new board
let addPieces (board : Option<Piece>[,]) pieces =
    let output = Array2D.copy board
    List.iter (fun (piece, (x, y)) -> Array2D.set output y x (Some piece)) pieces
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
        |> List.filter (fun (x, y) -> board.[y,x] = Cell.Free)
    // Iterates through the board, finding out what pieces are dead and adding them to the list of dead pieces
    for i = 0 to size - 1 do
        for j = 0 to size - 1 do
            if not visited.[j,i] then
                match board.[j,i] with
                | Cell.Free -> ()
                | Cell.Taken groupColor when groupColor = lastColor -> ()
                | Cell.Taken groupColor ->
                    let group = genGroup [ (i,j) ] board
                    let liberties = [ for p in group do yield! findEmpty p ]
                    if liberties = [] then
                        for p in group do dead.Add p
    [ for p in dead do yield p ]


/// Removes pieces given from the given board and returns the new board with given pieces removed
let removePieces (board : Option<Piece>[,]) pieces =
    let output = Array2D.copy board
    List.iter (fun (x, y) -> Array2D.set output y x None) pieces
    output