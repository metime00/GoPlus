module Board
open Util

type Color =
    | Black
    | White
    | Neutral

type Cell =
    | Taken of Color
    | Free

/// A piece in the game of GoPlus, where the int in some pieces is extension out from the center
type Piece =
    | Normal of Color
    | Vertical of Color * int
    | Horizontal of Color * int
    | Big of Color * int
    | None

/// Returns an empty board of pieces
let board size = Array2D.create size size Piece.None

/// Takes a board of pieces and returns the board with cells instead of pieces
let genCells board =
    let output = Array2D.create (Array2D.length1 board) (Array2D.length2 board) Cell.Free
    /// Populates the output board's color using whatever piece is at the given coordinate
    let piecePop (inputBoard : Piece[,]) x y outputBoard =
        match inputBoard.[x,y] with
        | Normal col ->
            Array2D.set outputBoard y x (Cell.Taken col)
        | Vertical (col, ext) ->
            for j in y - ext .. y + ext do (Cell.Taken col) |> Array2D.set outputBoard j x
        | Horizontal (col, ext) ->
            for i in x - ext .. x + ext do (Cell.Taken col) |> Array2D.set outputBoard y i
        | Big (col, ext)->
            for i in x - ext .. x + ext do
                for j in y - ext .. y + ext do
                    (Cell.Taken col) |> Array2D.set outputBoard j i
        | None -> ()
    //check every coordinate for a piece, even though some pieces populate multiple tiles
    for i in 0 .. Array2D.length2 board - 1 do
        for j in 0 .. Array2D.length1 board - 1 do
            piecePop board i j output
    output

/// Adds pieces given to the board and returns the new board
let addPieces board (pieces : (Piece * (int * int)) list) =
    let output = Array2D.copy board
    for (piece, (x, y)) in pieces do
        Array2D.set output y x piece
    output

/// Takes a board of cells and returns a list of dead cells, with the second argument being the color of the piece that was placed last, that does not get their liberties checked
let checkDead board lastColor =
    /// Running total of dead cells
    let dead = new System.Collections.Generic.List<Color * (int * int)>()
    let visited = Array2D.create (Array2D.length1 board) (Array2D.length2 board) false
    
    /// Gets adjacent unvisited neighboring pieces of the same color
    let findNeighbors (x, y) = 
        [(x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1)] 
        |> List.filter (fun (x, y) -> boundCheck (x, y) (Array2D.length2 board) (Array2D.length1 board))
        |> List.filter (fun (x2, y2) -> not visited.[y2,x2] && board.[y2,x2] = board.[y,x])
    /// Gets adjacent empty spaces
    let findEmpty (x, y) =
        [(x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1)] 
        |> List.filter (fun (x, y) -> boundCheck (x, y) (Array2D.length2 board) (Array2D.length1 board))
        |> List.filter (fun (x, y) -> board.[y,x] = Cell.Free)

    /// Returns a list of all pieces in a group
    let rec genGroup group =
        for (x,y) in group do true |> Array2D.set visited y x
        let newNeighbors = [ for p in group do yield! findNeighbors p ]
        match newNeighbors with
        | [] -> group
        | _ -> group @ (genGroup newNeighbors)
    // Iterates through the board, finding out what pieces are dead and adding them to the list of dead pieces
    for i in 0 .. Array2D.length2 board - 1 do
        for j in 0 .. Array2D.length1 board - 1 do
            if not visited.[j,i] then
                match board.[j,i] with
                | Cell.Free -> ()
                | Cell.Taken groupColor when groupColor = lastColor -> ()
                | Cell.Taken groupColor ->
                    let group = genGroup [ (i,j) ] 
                    let liberties = [ for p in group do yield! findEmpty p ]
                    if liberties = [] then
                        for p in group do dead.Add (groupColor, p)
    [ for p in dead do yield p ]


/// Removes pieces given from the given board and returns the new board with given pieces removed
let removePieces board pieces =
    let output = Array2D.copy board
    for (x, y) in pieces do
        Array2D.set output y x None
    output