namespace GameParts

type CellValue =
    | Value of int
    | Empty

type CellState =
    | Marked
    | Given
    | Wrong
    | Unselected
    | Selected

type Cell =
    {   Value: CellValue
        CellState: CellState } 
    
module Cell =
    let Create s v= 
        let value = if v = 0 then Empty else Value v
        {   Value = value
            CellState = s }

module Rules =    
    let private isUnique arr =
        arr |> Array.distinct |> Array.length = (arr |> Array.length)

    let private noEmpty arr =
        arr
        |> Array.map (fun cell -> cell.Value) 
        |> Array.contains Empty 
        |> not

    let Correct arr =
        isUnique arr && noEmpty arr

type Position = Top | Middle | Bottom
type Row = Cell[]
type Column = Cell[]

type Board = Cell[,]
module Board  =
    let Create arr : Board =
        arr
        |> Array2D.map( fun c ->
            if c = 0 then Cell.Create Unselected 0
            elif c < 0 || c > 9 then Cell.Create Unselected 0
            else Cell.Create Given c )
        
    let GetColumn col (board:Board) : Column  =
        board.[*, col]

    let GetRow row (board:Board) : Row =
        board.[row, *]
       
    let ChangeCellState row column state (board:Board): Board =
        board
        |> Array2D.mapi(fun r c cell ->
            if r = row && c = column then {cell with CellState = state}
            else cell )

    let ChangeCellStateMove (row, newRow) (column, newColumn) (board:Board): Board =
        board
        |> Array2D.mapi(fun r c cell ->
            if r = row && c = column then { cell with CellState = Unselected }
            elif r = newRow && c = newColumn then { cell with CellState = Selected }
            else cell )

    let ChangeCellValue row column value (board:Board): Board =
        board
        |> Array2D.mapi(fun r c cell ->
            if r = row && c = column then {cell with Value = value}
            else cell )

    let ChangeBoth row column state value (board:Board): Board =
        board
        |> Array2D.mapi(fun r c cell ->
            if r = row && c = column then {cell with Value = value; CellState = state}
            else cell )

    let Validate f idx (board:Board) =
        f idx board
        |> Rules.Correct

type Grid = Cell[,]
module Grid =
    let Get (row,col) (board:Board) : Grid  =
        board.[row..row+2, col..col+2]

    let Correct (g:Grid) =
        [| g.[0,*]; g.[1,*]; g.[2,*] |]
        |> Array.concat
        |> Rules.Correct

    let AllGrids (b:Board) =
        [|
            0,0
            0,3
            0,6
            3,0
            3,3
            3,6
            6,0
            6,3
            6,6
        |]
        |> Array.map(fun coords -> Get coords b)

type State =
    | EnterData
    | CheckData
    | StartGame
    | DrawBoard
    | Running
    | GameOver
    | Quit

type Game = 
    {   Board: Board
        State: State
        ActiveCell: int * int }

module Game =
    let private rulesCheck f (g: Game)=
        [|0..8|]
        |> Array.map (fun i -> Board.Validate f i g.Board)
        |> Array.filter (fun v -> v = false) 
        |> fun arr ->
            match arr.Length with 
            | 0 -> Ok g
            | _ -> Error g

    let private rulesCheckGrid (g: Game)=
        Grid.AllGrids g.Board
        |> Array.map Grid.Correct
            |> Array.distinct
            |> fun arr ->
                match arr.Length with 
                | 1 -> Ok g
                | _ -> Error g

    let CheckSolution (g:Game) =
        rulesCheck Board.GetRow g
        |> Result.bind (rulesCheck Board.GetColumn)
        |> Result.bind (rulesCheckGrid)
        |> fun r ->
            match r with 
            | Ok _ -> true
            | Error _ -> false