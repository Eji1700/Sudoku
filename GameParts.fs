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
            match c with 
            | 0 -> Cell.Create Unselected 0
            | _ when c < 0 -> Cell.Create Unselected 0
            | _ -> Cell.Create Given c )
        
    let GetColumn col (board:Board) : Column  =
        board.[*, col]

    let GetRow row (board:Board) : Row =
        board.[row, *]
       
    let ChangeCellState row column state (board:Board): Board =
        board
        |> Array2D.mapi(fun r c cell ->
            if r = row && c = column then 
                {cell with CellState = state}
            else
                cell )

    let ChangeCellValue row column value (board:Board): Board =
        board
        |> Array2D.mapi(fun r c cell ->
            if r = row && c = column then 
                {cell with Value = value}
            else
                cell )

    let ChangeBoth row column state value (board:Board): Board =
        board
        |> Array2D.mapi(fun r c cell ->
            if r = row && c = column then 
                {cell with Value = value; CellState = state}
            else
                cell )

    let Validate f idx (board:Board) =
        f idx board
        |> Rules.Correct

type Grid = Row []
module Grid =
    let Get (row,col) (board:Board) : Grid =
        [|
            board.[row, col..col+2] 
            board.[row+1, col..col+2] 
            board.[row+2, col..col+2] 
        |]

    let Correct (g:Grid) =
        g
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
        |> (fun arr -> arr.Length = 0)

    let private trueBind f x =
        if x then f else false

    let CheckSolution (g:Game) =
        //Result pattern?
        rulesCheck Board.GetRow g
        |> trueBind (rulesCheck Board.GetColumn g)
        |> trueBind (
            Grid.AllGrids g.Board
            |> Array.map Grid.Correct
            |> Array.distinct
            |> Array.length = 1
            )