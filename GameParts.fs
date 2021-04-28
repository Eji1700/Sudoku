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
        CellState: CellState
    } 
    
module Cell =
    let Create s v= 
        let value =
            if v = 0  then Empty else Value v
        {   Value = value
            CellState = s 
        }

module Rules =    
    let private isUnique lst =
        lst |> List.distinct |> List.length = (lst |> List.length)

    let private noEmpty lst =
        lst
        |> List.map (fun cell -> cell.Value) 
        |> List.contains Empty 
        |> not

    let Correct lst =
        isUnique lst && noEmpty lst

type Position = Top | Middle | Bottom
type Row = Cell list
type Column = Cell list

type Board = Row list
module Board  =
    let Create lst : Board =
        lst
        |> List.map( fun r ->
            r
            |> List.map (fun c->
                if c = 0 then 
                    Cell.Create Unselected c
                else
                    Cell.Create Given c
            )
        )
        
    let GetColumn col (board:Board) : Column  =
        [0..8]
        |> List.map(fun i -> board.[i].[col])

    let GetRow row (board:Board) : Row =
        board.[row]
       
    let ChangeCellState row column state (board:Board): Board =
        let currRow = board.[row]
        let setAt i x lst =
            if List.length lst > i && i >= 0 then
                lst.[0..i-1] @ x::lst.[i+1..]
            else lst
        let newCell = {board.[column].[row] with CellState = state}
        let newRow = setAt column newCell currRow
        let newBoard = setAt row newRow board
        newBoard

    let ChangeCellValue row column value (board:Board): Board =
        let currRow = board.[row]
        let setAt i x lst =
            if List.length lst > i && i >= 0 then
                lst.[0..i-1] @ x::lst.[i+1..]
            else lst
        let newCell = {board.[column].[row] with Value = value}
        let newRow = setAt column newCell currRow
        let newBoard = setAt row newRow board
        newBoard

    let ChangeBoth row column state value (board:Board): Board =
        let currRow = board.[row]
        let setAt i x lst =
            if List.length lst > i && i >= 0 then
                lst.[0..i-1] @ x::lst.[i+1..]
            else lst
        let newCell = {board.[column].[row] with  Value = value; CellState = state}
        let newRow = setAt column newCell currRow
        let newBoard = setAt row newRow board
        newBoard

    let Validate f idx (board:Board) =
        f idx board
        |> Rules.Correct

type Grid = Row []
module Grid =
    let Get (row,col) (board:Board) : Grid =
        [|
            board.[row].[col..col+2] 
            board.[row+1].[col..col+2] 
            board.[row+2].[col..col+2] 
        |]

    let Correct (g:Grid) =
        g
        |> List.concat
        |> Rules.Correct

    let AllGrids (b:Board) =
        [
            0,0
            0,3
            0,6
            3,0
            3,3
            3,6
            6,0
            6,3
            6,6
        ]
        |> List.map(fun coords -> Get coords b)

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
        ActiveCell: int * int}

module Game =
    let private rulesCheck f (g: Game)=
        //Need to check for empty/0 cells
        [|0..8|]
        |> Array.map (fun i -> Board.Validate f i g.Board)
        |> Array.filter (fun v -> v = false) 
        |> (fun arr -> arr.Length = 0)

    let private trueBind f x =
        if x then f else false

    let CheckSolution (g:Game) =
        //Result pattern?
        //Need to check for empty/0 cells
        rulesCheck Board.GetRow g
        |> trueBind (rulesCheck Board.GetColumn g)
        |> trueBind (
            Grid.AllGrids g.Board
            |> List.map Grid.Correct
            |> List.distinct
            |> List.length = 1
            )