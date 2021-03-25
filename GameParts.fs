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
        {   Value = v
            CellState = s 
        }

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

type Row = Cell []
type Column = Cell []

// type Grid = Row []
// module Grid =
//     let GetColumn i (g:Grid): Column  =
//         [| g.[0].[i]
//            g.[1].[i]
//            g.[2].[i] |]

//     let ToGrid g  =
//         g
//         |> Array.map(
//             Array.map( fun v ->
//                 if v <= 0 || v > 9 then 
//                     Cell.Create Unselected Empty
//                 else 
//                     Cell.Create Unselected (Value v)
//             )
//         )

//     let Correct (g:Grid) =
//         g
//         |> Array.concat
//         |> Rules.Correct

type Board = Row [] []
module Board  =
    let GetColumn col (board:Board)  =
        for i in 0..8 do
            printfn "%i" i

    let GetRow row (board:Board) : Row =
        let boardRow, gridRow = BoardConvert row
        board.[boardRow]
        |> Array.collect(fun r -> r.[gridRow])

    let private getCellIndex row column =
        let boardRow, gridRow = BoardConvert row
        let boardCol, gridCol = BoardConvert column
        boardCol, boardRow, gridCol, gridRow
       
    let ChangeCellState row column state (board:Board) =
        let boardCol, boardRow, gridCol, gridRow = getCellIndex  row column
        board.[boardCol].[boardRow].[gridCol].[gridRow] <-
            {board.[boardCol].[boardRow].[gridCol].[gridRow] with CellState = state}

    let ChangeCellValue row column value (board:Board) =
        let boardCol, boardRow, gridCol, gridRow = getCellIndex  row column
        board.[boardCol].[boardRow].[gridCol].[gridRow] <-
            {board.[boardCol].[boardRow].[gridCol].[gridRow] with Value = value}

    let Validate f idx (board:Board) =
        f idx board
        |> Rules.Correct

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
        [|1..9|]
        |> Array.map(fun i -> Board.Validate f i g.Board)
        |> Array.distinct
        |> Array.length = 1

    let private trueBind f x =
        if x then f else false

    let CheckSolution (g:Game) =
        //Result pattern?
        rulesCheck Board.GetRow g
        |> trueBind (rulesCheck Board.GetColumn g)
        |> trueBind (
            g.Board
            |> Array.collect(Array.map Grid.Correct)
            |> Array.distinct
            |> Array.length = 1)