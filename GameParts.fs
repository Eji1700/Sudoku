namespace GameParts
open System

type Value = V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9
module Value =
    let ConvertKey k =
        match k with
        | ConsoleKey.D1
        | ConsoleKey.NumPad1 -> Some V1
        | ConsoleKey.D2
        | ConsoleKey.NumPad2 -> Some V2
        | ConsoleKey.D3
        | ConsoleKey.NumPad3 -> Some V3
        | ConsoleKey.D4
        | ConsoleKey.NumPad4 -> Some V4
        | ConsoleKey.D5
        | ConsoleKey.NumPad5 -> Some V5
        | ConsoleKey.D6
        | ConsoleKey.NumPad6 -> Some V6
        | ConsoleKey.D7
        | ConsoleKey.NumPad7 -> Some V7
        | ConsoleKey.D8
        | ConsoleKey.NumPad8 -> Some V8
        | ConsoleKey.D9
        | ConsoleKey.NumPad9 -> Some V9
        | _ -> None

    let ConvertInt i =
        match i with 
        | 1 -> Some V1
        | 2 -> Some V2
        | 3 -> Some V3
        | 4 -> Some V4
        | 5 -> Some V5
        | 6 -> Some V6
        | 7 -> Some V7
        | 8 -> Some V8
        | 9 -> Some V9
        | _ -> None

type SelectableCell =
    | Entered of Value
    | Wrong of Value Option
    | Marked of Value option  
    | Empty

type Cell =
    | Given of Value
    | SelectableCell of SelectableCell

module Cell =
    let Create i =
        Option.bind Value.ConvertInt i  
        |> fun v ->
            match v with 
            | Some v -> Given v
            | None -> SelectableCell Empty

    let Mark (c: SelectableCell)  =
        match c with 
        | Wrong v -> Marked v
        | Entered v -> Marked (Some v)
        | Empty -> Empty
        | Marked v -> 
            match v with 
            | Some v -> Entered v
            | None -> Empty

type Cursor = int * int * SelectableCell 
module Cursor =
    let Move x y c : Cursor = x,y,c 

module Rules =    
    let private isUnique arr =
        arr |> Array.distinct |> Array.length = (arr |> Array.length)

    let private noEmpty arr =
        arr
        |> Array.contains Empty
        |> not

    let Correct arr = isUnique arr && noEmpty arr

type Position = Top | Middle | Bottom
type Row = Cell[]
type Column = Cell[]

type Board = Cell[,]
module Board  =
    let Create arr : Board =
        arr |> Array2D.map Cell.Create
        
    let GetColumn col (board:Board) : Column  =
        board.[*, col]

    let GetRow row (board:Board) : Row =
        board.[row, *]
       
    // let ChangeCellState row column state (board:Board): Board =
    //     board
    //     |> Array2D.mapi(fun r c cell ->
    //         if r = row && c = column then {cell with CellState = state}
    //         else cell )

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