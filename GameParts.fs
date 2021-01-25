namespace GameParts
open Helpers

type Cell =
    | Value of int
    | Empty

module Rules =    
    let private isUnique arr =
        arr |> Array.distinct |> Array.length = (arr |> Array.length)

    let private noEmpty arr =
        arr |> Array.contains Empty |> not

    let Correct arr =
        isUnique arr && noEmpty arr

type Postiion = Top | Middle | Bottom
type Row = Cell []
type Column = Cell []
type Diagonal = Cell []

type Grid = Row []
module Grid =
    let GetColumn i (g:Grid): Column  =
        [| g.[0].[i]
           g.[1].[i]
           g.[2].[i] |]

    let ToGrid g  =
        g
        |> Array.map(
            Array.map( fun v ->
                if v = 0 then Empty
                else Value v))

    let Correct (g:Grid) =
        g
        |> Array.concat
        |> Rules.Correct

type Board = Grid [] []
module Board  =
    let GetColumn col (board:Board)  =
        let boardCol, gridCol = BoardConvert col
        board
        |> Array.collect(fun r -> Grid.GetColumn gridCol r.[boardCol])

    let GetRow row (board:Board) : Row =
        let boardRow, gridRow = BoardConvert row
        board.[boardRow]
        |> Array.collect(fun r -> r.[gridRow])

    let GetCell row column (board:Board) =
        let boardRow, gridRow = BoardConvert row
        let boardCol, gridCol = BoardConvert column
        board.[boardRow].[boardCol].[gridRow].[gridCol]

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
    {Board: Board; State: State;}

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