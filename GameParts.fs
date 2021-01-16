namespace GameParts
open Cell
open Helpers

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