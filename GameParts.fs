namespace GameParts
open System

module Array2D = 
    let toList (arr: 'T [,]) = arr |> Seq.cast<'T> |> Seq.toList

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

type Cell =
    | Entered of Value
    | Given of Value

module Cell =
    let Create i =
        Option.bind Value.ConvertInt i  
        |> fun v -> 
            match v with 
            | Some v -> v |> Given |> Some
            | None -> None 

type Position = Top | Middle | Bottom
type Index = int * int
type Row = ((Cell option) * Index)[]        
type Column = ((Cell option) * Index)[]
type Grid = ((Cell option) * Index)[,]
module Grid =
    let AllGrids =
        [|  0,0
            0,3
            0,6
            3,0
            3,3
            3,6
            6,0
            6,3
            6,6|]

module Duplicates =    
    let getDupes f arr =
        arr
        |> f
        |> List.groupBy(fun (c,_) -> c)
        |> List.choose(fun(v, lst) ->
            if lst.Length > 1 && v <> None
            then 
                lst
                |> List.map (fun (_,idx) -> idx )
                |> Some
            else None
        )
        |> List.concat

    let Row (r:Row) = 
        getDupes Array.toList r

    let Column (c:Column) =
        getDupes Array.toList c

    let Grid (g:Grid) =
        getDupes Array2D.toList g

type Board = ((Cell option) * (Index)) [,] 
module Board  =
    let Create arr : Board =
        arr 
        |> Array2D.mapi(fun x y i ->
            (Cell.Create i), (x,y)
        ) 
        
    let GetRow (board:Board) row : Row =
        board.[row, *]

    let GetColumn (board:Board) col : Column  =
        board.[*, col] 

    let GetGrid (board:Board) (row,col) : Grid =
        board.[row..row+2, col..col+2]

    let GetAllGrids b =
        Grid.AllGrids
        |> Array.map (GetGrid b)

    let private getDupes get dupes b =
       [|0..8|]
       |> Array.map (get b)
       |> Array.map dupes
       |> Array.toList
       |> List.concat

    let getRowDupes b =
        getDupes GetRow Duplicates.Row b

    let getColDupes b =
        getDupes GetColumn Duplicates.Column b

    let getGridDupes b =
        GetAllGrids b
        |> Array.map Duplicates.Grid
        |> Array.toList
        |> List.concat

    let GetDupes b =
        getRowDupes b
        |> List.distinct

    let GetEmpty (b:Board) =
        b
        |> Array2D.map(fun c ->
            let v,i = c
            match v with 
            | None -> Some i
            | Some _ -> None
        )
        |> Array2D.toList
        |> List.choose id

type GameState =
    | EnterData
    | CheckData
    | StartGame
    | DrawBoard
    | Running
    | GameOver
    | Quit

type Altered =
    | Marked
    | Wrong

type AlteredCells = (int * int * Altered) list
type Cursor = Index

type Game = 
    {   Board: Board
        State: GameState
        Cursor: Cursor
        Empty: Index list
        Duplicates: Index list}

module Game =
    let GetEmpty g =
        {g with Empty = (Board.GetEmpty g.Board)}

    let GetDupes g =
        {g with Duplicates = (Board.GetDupes g.Board)}