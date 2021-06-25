namespace GameParts
open System

module Array2D = 
    let toArray (arr: 'T [,]) = arr |> Seq.cast<'T> |> Seq.toArray

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
        |> Option.bind( Given >> Some)

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

module private Duplicates =    
    let private getDupes f arr =
        arr
        |> f //to allow grids to convert to array, just skipped otherwise with id call.
        |> Array.groupBy(fun (c,_) -> c)
        |> Array.choose(fun(v, arr) ->
            if arr.Length > 1 && v <> None
            then 
                arr
                |> Array.map(fun (_,idx) -> idx )
                |> Some
            else None
        )
        |> Array.concat

    let Row (r:Row) = 
        getDupes id r

    let Column (c:Column) =
        getDupes id c

    let Grid (g:Grid) =
        getDupes Array2D.toArray g

type Board = ((Cell option) * (Index)) [,] 
module Board  =
    let Create arr : Board =
        arr 
        |> Array2D.mapi(fun x y i ->
            (Cell.Create i), (x,y)
        ) 
        
    let GetRow (board:Board) row : Row =
        board.[row, *]

    let GetAllRows board =
        [|0..8|]
        |> Array.map(GetRow board)

    let GetColumn (board:Board) col : Column  =
        board.[*, col] 

    let GetAllColumns board =
        [|0..8|]
        |> Array.map(GetColumn board)

    let GetGrid (board:Board) (row,col) : Grid =
        board.[row..row+2, col..col+2]

    let GetAllGrids b =
        Grid.AllGrids
        |> Array.map(GetGrid b)

    let private getDupes getall dupes b =
       getall b
       |> Array.collect dupes

    let private getRowDupes b =
        getDupes GetAllRows Duplicates.Row b

    let private getColDupes b =
        getDupes GetAllColumns Duplicates.Column b

    let private getGridDupes b =
        getDupes GetAllGrids Duplicates.Grid b

    let GetDupes b =
        [|getRowDupes b; getColDupes b; getGridDupes b |]
        |> Array.concat
        |> Array.distinct
        |> Set.ofArray
        
    let GetEmpty (b:Board) =
        b
        |> Array2D.map(fun c ->
            let v,i = c
            match v with 
            | None -> Some i
            | Some _ -> None
        )
        |> Array2D.toArray
        |> Array.choose id
        |> Set.ofArray

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
        EmptyCells: Index Set
        DuplicateCells: Index Set}

module Game =
    let GetEmpty g =
        {g with EmptyCells = (Board.GetEmpty g.Board)}

    let GetDupes g =
        {g with DuplicateCells = (Board.GetDupes g.Board)}