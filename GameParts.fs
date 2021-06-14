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

module private Rules =    
    let private duplicates f arr = 
        arr
        |> f
        |> List.groupBy id
        |> List.choose(fun(key, lst) ->
            if lst.Length > 1 
                then Some key
                else None
        )

    let RowColDupes arr = duplicates Array.toList arr
    let GridDupes arr = duplicates Array2D.toList arr 

type Position = Top | Middle | Bottom
type Row = Cell option []
module Row =
    let Duplicates (r:Row) =
        r |> Rules.RowColDupes
        
type Column = Cell option []
module Column =
    let Duplicates (c:Column) =
        c |> Rules.RowColDupes

type Grid = Cell option[,]
module Grid =
    let Duplicates (g:Grid) =
        g |> Rules.GridDupes

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

    // let Correct (g:Grid) =
    //     [| g.[0,*]; g.[1,*]; g.[2,*] |]
    //     |> Array.concat
    //     |> Rules.Correct

type Board = Cell option [,] 
module Board  =
    let Create arr : Board =
        arr |> Array2D.map Cell.Create
        
    let GetRow (board:Board) row : Row =
        board.[row, *]

    let GetColumn (board:Board) col : Column  =
        board.[*, col] 

    let GetGrid (board:Board) (row,col) : Grid =
        board.[row..row+2, col..col+2]

    let GetAllGrids b =
        Grid.AllGrids
        |> Array.map (GetGrid b)

    let private getDupes f b =
       [|0..8|]
       |> Array.map (f b)
       |> Array.map Rules.RowColDupes
       |> Array.toList
       |> List.concat

    let private getRowDupes b =
        getDupes GetRow b

    let private getColDupes b =
        getDupes GetColumn b

    let private getGridDupes b =
        b
        |> GetAllGrids
        |> Array.map Rules.GridDupes
        |> Array.toList
        |> List.concat

    let GetDupes (b:Board) =
        getRowDupes b @ getColDupes b @ getGridDupes b
        |> List.distinct

    let GetEmpty (b:Board) =
        b
        |> Array2D.mapi(fun x y c ->
            match c with 
            | None -> Some (x, y)
            | Some _ -> None
        )
        |> Array2D.toList
        |> List.choose id
    
    // let Validate f idx (board:Board) =
    //     f idx board
    //     |> Rules.Correct


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
type Cursor = int * int 

type Game = 
    {   Board: Board
        State: GameState
        Cursor: Cursor
        Empty: (int * int) list
        Duplicates: (int * int) list}

module Game =
    let GetEmpty g =
        {g with Empty = (Board.GetEmpty g.Board)}

    let GetDupes g =
        let dupes =
            let r = 
        

    // let private rulesCheck f (g: Game)=
    //     [|0..8|]
    //     |> Array.map (fun i -> Board.Validate f i g.Board)
    //     |> Array.filter (fun v -> v = false) 
    //     |> fun arr ->
    //         match arr.Length with 
    //         | 0 -> Ok g
    //         | _ -> Error g

    // let private rulesCheckGrid (g: Game)=
    //     Grid.AllGrids g.Board
    //     |> Array.map Grid.Correct
    //         |> Array.distinct
    //         |> fun arr ->
    //             match arr.Length with 
    //             | 1 -> Ok g
    //             | _ -> Error g

    // let CheckSolution (g:Game) =
    //     rulesCheck Board.GetRow g
    //     |> Result.bind (rulesCheck Board.GetColumn)
    //     |> Result.bind (rulesCheckGrid)
    //     |> fun r ->
    //         match r with 
    //         | Ok _ -> true //this eventually needs to be result all the way through
    //         | Error _ -> false