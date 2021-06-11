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

module Rules =    
    let private isUnique arr =
        arr |> Array.distinct |> Array.length = (arr |> Array.length)

    let private noEmpty arr =
        arr
        |> Array.contains None
        |> not

    let Correct arr = isUnique arr && noEmpty arr

type Position = Top | Middle | Bottom
type Row = Cell option []
module Row =
    let Wrong (r:Row) =
        let empty =
            r
            |> Array.filter(fun c -> c = None)
        
        let duplicates =
            r
            |> Array.groupBy id
            |> Array.choose(fun(key, arr) ->
                if arr.Length > 1 
                    then Some key
                    else None
            )
        empty, duplicates
        
type Column = Cell option []

type Board = Cell option [,] 
module Board  =
    let Create arr : Board =
        arr |> Array2D.map Cell.Create
        
    let GetColumn col (board:Board) : Column  =
        board.[*, col] 

    let GetRow row (board:Board) : Row =
        board.[row, *]

    let Validate f idx (board:Board) =
        f idx board
        |> Rules.Correct

    let GetEmpty (b:Board) =
        [|0..8|]
        |> Array.map(fun x ->
            GetRow x b
            |> Array.mapi(fun i c -> 
                match c with 
                | None -> None
                | Some _ -> Some i)
        )
        |> Array.mapi(fun x arr ->
            arr
            |> Array.mapi(fun y o ->
                match o with
                | None -> Some (x,y)
                | Some _ -> None
            )
            |> Array.toList
        )
        |> Array.map(List.choose id)
        |> Array.toList
        |> List.concat

type Grid = Cell option[,]
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
        Altered: AlteredCells}

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
            | Ok _ -> true //this eventually needs to be result all the way through
            | Error _ -> false