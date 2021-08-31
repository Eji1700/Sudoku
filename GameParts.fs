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

    let ToInt v =
        match v with 
        | V1 -> 1
        | V2 -> 2
        | V3 -> 3
        | V4 -> 4
        | V5 -> 5
        | V6 -> 6
        | V7 -> 7
        | V8 -> 8
        | V9 -> 9

type Cell =
    | Entered of Value
    | Given of Value

module Cell =
    let Create i =
        Option.bind Value.ConvertInt i  
        |> Option.bind( Given >> Some)

    let GetValue c = 
        match c with 
        | Entered v
        | Given v -> v

    let ToInt c =
        match c with 
        | Entered v
        | Given v  -> Value.ToInt v
    
    let ConvertKey k  =
        match Value.ConvertKey k with 
        | Some v -> Some (Entered v)
        | None -> None

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
    let private getDupes arr =
        arr
        |> Array.groupBy(fun (c,_) ->
            match c with 
            | Some c -> Some (Cell.GetValue c)
            | None -> None)
        |> Array.choose(fun(v, arr) ->
            if arr.Length > 1 && v <> None
            then 
                arr
                |> Array.map(fun (_,idx) -> idx )
                |> Some
            else None
        )
        |> Array.concat

    let private row (r:Row) = 
        getDupes r

    let private column (c:Column) =
        getDupes c

    let private grid (g:Grid) =
        g |> Array2D.toArray |> getDupes 

    let private all dupes b getAll =
       getAll b
       |> Array.collect dupes

    let private allRows b getAll =
        all getDupes b getAll
    
    let private allColumns b getAll =
        all getDupes b getAll

    let private allGrids b getAll =
        all grid b getAll

    let GetAll rows cols grids b =
        [|allRows b rows; allColumns b cols; allGrids b grids|]
        |> Array.concat
        |> Array.distinct
        |> Set.ofArray

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

    let GetDupes b =
        Duplicates.GetAll GetAllRows GetAllColumns GetAllGrids b 
        
    let GetEmpty b =
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

    let ChangeValue row col value (b:Board) =
        // i hate that this is mutable but i'm too tired to do it immutably.
        // I'll try later but for now returning a board just to setup the rest
        b.[row,col] <- (value,(row,col))
        b

type GameState =
    | CheckData
    | StartGame
    | DrawBoard
    | Running
    | GameOver
    | Quit

type Cursor = Index

type Direction =
    | Up
    | Down
    | Left
    | Right

module Direction =
    let Get d amount = 
        match d with 
        | Up ->  (amount * -1, 0)
        | Down -> (amount, 0)
        | Left -> (0, amount * -1)
        | Right -> (0, amount) 

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

    let UpdateIncorrectCells g =
        {g with 
            EmptyCells = Board.GetEmpty g.Board
            DuplicateCells = Board.GetDupes g.Board}

    let CheckSolution g =
        Set.isEmpty g.EmptyCells && Set.isEmpty g.DuplicateCells

module Input = 
    let private boundryCheck x y =
        x < 9 && 
        y < 9 &&
        x > -1 &&
        y > -1

    let rec moveCell direction amount g =
        let x, y = g.Cursor
        let xAdj, yAdj = Direction.Get direction amount
        let newX = x + xAdj
        let newY = y + yAdj
        
        if boundryCheck newX newY then 
            match  g.Board.[newX, newY] with  
            | (Some (Given _)),_ ->
                moveCell direction (amount + 1) g
            | _ ->
                { g with 
                    Cursor = newX,newY
                    State = DrawBoard }
        else 
            { g with State = Running }

    let private cellChange k g =
        let r,c = g.Cursor
        let v = Cell.ConvertKey k 
        { g with 
            Board = Board.ChangeValue r c v g.Board
            State = DrawBoard }  

    let Check g input  =
        match input with 
        | ConsoleKey.Escape -> { g with State = Quit }
        | ConsoleKey.Spacebar -> { g with State = CheckData }
        | ConsoleKey.P -> { g with State = GameOver }
        | ConsoleKey.C -> 
            {g with
                DuplicateCells = Set.empty
                EmptyCells = Set.empty
                State = DrawBoard}

        | ConsoleKey.K 
        | ConsoleKey.S
        | ConsoleKey.DownArrow -> moveCell Down 1 g

        | ConsoleKey.I
        | ConsoleKey.W
        | ConsoleKey.UpArrow -> moveCell Up 1 g

        | ConsoleKey.J
        | ConsoleKey.A
        | ConsoleKey.LeftArrow -> moveCell Left 1 g

        | ConsoleKey.L
        | ConsoleKey.D
        | ConsoleKey.RightArrow -> moveCell Right 1 g

        | ConsoleKey.D0 
        | ConsoleKey.NumPad0
        | ConsoleKey.D1 
        | ConsoleKey.NumPad1 
        | ConsoleKey.D2  
        | ConsoleKey.NumPad2  
        | ConsoleKey.D3   
        | ConsoleKey.NumPad3   
        | ConsoleKey.D4  
        | ConsoleKey.NumPad4  
        | ConsoleKey.D5   
        | ConsoleKey.NumPad5   
        | ConsoleKey.D6  
        | ConsoleKey.NumPad6  
        | ConsoleKey.D7  
        | ConsoleKey.NumPad7  
        | ConsoleKey.D8 
        | ConsoleKey.NumPad8 
        | ConsoleKey.D9
        | ConsoleKey.NumPad9 as k -> cellChange k g 
        | _ -> g 