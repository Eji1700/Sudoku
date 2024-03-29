let boardConvert idx =
    match idx with
    | 1 -> 0,0
    | 2 -> 0,1
    | 3 -> 0,2
    | 4 -> 1,0
    | 5 -> 1,1
    | 6 -> 1,2
    | 7 -> 2,0
    | 8 -> 2,1
    | 9 -> 2,2
    | _ -> failwith "boardCovert takes 1-9"

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
        let boardCol, gridCol = boardConvert col
        board
        |> Array.collect(fun r -> Grid.GetColumn gridCol r.[boardCol])

    let GetRow row (board:Board) : Row =
        let boardRow, gridRow = boardConvert row
        board.[boardRow]
        |> Array.collect(fun r -> r.[gridRow])

    let GetCell row column (board:Board) =
        let boardRow, gridRow = boardConvert row
        let boardCol, gridCol = boardConvert column
        board.[boardRow].[boardCol].[gridRow].[gridCol]

    let Validate f idx (board:Board) =
        f idx board
        |> Rules.Correct

type State =
    | EnterData
    | CheckData
    | StartGame
    | GameOver

type Game = Board * State
module Game =
    let private rulesCheck f (g: Game)=
        let board, _ = g
        [|1..9|]
        |> Array.map(fun i -> Board.Validate f i board)
        |> Array.distinct
        |> Array.length = 1

    let private trueBind f x =
        if x then f else false

    let CheckSolution (g:Game) =
        //Result pattern?
        let board, _ = g
        rulesCheck Board.GetRow g
        |> trueBind (rulesCheck Board.GetColumn g)
        |> trueBind (
            board
            |> Array.collect(Array.map Grid.Correct)
            |> Array.distinct
            |> Array.length = 1)

module UI =
    let private printCell c =
        match c with
        | Empty ->    printf "| |"
        | Value v -> printf "|%i" v

    let private printRow (r:Row) =
        r
        |> Array.iter printCell


    let private showRow ((r:Row), p) =
        match p with
        | Top ->
            printfn "___________________"
            printRow r
            printfn "|"
        | Middle ->
            printRow r
            printfn "|"
        | Bottom ->
            printRow r
            printfn "|"
            printfn "-------------------"

    let DrawBoard g =
        let board, _ = g
        [|1..9|]
        |> Array.map (fun i ->
            let p =
                match i with
                | 1 -> Top
                | 2 | 5 | 8 | 4 | 7 -> Middle
                | 3 | 6 | 9 -> Bottom
                | _ -> Bottom

            (Board.GetRow i board), p)
        |> Array.iter showRow

let ulGrid: Grid=
    [|[|1;2;8|]
      [|5;3;4|]
      [|6;7;9|]|]
    |> Grid.ToGrid

let uGrid: Grid=
    [|[|3;4;5|]
      [|6;7;9|]
      [|1;8;2|]|]
    |> Grid.ToGrid

let urGrid: Grid=
    [|[|6;9;7|]
      [|2;1;8|]
      [|5;4;3|]|]
    |> Grid.ToGrid

let mlGrid: Grid=
    [|[|2;1;6|]
      [|4;8;5|]
      [|3;9;7|]|]
    |> Grid.ToGrid

let mGrid: Grid=
    [|[|4;3;8|]
      [|7;9;1|]
      [|5;2;6|]|]
    |> Grid.ToGrid

let mrGrid: Grid=
    [|[|7;5;9|]
      [|3;2;6|]
      [|4;8;1|]|]
    |> Grid.ToGrid

let blGrid: Grid=
    [|[|7;6;2|]
      [|9;4;3|]
      [|8;5;1|]|]
    |> Grid.ToGrid

let bGrid: Grid=
    [|[|9;1;4|]
      [|8;5;7|]
      [|2;6;3|]|]
    |> Grid.ToGrid

let brGrid: Grid=
    [|[|8;3;5|]
      [|1;6;2|]
      [|9;7;4|]|]
    |> Grid.ToGrid

let board: Board =
    [|[|ulGrid;uGrid;urGrid|]
      [|mlGrid;mGrid;mrGrid|]
      [|blGrid;bGrid;brGrid|]|]

let game = board, StartGame

let test () =
    UI.DrawBoard game
    printfn "Correct? = %b" (Game.CheckSolution game)