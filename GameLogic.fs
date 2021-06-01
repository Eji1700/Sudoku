module GameLogic
open GameParts
open UI
open System

type Direction =
    | Up
    | Down
    | Left
    | Right

module private Direction =
    let Get d amount = 
        match d with 
        | Up ->  (amount * -1, 0)
        | Down -> (amount, 0)
        | Left -> (0, amount * -1)
        | Right -> (0, amount) 

let private boundryCheck x y =
    x < 9 && 
    y < 9 &&
    x > -1 &&
    y > -1

let rec private moveCell direction amount g =
    let x, y = g.ActiveCell
    let xAdj, yAdj = Direction.Get direction amount
    let newX = x + xAdj
    let newY = y + yAdj
    
    if boundryCheck newX newY then 
        match  g.Board.[newX, newY].CellState with  
        | Given ->  
            let incr = amount + 1
            moveCell direction incr g
        | _ ->
            let newBoard = Board.ChangeCellState x y Unselected g.Board
            let newestBoard = Board.ChangeCellState newX newY Selected newBoard
            {g with 
                Board = newestBoard
                ActiveCell = newX, newY
                State = DrawBoard}
    else
        {g with State = Running}

let private cellChange v g =
    let r,c = g.ActiveCell
    {g with 
        Board = Board.ChangeCellValue r c (Value v) g.Board
        State = DrawBoard}  

let private checkInput g input  =
    match input with 
    | ConsoleKey.Escape -> 
        {g with State = Quit}
    | ConsoleKey.Spacebar ->
        {g with State = CheckData}
    | ConsoleKey.K 
    | ConsoleKey.S
    | ConsoleKey.DownArrow  ->
        moveCell Down 1 g
    | ConsoleKey.I
    | ConsoleKey.W
    | ConsoleKey.UpArrow  ->
        moveCell Up 1 g
    | ConsoleKey.J
    | ConsoleKey.A
    | ConsoleKey.LeftArrow  ->
        moveCell Left 1 g
    | ConsoleKey.L
    | ConsoleKey.D
    | ConsoleKey.RightArrow  ->
        moveCell Right 1 g
    | ConsoleKey.D0 -> 
        cellChange 0 g 
    | ConsoleKey.D1 ->
        cellChange 1 g  
    | ConsoleKey.D2 ->
        cellChange 2 g   
    | ConsoleKey.D3 ->
        cellChange 3 g   
    | ConsoleKey.D4 ->
        cellChange 4 g  
    | ConsoleKey.D5 ->
        cellChange 5 g   
    | ConsoleKey.D6 ->
        cellChange 6 g 
    | ConsoleKey.D7 ->
        cellChange 7 g  
    | ConsoleKey.D8 ->
        cellChange 8 g 
    | ConsoleKey.D9 ->
        cellChange 9 g 
    | _ -> g 

let rec GameLoop (g: Game) =
    match g.State with
    | StartGame -> 
        ConsoleOutput.DisplayMessage (0,0) "Press enter to continue. While playing ESC to quit and Space to check your answer"
        Console.ReadLine() |> ignore
        Console.Clear()
        GameLoop {g with State = DrawBoard}
    | EnterData -> ()
    | CheckData ->
        let s =  Game.CheckSolution g
        ConsoleOutput.DrawBoard g
        sprintf "Correct?: %b" s
        |> ConsoleOutput.DisplayMessage (0,14)
        if s then 
            {g with State = GameOver}
            |> GameLoop
        else
            ConsoleOutput.DisplayMessage (0,15) "Incorrect, keep trying"
            {g with State = Running}
            |> GameLoop
    | DrawBoard ->
        ConsoleOutput.DrawBoard g
        GameLoop {g with State = Running}
    | Running -> 
        let r = Console.KeyAvailable
        if r |> not then
            {g with State = Running}
            |> GameLoop
        else
            Console.ReadKey(true).Key
            |> checkInput g
            |> GameLoop
    | GameOver -> 
        ConsoleOutput.GameOver()
    | Quit ->
        ()