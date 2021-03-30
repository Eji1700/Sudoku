module GameLogic
open GameParts
open UI
open System

let private moveCell xAdj yAdj g =
    let x, y = g.ActiveCell
    let newX = x + xAdj
    let newY = y + yAdj
    if newX < 9 && 
        newY < 9 &&
        newX > -1 &&
        newY > -1   then 
        Board.ChangeCellState x y Unselected g.Board
        Board.ChangeCellState newX newY Selected g.Board
        {g with 
            ActiveCell = newX, newY
            State = DrawBoard}
    else
        {g with State = Running}

let rec private getValue() =
    ConsoleOutput.DisplayMessage (0,14) "Enter a value 0 - 9"
    let result = Console.ReadKey().Key
    match result with
    | ConsoleKey.D0
    | ConsoleKey.D1
    | ConsoleKey.D2
    | ConsoleKey.D3
    | ConsoleKey.D4
    | ConsoleKey.D5
    | ConsoleKey.D6
    | ConsoleKey.D7
    | ConsoleKey.D8
    | ConsoleKey.D9 ->
        printfn "valid key"
    | _ ->
        ConsoleOutput.DisplayMessage (0,14) "Enter a value 0 - 9"
        getValue()

let private checkInput g input  =
    match input with 
    | ConsoleKey.Escape -> 
        {g with State = Quit}
    | ConsoleKey.Spacebar ->
        {g with State = CheckData}
    | ConsoleKey.K 
    | ConsoleKey.S
    | ConsoleKey.DownArrow  ->
        moveCell 0 1 g
    | ConsoleKey.I
    | ConsoleKey.W
    | ConsoleKey.UpArrow  ->
        moveCell 0 -1 g
    | ConsoleKey.J
    | ConsoleKey.A
    | ConsoleKey.LeftArrow  ->
        moveCell -1 0 g
    | ConsoleKey.L
    | ConsoleKey.D
    | ConsoleKey.RightArrow  ->
        moveCell 1 0 g
    | ConsoleKey.D0 -> 
        let r,c = g.ActiveCell
        Board.ChangeCellValue r c (Value 0) g.Board
        {g with State = DrawBoard}   
    | ConsoleKey.D1 ->
        let r,c = g.ActiveCell
        Board.ChangeCellValue r c (Value 1) g.Board
        {g with State = DrawBoard}   
    | ConsoleKey.D2 ->
        let r,c = g.ActiveCell
        Board.ChangeCellValue r c (Value 2) g.Board
        {g with State = DrawBoard}   
    | ConsoleKey.D3 ->
        let r,c = g.ActiveCell
        Board.ChangeCellValue r c (Value 3) g.Board
        {g with State = DrawBoard}   
    | ConsoleKey.D4 ->
        let r,c = g.ActiveCell
        Board.ChangeCellValue r c (Value 4) g.Board
        {g with State = DrawBoard}   
    | ConsoleKey.D5 ->
        let r,c = g.ActiveCell
        Board.ChangeCellValue r c (Value 5) g.Board
        {g with State = DrawBoard}   
    | ConsoleKey.D6 ->
        let r,c = g.ActiveCell
        Board.ChangeCellValue r c (Value 6) g.Board
        {g with State = DrawBoard} 
    | ConsoleKey.D7 ->
        let r,c = g.ActiveCell
        Board.ChangeCellValue r c (Value 7) g.Board
        {g with State = DrawBoard} 
    | ConsoleKey.D8 ->
        let r,c = g.ActiveCell
        Board.ChangeCellValue r c (Value 8) g.Board
        {g with State = DrawBoard} 
    | ConsoleKey.D9 ->
        let r,c = g.ActiveCell
        Board.ChangeCellValue r c (Value 9) g.Board
        {g with State = DrawBoard} 
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