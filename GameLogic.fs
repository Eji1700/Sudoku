module GameLogic
open GameParts
open UI
open System

let private (|KeyPress|) g input =
    if input = ConsoleKey.Escape then 
        {g with State = Quit}
    elif input = ConsoleKey.Spacebar then 
        {g with State = CheckData}
    else 
        {g with State = Running}

let private checkInput g input  =
    match input with 
    | KeyPress g newG-> newG 

let rec GameLoop (g: Game) =
    match g.State with
    | EnterData -> ()
    | CheckData ->
        let s =  Game.CheckSolution g
        printfn "Correct?: %b" s
        if s then 
            {g with State = GameOver}
            |> GameLoop
        else
            printfn "Incorrect, keep trying"
            {g with State = Running}
            |> GameLoop
    | StartGame -> 
        printfn "Press enter to continue. While playing ESC to quit and Space to check your answer"
        Console.ReadLine() |> ignore
        GameLoop {g with State = DrawBoard}
    | DrawBoard ->
        ConsoleOutput.DrawBoard g
        GameLoop {g with State = Running}
    | Running -> 
        let r = Console.KeyAvailable
        if r then
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