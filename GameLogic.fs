module GameLogic
open GameParts
open UI
open System

let private checkInput g input  =
    match input with 
    | ConsoleKey.Escape -> 
        {g with State = Quit}
    | ConsoleKey.Spacebar ->
        {g with State = CheckData}
    | _ -> g 

let rec GameLoop (g: Game) =
    match g.State with
    | EnterData -> ()
    | CheckData ->
        let s =  Game.CheckSolution g
        sprintf "Correct?: %b" s
        |> ConsoleOutput.DisplayMessage
        if s then 
            {g with State = GameOver}
            |> GameLoop
        else
            ConsoleOutput.DisplayMessage "Incorrect, keep trying"
            {g with State = Running}
            |> GameLoop
    | StartGame -> 
        ConsoleOutput.DisplayMessage "Press enter to continue. While playing ESC to quit and Space to check your answer"
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