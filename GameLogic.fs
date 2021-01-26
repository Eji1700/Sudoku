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