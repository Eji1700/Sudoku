module GameLogic
open GameParts
open UI
open System

let private checkInput g (input: ConsoleKeyInfo)  =
    match input with 
    | i when i.Key = ConsoleKey.Escape -> 
        {g with State = Quit}
    | i when i.Key = ConsoleKey.Spacebar -> 
        {g with State = CheckData}
    | _ -> 
        {g with State = Running}

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
    | StartGame -> ()
    | Running -> 
        ConsoleOutput.DrawBoard g
        let r = Console.KeyAvailable
        if r then
            {g with State = Running}
            |> GameLoop
        else
            Console.ReadKey(true)
            |> checkInput g
            |> GameLoop
    | GameOver -> 
        ConsoleOutput.GameOver()
    | Quit ->
        ()