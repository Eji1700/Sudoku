module GameLogic
open GameParts
open UI
open System

let rec GameLoop (g: Game) =
    match g.State with
    | StartGame -> 
        ConsoleOutput.DisplayMessage "Press enter to continue. While playing ESC to quit and Space to check your answer." (0,0)
        ConsoleOutput.DisplayMessage "C can be used to clear errors after checking." (0,1)
        Console.ReadLine() |> ignore
        Console.Clear()
        GameLoop { g with State = DrawBoard }
        
    | CheckData ->
        ConsoleOutput.DrawBoard g

        let newG = Game.UpdateIncorrectCells g
        let solved =  Game.CheckSolution newG
        let message = sprintf "Correct?: %b" solved
        ConsoleOutput.DisplayMessage message (0,14)

        if solved then { newG with State = GameOver } |> GameLoop
        else
            ConsoleOutput.DisplayMessage "Incorrect, keep trying" (0,15) 
            { newG with State = DrawBoard } |> GameLoop

    | DrawBoard ->
        ConsoleOutput.DrawBoard g
        GameLoop { g with State = Running }

    | Running -> 
        let r = Console.KeyAvailable
        if r |> not then { g with State = Running } |> GameLoop
        else
            Console.ReadKey(true).Key
            |> Input.Check g
            |> GameLoop

    | GameOver -> ConsoleOutput.GameOver g 
    | Quit -> () //change if want to not be unit