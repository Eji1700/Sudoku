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

module private Input = 
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
        let s =  Game.CheckSolution newG
        let m = sprintf "Correct?: %b" s
        ConsoleOutput.DisplayMessage m (0,14)

        if s then { newG with State = GameOver } |> GameLoop
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