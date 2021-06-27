namespace UI
open System
open GameParts

module ConsoleOutput =
    let Init() =   
        Console.CursorVisible <- false
        Console.Clear()

    let DisplayMessage pos s =
        Console.SetCursorPosition pos
        printfn "%s" s

    module private Color =
        let private setColor background foreground =
            Console.BackgroundColor <- background
            Console.ForegroundColor <- foreground

        let private white = ConsoleColor.White
        let private black = ConsoleColor.Black
        let private blue = ConsoleColor.DarkBlue
        let private red = ConsoleColor.DarkRed

        let private cursorColor() = setColor white black 
        let private wrongCursorColor() = setColor white red
        let private givenColor() = setColor blue white
        let private wrongColor() = setColor red white
        let normalColor() = setColor black white


        let private nonGiven i g =
            let wrong = g.DuplicateCells.Contains i || g.EmptyCells.Contains i 
            let cursor = g.Cursor = i

            match cursor,wrong with
            | false, true -> wrongColor()
            | true, true -> wrongCursorColor()
            | true, false -> cursorColor()
            | false, false -> normalColor()

        // Should proably move to cell module and make more generic?
        // Need to rework option logic?
        let CheckCell i c g = 
            match c with 
            | None -> nonGiven i g
            | Some v -> 
                match v with 
                | Given _ -> givenColor()
                | _ -> nonGiven i g

    let private printCell g (o,i) =
        Color.CheckCell i o g
        match o with
        | None -> printf "| "
        | Some c -> printf "|%i" (Cell.ToInt c)

    let private printRow g (r:Row) =
        r |> Array.iter (printCell g) 

    let DrawBoard g =
        Console.SetCursorPosition (0,0)
        g.Board
        |> Array2D.iter(fun c ->
            let x, y = snd c
            match x,y with 
            | 0,0 -> printfn "___________________"
            | 3,0 
            | 6,0 -> 
                Color.normalColor()
                printfn "-------------------"
            | _ -> ()

            printCell g c

            if y = 8 then 
                Color.normalColor()
                printfn "|"
        )
        printfn "-------------------"

    let GameOver() =
        DisplayMessage (0,14) "Game Over, you win!" 
        DisplayMessage (0,15) "Press enter to quit" 
        Console.ReadLine() |> ignore