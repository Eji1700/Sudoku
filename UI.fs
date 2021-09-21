namespace UI
open System
open GameParts

module ConsoleOutput =
    let Init() =   
        Console.CursorVisible <- false
        Console.Clear()

    let private clearRow pos =
        Console.SetCursorPosition pos
        printf "                                "
        pos

    let DisplayMessage s pos =
        Console.SetCursorPosition pos
        printfn "%s" s
        
    let CDisplayMessage s pos =
        clearRow pos
        |> DisplayMessage s

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
        let private normalColor() = setColor black white

        let ColorMap = 
            [|  "cursor", cursorColor
                "wrongCursor", wrongCursorColor
                "given", givenColor
                "wrong", wrongColor
                "normal", normalColor|]
            |> Map.ofArray

        let private nonGiven i g =
            let wrong = g.DuplicateCells.Contains i || g.EmptyCells.Contains i 
            let cursor = g.Cursor = i

            match cursor,wrong with
            | false, true -> ColorMap.["wrong"]()
            | true, true -> ColorMap.["wrongCursor"]()
            | true, false -> ColorMap.["cursor"]()
            | false, false -> ColorMap.["normal"]()

        // Should proably move to cell module and make more generic?
        let CheckCell idx optCell g = 
            match optCell with 
            | None -> nonGiven idx g
            | (Some (Given _)) -> givenColor()
            | _ -> nonGiven idx g

    let private printCell g optCell idx =
        Color.CheckCell idx optCell g
        match optCell with
        | None -> printf "| "
        | Some c -> printf "|%i" (Cell.ToInt c)

    let DrawBoard g =
        Console.SetCursorPosition (0,0)
        g.Board
        |> Array2D.iteri(fun x y (cell,_) ->
            match x, y with 
            | 0, 0 -> printfn "___________________"
            
            | 3, 0 
            | 6, 0 -> 
                Color.ColorMap.["normal"]()
                printfn "-------------------"
            
            | _ -> ()

            printCell g cell (x,y)

            if y = 8 then 
                Color.ColorMap.["normal"]()
                printfn "|"
        )
        printfn "-------------------"

    let GameOver g =
        DrawBoard g
        CDisplayMessage "Game Over, you win!" (0, 14)
        CDisplayMessage "Press enter to quit." (0, 15)  
        // Should eventually call a main menu thing
        Console.ReadLine() |> ignore