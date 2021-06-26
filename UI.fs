namespace UI

module ConsoleOutput =
    open System
    open GameParts

    let Init() =   
        Console.CursorVisible <- false
        Console.Clear()

    module private Color =
        let private setColor background foreground =
            Console.BackgroundColor <- background
            Console.ForegroundColor <- foreground

        let private white = ConsoleColor.White
        let private black = ConsoleColor.Black
        let private green = ConsoleColor.DarkGreen
        let private red = ConsoleColor.DarkRed

        let private cursorColor() = setColor white black 
        let private wrongCursorColor() = setColor white red
        let private givenColor() = setColor green white
        let private wrongColor() = setColor red white
        let private normalColor() = setColor black white

        let private colorMap =
            [   "cursor", cursorColor()
                "wrongCursor", wrongCursorColor()
                "given", givenColor()
                "wrong", wrongColor()
                "normal",normalColor()
            ]
            |> Map.ofList

        // Should proably move to cell module and make more generic?
        // Need to rework option logic?
        let CheckCell i c g = 
            match c with 
            | Some v -> 
                match v with 
                |  Given _ -> colorMap.["given"]
                | _ -> 
                    let wrong = g.DuplicateCells.Contains i || g.EmptyCells.Contains i 
                    let cursor = g.Cursor = i

                    match cursor,wrong with
                    | false,true -> colorMap.["wrong"]
                    | true,true -> colorMap.["wrongCursor"]
                    | true,false -> colorMap.["cursor"]
                    | false,false -> colorMap.["normal"]
            | None ->
                let wrong = g.DuplicateCells.Contains i || g.EmptyCells.Contains i 
                let cursor = g.Cursor = i

                match cursor,wrong with
                | false,true -> colorMap.["wrong"]
                | true,true -> colorMap.["wrongCursor"]
                | true,false -> colorMap.["cursor"]
                | false,false -> colorMap.["normal"]


    let private printCell i o g =
        Color.CheckCell i o g
        match o with
        | None -> printf "| "
        | Some c -> printf "|%i" (Cell.ToInt c)

    // let private printRow (r:Row) =
    //     r |> Array.iter printCell

    // let private showRow ((r:Row), p) =
    //     match p with
    //     | Top ->
    //         printfn "___________________"
    //         printRow r
    //         printfn "|"
    //     | Middle ->
    //         printRow r
    //         printfn "|"
    //     | Bottom ->
    //         printRow r
    //         printfn "|"
    //         printfn "-------------------"

    let DisplayMessage pos s =
        Console.SetCursorPosition pos
        printfn "%s" s

    // let DrawBoard g =
    //     Console.SetCursorPosition (0,0)
    //     [|0..8|]
    //     |> Array.map (fun i ->
    //         let p =
    //             match i with
    //             | 0 -> Top
    //             | 1 | 4 | 7 | 3 | 6 -> Middle
    //             | 2 | 5 | 8 -> Bottom
    //             | _ -> Bottom

    //         (Board.GetRow i g.Board), p)
    //     |> Array.iter showRow

    let GameOver() =
        DisplayMessage (0,14) "Game Over, you win!" 
        DisplayMessage (0,15) "Press enter to quit" 
        Console.ReadLine() |> ignore