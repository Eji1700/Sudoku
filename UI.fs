namespace UI

module ConsoleOutput =
    open System
    open GameParts

    let private setColor background foreground =
        Console.BackgroundColor <- background
        Console.ForegroundColor <- foreground

    let private White = ConsoleColor.White
    let private Black = ConsoleColor.Black
    let private Green = ConsoleColor.DarkGreen
    let private Red = ConsoleColor.DarkRed

    let private checkCellColor c =
        let b, f = Console.BackgroundColor, Console.ForegroundColor
        match c.CellState with
        | Selected -> setColor White Black
        | Marked -> setColor Green White
        | Given -> setColor White Red
        | Wrong -> setColor Red White
        | Unselected -> ()
        b, f

    let private printCell c =
        let b,f = checkCellColor c
        match c.Value with
        | Empty -> printf "| "
        | Value v -> printf "|%i" v
        setColor b f 

    let private printRow (r:Row) =
        r
        |> Array.iter printCell

    let private showRow ((r:Row), p) =
        match p with
        | Top ->
            printfn "___________________"
            printRow r
            printfn "|"
        | Middle ->
            printRow r
            printfn "|"
        | Bottom ->
            printRow r
            printfn "|"
            printfn "-------------------"
             
    let Init() =   
        Console.CursorVisible <- false
        Console.Clear()

    let DisplayMessage pos s =
        Console.SetCursorPosition pos
        printfn "%s" s

    let DrawBoard g =
        Console.SetCursorPosition (0,0)
        [|0..8|]
        |> Array.map (fun i ->
            let p =
                match i with
                | 0 -> Top
                | 1 | 4 | 7 | 3 | 6 -> Middle
                | 2 | 5 | 8 -> Bottom
                | _ -> Bottom

            (Board.GetRow i g.Board), p)
        |> Array.iter showRow

    let GameOver() =
        DisplayMessage (0,14) "Game Over, you win!" 
        DisplayMessage (0,15) "Press enter to quit" 
        Console.ReadLine() |> ignore