namespace UI
open GameParts
module ConsoleOutput =
    open System
    
    let private consoleConvert row =
        match row with 
        | 1 -> 1
        | 2 -> 2
        | 3 -> 3
        | 4 -> 5
        | 5 -> 6
        | 6 -> 7
        | 7 -> 9
        | 8 -> 10
        | 9 -> 11
        | _ -> failwith "ConsoleConvert takes 1-9"

    let private printCell c =
        
        match c.Value with
        | Empty ->    printf "| |"
        | Value v -> printf "|%i" v

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
        Console.BackgroundColor <- ConsoleColor.DarkRed

    let DisplayMessage pos s =
        Console.SetCursorPosition pos
        printfn "%s" s

    let DrawBoard g =
        Console.SetCursorPosition (0,0)
        [|1..9|]
        |> Array.map (fun i ->
            let p =
                match i with
                | 1 -> Top
                | 2 | 5 | 8 | 4 | 7 -> Middle
                | 3 | 6 | 9 -> Bottom
                | _ -> Bottom

            (Board.GetRow i g.Board), p)
        |> Array.iter showRow

    let GameOver() =
        printfn "Game Over, you win!"
        printfn "Press enter to quit"
        Console.ReadLine() |> ignore