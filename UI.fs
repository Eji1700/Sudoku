namespace UI
open GameParts
open Cell

module ConsoleOutput =
    let private printCell c =
        match c with
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

    let DrawBoard g =
        let board, _ = g
        [|1..9|]
        |> Array.map (fun i ->
            let p =
                match i with
                | 1 -> Top
                | 2 | 5 | 8 | 4 | 7 -> Middle
                | 3 | 6 | 9 -> Bottom

            (Board.GetRow i board), p)
        |> Array.iter showRow