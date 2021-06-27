open GameSettings
open GameLogic

[<EntryPoint>]
let main argv =
    UI.ConsoleOutput.Init() //remove me and put in argu console parsing?    
    //GameLoop Initial.Game
    GameParts.Board.GetDupes Initial.dupeBoard
    |> printfn "%A"

    let test =
        [|
            [|1;2;3|]
            [|4;5;6|]
        |]
        |> array2D
        |> Array2D.mapi(fun x y i ->
            printfn "%A %A %A" x y i
        )

    0

    // todo
    // Redo types to prevent bad states.
    // Marked/Wrong options
    // Mark cell for testing with m
    // Highlight all errors on solution check, clear with e?
    // Dear god the Console UI
    // At least need a "clear line" function to help blank things out
    // Argu integration and eventually real frontend.
    // Dynamic active starting cell so as not to screw up puzzles