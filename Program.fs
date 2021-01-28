open GameSettings
open GameLogic

[<EntryPoint>]
let main argv =
    match argv with
    | [||] -> ()
    | _ -> 
        UI.ConsoleOutput.Init()
    GameLoop Initial.Game
    0