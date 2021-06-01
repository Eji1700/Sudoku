open GameSettings
open GameLogic

[<EntryPoint>]
let main argv =
    UI.ConsoleOutput.Init() //remove me
    // match argv with
    // | [||] -> ()
    // | _ -> 
    //     UI.ConsoleOutput.Init()
    
    GameLoop Initial.Game
    0