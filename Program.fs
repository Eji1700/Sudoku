open GameSettings
open GameLogic

[<EntryPoint>]
let main argv =
    UI.ConsoleOutput.Init() //remove me
    // match argv with
    // | [||] -> ()
    // | _ -> 
    //     UI.ConsoleOutput.Init()
    
    // todo
    // Marked/Wrong options
    // Result patttern instead of true bind
    // Dear god the Console UI
    // At least need a "clear line" function to help blank things out
    GameLoop Initial.Game
    0