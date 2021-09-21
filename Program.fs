open GameSettings
open GameLogic

[<EntryPoint>]
let main argv =
    UI.ConsoleOutput.Init() //remove me and put in argu console parsing?    
    GameLoop Initial.Game
    UI.ConsoleOutput.DrawBoard Initial.Game
    0

    // todo
    // Redo types to prevent bad states.
    // Mark cell for testing with m
    // Dear god the Console UI
    // Argu integration and eventually real frontend.
    // Dynamic active starting cell so as not to screw up puzzles