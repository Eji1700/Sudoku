open GameSettings
open GameLogic

[<EntryPoint>]
let main argv =
    GameLoop Initial.Game
    0