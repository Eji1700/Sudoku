open GameSettings
open GameLogic

[<EntryPoint>]
let main argv =
    let g =  Initial.Game
    GameLoop g
    0