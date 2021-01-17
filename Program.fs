open UI
open GameSettings
open GameLogic
open System

[<EntryPoint>]
let main argv =
    let g =  Initial.Game
    ConsoleOutput.DrawBoard g
    GameLoop g
    0