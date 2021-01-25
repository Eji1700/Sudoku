open GameSettings
open GameLogic
open System

[<EntryPoint>]
let main argv =
    Console.CursorVisible <- false
    GameLoop Initial.Game
    0