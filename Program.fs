open UI
open GameSettings
open GameLogic
open System

[<EntryPoint>]
let main argv =
    let g =  Initial.Game
    ConsoleOutput.DrawBoard g
    Game.CheckSolution g
    |> printfn "Correct? = %b"
    Console.ReadLine() |> ignore
    0