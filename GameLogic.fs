namespace GameLogic
open GameParts

type Game = Board * State
module Game =
    let private rulesCheck f (g: Game)=
        let board, _ = g
        [|1..9|]
        |> Array.map(fun i -> Board.Validate f i board)
        |> Array.distinct
        |> Array.length = 1

    let private trueBind f x =
        if x then f else false

    let CheckSolution (g:Game) =
        //Result pattern?
        let board, _ = g
        rulesCheck Board.GetRow g
        |> trueBind (rulesCheck Board.GetColumn g)
        |> trueBind (
            board
            |> Array.collect(Array.map Grid.Correct)
            |> Array.distinct
            |> Array.length = 1)
