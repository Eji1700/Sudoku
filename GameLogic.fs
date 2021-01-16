namespace GameLogic
open GameParts

type State =
    | EnterData
    | CheckData
    | StartGame
    | GameOver

type Game = 
    {Board: Board; State: State}

module Game =
    let private rulesCheck f (g: Game)=
        [|1..9|]
        |> Array.map(fun i -> Board.Validate f i g.Board)
        |> Array.distinct
        |> Array.length = 1

    let private trueBind f x =
        if x then f else false

    let CheckSolution (g:Game) =
        //Result pattern?
        rulesCheck Board.GetRow g
        |> trueBind (rulesCheck Board.GetColumn g)
        |> trueBind (
            g.Board
            |> Array.collect(Array.map Grid.Correct)
            |> Array.distinct
            |> Array.length = 1)

    let rec GameLoop (g: Game) =
        match g.State with
        | EnterData -> ()
        | CheckData -> ()
        | StartGame -> ()
        | GameOver -> ()