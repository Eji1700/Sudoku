namespace GameSettings
open GameParts
open GameLogic

module Initial =
    let private ulGrid: Grid=
        [|[|1;2;8|]
          [|5;3;4|]
          [|6;7;9|]|]
        |> Grid.ToGrid

    let private uGrid: Grid=
        [|[|3;4;5|]
          [|6;7;9|]
          [|1;8;2|]|]
        |> Grid.ToGrid

    let private urGrid: Grid=
        [|[|6;9;7|]
          [|2;1;8|]
          [|5;4;3|]|]
        |> Grid.ToGrid

    let private mlGrid: Grid=
        [|[|2;1;6|]
          [|4;8;5|]
          [|3;9;7|]|]
        |> Grid.ToGrid

    let private mGrid: Grid=
        [|[|4;3;8|]
          [|7;9;1|]
          [|5;2;6|]|]
        |> Grid.ToGrid

    let private mrGrid: Grid=
        [|[|7;5;9|]
          [|3;2;6|]
          [|4;8;1|]|]
        |> Grid.ToGrid

    let private blGrid: Grid=
        [|[|7;6;2|]
          [|9;4;3|]
          [|8;5;1|]|]
        |> Grid.ToGrid

    let private bGrid: Grid=
        [|[|9;1;4|]
          [|8;5;7|]
          [|2;6;3|]|]
        |> Grid.ToGrid

    let private brGrid: Grid=
        [|[|8;3;5|]
          [|1;6;2|]
          [|9;7;4|]|]
        |> Grid.ToGrid

    let private board: Board =
        [|[|ulGrid;uGrid;urGrid|]
          [|mlGrid;mGrid;mrGrid|]
          [|blGrid;bGrid;brGrid|]|]

    let Game = 
      {Board = board; State = StartGame}