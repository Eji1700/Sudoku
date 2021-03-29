namespace GameSettings
open GameParts

module Initial =
  let private board : Board =
    [|
      [|1;2;8;3;4;5;6;9;7|]
      [|5;3;4;6;7;9;2;1;8|]
      [|6;7;9;1;8;2;5;4;3|]
      [|2;1;6;4;3;8;7;5;9|]
      [|4;8;5;7;9;1;3;2;6|]
      [|3;9;7;5;2;6;4;8;1|]
      [|7;6;2;9;1;4;8;3;5|]
      [|9;4;3;8;5;7;1;6;2|]
      [|8;5;1;2;6;3;9;7;4|]
    |]
    |> Board.Create

  let Game = 
    { Board = board 
      State = StartGame
      ActiveCell = 0,0}