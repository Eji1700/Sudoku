namespace GameSettings
open GameParts

module Initial =
  let private board : Board =
    [|
      [|6;0;0;1;0;8;2;0;3|]
      [|0;2;0;0;4;0;0;9;0|]
      [|8;0;3;0;0;5;4;0;0|]
      [|5;0;4;6;0;7;0;0;9|]
      [|0;3;0;0;0;0;0;5;0|]
      [|7;0;0;8;0;3;1;0;2|]
      [|0;0;1;7;0;0;9;0;6|]
      [|0;8;0;0;3;0;0;2;0|]
      [|3;0;2;9;0;4;0;0;5|]
    |]
    |> Board.Create

  let Game = 
    { Board = board 
      State = StartGame
      ActiveCell = 0,0}