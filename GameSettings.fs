namespace GameSettings
open GameParts

module Initial =
  let private board : Board =
    [|
      [|0;0;3;0;4;2;0;9;0|]
      [|0;9;0;0;6;0;5;0;0|]
      [|5;0;0;0;0;0;0;1;0|]
      [|0;0;1;7;0;0;2;8;5|]
      [|0;0;8;0;0;0;1;0;0|]
      [|3;2;9;0;0;8;7;0;0|]
      [|0;3;0;0;0;0;0;0;1|]
      [|0;0;5;0;9;0;0;2;0|]
      [|0;8;0;2;1;0;6;0;0|]
    |]
    |> Board.Create

  let Game = 
    { Board = board 
      State = StartGame
      ActiveCell = 0,0}