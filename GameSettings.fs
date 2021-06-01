namespace GameSettings
open GameParts

module Initial =
  let private board  =
    array2D [|
      [|3;0;0;5;7;0;2;4;0|]
      [|5;7;2;0;4;0;9;0;0|]
      [|0;0;4;0;2;6;5;0;3|]
      [|7;0;3;0;0;2;0;8;1|]
      [|4;2;0;1;0;7;0;0;9|]
      [|0;0;6;0;3;4;7;0;5|]
      [|6;3;0;2;0;5;0;9;0|]
      [|0;4;0;7;6;3;0;5;0|]
      [|0;8;5;0;0;0;6;3;7|]
    |] 
    |> Board.Create

  let Game = 
    { Board = board 
      State = StartGame
      ActiveCell = 1,0}