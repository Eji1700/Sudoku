namespace GameSettings
open GameParts
open System.IO

module Initial =
  let private defaultBoard  =
    [|
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
    |> array2D
    |> Board.Create

  let private fileBoard =
    let sourceDir = __SOURCE_DIRECTORY__
    let data = 
      Directory.GetFiles(sourceDir,"board.txt") 
      |> Array.head
      |> File.ReadLines

    data
    |> Seq.map (fun line -> line.Split(","))
    |> Seq.map( fun row -> row |> Array.map (fun s -> s |> int))
    |> array2D 
    |> Board.Create

  let Game = 
    { Board = fileBoard 
      State = StartGame
      ActiveCell = 0,1 }