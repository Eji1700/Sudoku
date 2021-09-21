namespace GameSettings
open GameParts
open System.IO

module Initial =
    let testBoard  =
        [|
        [|Some 3;None;None;Some 5;Some 7;None;Some 2;Some 4;None|]
        [|Some 5;Some 7;Some 2;None;Some 4;None;Some 9;None;None|]
        [|None;None;Some 4;None;Some 2;Some 6;Some 5;None;Some 3|]
        [|Some 7;None;Some 3;None;None;Some 2;None;Some 8;Some 1|]
        [|Some 4;Some 2;None;Some 1;None;Some 7;None;None;Some 9|]
        [|None;None;Some 6;None;Some 3;Some 4;Some 7;None;Some 5|]
        [|Some 6;Some 3;None;Some 2;None;Some 5;None;Some 9;None|]
        [|None;Some 4;None;Some 7;Some 6;Some 3;None;Some 5;None|]
        [|None;Some 8;Some 5;None;None;None;Some 6;Some 3;Some 7|]
        |] 
        |> array2D
        |> Board.Create

    testBoard
    |> Array2D.iteri(fun x y v ->
        printfn "%A %A %A" x y v
    )

    let private fileBoard =
        let sourceDir = __SOURCE_DIRECTORY__
        let data = 
            Directory.GetFiles(sourceDir,"board.txt") 
            |> Array.head
            |> File.ReadLines

        data
        |> Seq.map(
            fun line -> line.Split(",")
            >> fun row -> row |> Array.map (int >>Some)
        )
        |> array2D 
        |> Board.Create

    let Game = 
        {   Board = testBoard 
            State = StartGame
            Cursor = 0,1 
            EmptyCells = Set.empty
            DuplicateCells = Set.empty}