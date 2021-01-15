module Rules 
    open Cell
    
    let private isUnique arr =
        arr |> Array.distinct |> Array.length = (arr |> Array.length)

    let private noEmpty arr =
        arr |> Array.contains Empty |> not

    let Correct arr =
        isUnique arr && noEmpty arr