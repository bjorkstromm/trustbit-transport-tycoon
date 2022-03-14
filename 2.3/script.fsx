type Road = {
    Start : string
    End : string
}

let loadData file =
    $"{__SOURCE_DIRECTORY__}/{file}"
    |> System.IO.File.ReadAllLines
    |> Array.skip 1
    |> Array.choose (fun str ->
        match str.Split(',') with
        | [|_;_;a;b;speed|] ->
            let road = { Start = a; End = b }
            Some (road, speed |> float)
        | _ -> None)

let trainData = loadData "train_data.csv"
let averageSpeeds =
    trainData
    |> Array.groupBy fst
    |> Array.map (fun (key, arr) ->
        (key, arr |> Array.map snd |> Array.average))
    |> Map.ofArray

let predict road =
    match averageSpeeds |> Map.tryFind road with
    | Some speed -> speed
    | _ -> failwithf "No data for this road %A" road

let testData = loadData "test_data.csv"

let calcMSE mse (road, speed) =
    let prediction = predict road
    let diff = speed - prediction
    let squaredError = diff * diff
    mse + squaredError

let errorSum =
    testData
    |> Array.fold calcMSE 0.0
    |> (fun mse -> mse / (float)testData.Length)

printfn "MSE is %f" errorSum
