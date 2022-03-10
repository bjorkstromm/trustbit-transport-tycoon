type Milestone = {
    location : string
    traveled : float
    previous : Milestone option
}

let distances =
    $"{__SOURCE_DIRECTORY__}/data.csv"
    |> System.IO.File.ReadAllLines
    |> Array.skip 1
    |> Array.choose (fun str ->
        match str.Split(',') with
        | [|a;b;km;speed|] ->
            let traveltime = (km |> float) / (speed |> float)
            Some [|(a, (b, traveltime)); (b, (a, traveltime))|]
        | _ -> None)
    |> Array.concat
    |> Array.groupBy fst
    |> Array.map (fun (key, arr) ->
        (key, arr |> Array.map snd |> Array.distinct))
    |> Map.ofArray

let adjacent location =
    distances.[location]

let milestones milestone =
    let rec loop milestone =
        match milestone.previous with
        | None -> [(milestone.location, milestone.traveled)]
        | Some previous ->
            (milestone.location, milestone.traveled) :: loop previous

    loop milestone
    |> List.rev


let route start end' =
    let queue = System.Collections.Generic.PriorityQueue<string, int>()
    let visited = System.Collections.Generic.Dictionary<string, Milestone>()

    visited.[start] <- { location = start; traveled = 0; previous = None }
    queue.Enqueue (start, 0)

    let rec loop () =
        if queue.Count = 0 then ()
        else
            let current = queue.Dequeue()

            current
            |> adjacent
            |> Array.iter (fun (next, distance) ->
                let previous = visited.[current]
                let traveled = previous.traveled + distance

                let visitedTime =
                    match visited.TryGetValue(next) with
                    | (true, milestone) -> milestone.traveled
                    | _ -> System.Int32.MaxValue

                if traveled < visitedTime then
                    visited.[next] <- { location = next; traveled = traveled; previous = Some(previous) }
                    queue.Enqueue(next, (traveled * 100.0) |> ceil |> int))
            loop ()

    loop ()

    visited.[end']
    |> milestones

let printRoute milestone =
    milestone
    |> List.iteri (fun i (location, traveled)->
        let desc = if i = 0 then "DEPART" else "ARRIVE"
        printfn "%.2fh %s %s" traveled desc location) 

// Run 'dotnet fsi 2.1/script.fsx "Steamdrift" "Leverstorm"'
match fsi.CommandLineArgs with
| [|_;start;end'|] ->
    route start end'
    |> printRoute
| _ -> printfn "Invalid args: %A" fsi.CommandLineArgs

// route "Steamdrift" "Leverstorm"