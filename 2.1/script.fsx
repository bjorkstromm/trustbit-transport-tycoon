type Milestone = {
    location : string
    traveled : int
    previous : Milestone option
}

let distances =
    $"{__SOURCE_DIRECTORY__}/data.csv"
    |> System.IO.File.ReadAllLines
    |> Array.skip 1
    |> Array.choose (fun str ->
        match str.Split(',') with
        | [|a;b;km|] -> Some [|(a, (b, km |> int)); (b, (a, km |> int))|]
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
        | None -> [milestone.location]
        | Some previous ->
            milestone.location :: loop previous

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

                let visitedDistance =
                    match visited.TryGetValue(next) with
                    | (true, milestone) -> milestone.traveled
                    | _ -> System.Int32.MaxValue

                if traveled < visitedDistance then
                    visited.[next] <- { location = next; traveled = traveled; previous = Some(previous) }
                    queue.Enqueue(next, traveled))
            loop ()

    loop ()

    visited.[end']
    |> milestones

let printRoute milestone =
    milestone
    |> List.reduce (fun a b -> $"{a},{b}") 
    |> printfn "%s"

// Run 'dotnet fsi 2.1/script.fsx "Steamdrift" "Leverstorm"'
match fsi.CommandLineArgs with
| [|_;start;end'|] ->
    route start end'
    |> printRoute
| _ -> printfn "Invalid args: %A" fsi.CommandLineArgs

// route "Steamdrift" "Leverstorm"