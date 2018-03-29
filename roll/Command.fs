module Command


let private rng = System.Random()
let private roll n _ = if n < 0 then 0 else 1 + rng.Next(n)

type Command =
    | Roll of int * int
    | RollDrop of int * int * int
    | Constant of int
    | Sum of Command * Command
    | Verbose of Command
    | Multiple of Command list
    | Multiplier of int * Command
    | Nothing
    


let rec parseCommand str =
    match str with
    | Regex "(\d+)x(.+)" [ Integer n; cmd ] -> Multiple <| List.init n (fun _ -> parseCommand cmd)
    | Regex "(\d+)\*(.+)" [ Integer n; cmd ] -> Multiplier (n, parseCommand cmd)
    | Regex "!([^!]+)" [ cmd ] -> Verbose(parseCommand cmd)
    | Regex "(.+)\\+(.+)" [ left; right ] -> Sum(parseCommand left, parseCommand right)
    | Regex "(\d+)d(\d+)d(\d+)" [ Integer n; Integer s; Integer d ] -> RollDrop(n, s, d)
    | Regex "(\d+)d(\d+)" [ Integer n; Integer s ] -> Roll(n, s)
    | Regex "(\d+)" [ Integer n ] -> Constant n
    | _ -> printfn "Unknown command"; Nothing

    
let private splice = List.map string >> List.reduce(sprintf "%s+%s")

let rec executeVerbose = function
    | Roll (n, s) -> let rolls = List.init n (fun _ -> 1 + rng.Next(s))
                     printfn "%-4d (%s)" <| List.sum rolls <| splice rolls 
    | RollDrop (n, s, d) -> let rolls = List.init n (fun _ -> 1 + rng.Next(s)) |> List.sortDescending
                            let taken, dropped = List.splitAt (n - d) rolls
                            printfn "%-4d (%s // %s)" <| List.sum taken <| splice taken <| splice dropped
    | Constant n -> printfn "%d" n
    | Multiple s -> List.iter executeVerbose s
    | Sum _ | Verbose _ | Multiplier _ -> printfn "Unimplemented command"
    | Nothing -> ()


let rec calculate = function
    | Roll (n, s) -> List.init n (roll s) |> List.sum
    | RollDrop (n, s, d) -> List.init n (roll s) |> List.sort |> List.skip d |> List.sum
    | Constant n -> n
    | Sum (left, right) -> calculate left + calculate right
    | Multiple s -> List.sumBy calculate s
    | Multiplier (n, s) -> n * calculate s
    | Verbose cmd -> executeVerbose cmd; 0
    | Nothing -> 0
   

