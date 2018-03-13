module Main

let rng = System.Random()
let roll n _ = if n < 0 then 0 else 1 + rng.Next(n)

let (|Integer|_|) str =
    match System.Int32.TryParse(str) with
    | (true, value) -> Some value
    | (false, _) -> None

let (|Regex|_|) pattern str =
    let regex = System.Text.RegularExpressions.Regex("^" + pattern + "$")
    let m = regex.Match(str)
    if not m.Success then None
    else Some (List.tail [ for g in m.Groups -> g.Value ])

type Command =
    | Roll of int * int
    | RollDrop of int * int * int
    | Constant of int
    | Sum of Command * Command
    | Verbose of Command
    | Multiple of Command list
    | Nothing
    

let usage () = 
    printfn  <| "Usage:
    roll <cmd>
    roll -c
    roll -v cmd

Command specification
    <cmd1>+<cmd2> - sum the results of cmd1 and cmd2
    (n)d(s) - roll n s-sided dice
    (k) - return the constant number k, equivalent to kd1
    (n)d(s)d(d) - roll n s-sided dice, then drop the d smallest ones
    (m)x<cmd> - execute cmd m times and sum the results
    !<cmd> - execute the command verbosely, showing intermediate results
    -c, --character - roll a character. Equivalent to 6x4d6d1
    -v, --verbose <cmd> - execute the command verbosely. Equivalent to !cmd
    "


let rec parseCommand str =
    match str with
    | Regex "(\d+)x(.+)" [ Integer n; cmd ] -> Multiple <| List.init n (fun _ -> parseCommand cmd)
    | Regex "!([^!]+)" [ cmd ] -> Verbose(parseCommand cmd)
    | Regex "(.+)\\+(.+)" [ left; right ] -> Sum(parseCommand left, parseCommand right)
    | Regex "(\d+)d(\d+)d(\d+)" [ Integer n; Integer s; Integer d ] -> RollDrop(n, s, d)
    | Regex "(\d+)d(\d+)" [ Integer n; Integer s ] -> Roll(n, s)
    | Regex "(\d+)" [ Integer n ] -> Constant n
    | _ -> printfn "Unknown command"; Nothing

    
let private splice = List.map string >> List.reduce(sprintf "%s + %s")
let rec executeVerbose = function
    | Roll (n, s) -> let rolls = List.init n (fun _ -> 1 + rng.Next(s))
                     printfn "%-4d (%s)" <| List.sum rolls <| splice rolls 
    | RollDrop (n, s, d) -> let rolls = List.init n (fun _ -> 1 + rng.Next(s)) |> List.sortDescending
                            let taken, dropped = List.splitAt (n - d) rolls
                            printfn "%-4d (%s // %s)" <| List.sum taken <| splice taken <| splice dropped
    | Constant n -> printfn "%d" n
    | Multiple s -> List.iter executeVerbose s
    | Sum _ | Verbose _ | Nothing -> printfn "Unimplemented command"


let rec calculate = function
    | Roll (n, s) -> List.init n (roll s) |> List.sum
    | RollDrop (n, s, d) -> List.init n (roll s) |> List.sort |> List.skip d |> List.sum
    | Constant n -> n
    | Sum (left, right) -> calculate left + calculate right
    | Multiple s -> List.sumBy calculate s
    | Verbose cmd -> executeVerbose cmd; 0
    | Nothing -> 0
   

[<EntryPoint>]
let main argv = 
    if argv.Length = 0 then Roll (1, 20) |> calculate |> printfn "%d"
    else match argv.[0] with
         | "-c" | "--character" -> for _ in 1 .. 6 do calculate (Verbose <| RollDrop(4, 6, 1)) |> ignore
         | "-v" | "--verbose" -> let cmd = parseCommand argv.[1]
                                 executeVerbose cmd
         | cmd -> let cmd = parseCommand argv.[0]
                  let result = calculate cmd
                  if result <> 0 then printfn "%d" <| calculate cmd

    #if DEBUG
    System.Console.ReadKey() |> ignore
    #endif
    0
