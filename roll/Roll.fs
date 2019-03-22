[<AutoOpen>]
module Roll

let private rng = System.Random()
let private dieRoll s = if s < 0 then 0 else 1 + rng.Next(s)


type RollCommand = 
    | Dice of int * int
    | DiceDrop of int * int * int
    | Constant of int
    | Sum of RollCommand * RollCommand
    | Difference of RollCommand * RollCommand
    | Multiplier of int * RollCommand
    | Nothing
    with 
        static member (+) (cmd1, cmd2) = Sum(cmd1, cmd2)
        override this.ToString() =
            match this with
            | Dice (n, s) -> sprintf "%dd%d" n s
            | DiceDrop (n, s, d) -> sprintf "%dd%dd%d" n s d
            | Constant c -> c.ToString()
            | Sum (cmd1, cmd2) -> sprintf "%s+%s" (cmd1.ToString()) (cmd2.ToString())
            | Difference (cmd1, cmd2) -> sprintf "%s-%s" (cmd1.ToString()) (cmd2.ToString())
            | Multiplier (n, cmd) -> sprintf "%dx%s" n (cmd.ToString())
            | Nothing -> ""


let private splice = List.map string >> List.reduce(sprintf "%s+%s")


type RollResult = {
    Command : RollCommand;
    Rolls : int list;
    } with
        member this.Sum = List.sum this.Rolls
        member this.Description = splice this.Rolls

let sum (result : RollResult) = result.Sum

let rec tryParseCommand str =
    match str with
    | Regex "(.+)\\+(.+)" [ left; right ] -> match (tryParseCommand left, tryParseCommand right) with
                                             | (Some left, Some right) -> Some <| Sum(left, right)
                                             | _ -> None
    | Regex "(.+)-(.+)" [ left; right ] -> match (tryParseCommand left, tryParseCommand right) with
                                           | (Some left, Some right) -> Some <| Difference(left, right)
                                           | _ -> None
    | Regex "(\d+)x(.+)" [ Integer n; cmd ] -> match tryParseCommand cmd with 
                                               | Some cmd -> Some <| Multiplier(n, cmd)
                                               | _ -> None
    | Regex "(\d+)d(\d+)d(\d+)" [ Integer n; Integer s; Integer d ] -> Some <| DiceDrop(n, s, d)
    | Regex "(\d+)d(\d+)" [ Integer n; Integer s ] -> Some <| Dice(n, s)
    | Regex "(\d+)" [ Integer n ] -> Some <| Constant n
    | _ -> None

let parseCommand str = 
    match tryParseCommand str with
    | Some rollCommand -> rollCommand
    | None -> failwith "Unknown command"

let rec writeCommand (cmd : RollCommand) = cmd.ToString()

let rec roll cmd =
    { Command = cmd;
      Rolls = 
        match cmd with
        | Dice (n, s) -> List.init n (fun _ -> dieRoll s)
        | DiceDrop (n, s, d) -> List.init n (fun _ -> dieRoll s)
                                |> List.sortDescending
                                |> List.take (n - d)
        | Constant c -> [ c ]
        | Sum (cmd1, cmd2) -> (roll cmd1).Rolls @ (roll cmd2).Rolls
        | Difference (cmd1, cmd2) -> (roll cmd1).Rolls @ (List.map (~-) (roll cmd2).Rolls)
        | Multiplier (m, cmd) -> (roll cmd).Rolls
                                 |> List.map (( *) m)
        | Nothing -> []
    }

let rec rollVerbose cmd = 
    if cmd <> Nothing then
        let result = roll cmd
        let verboseString =
            match cmd with
            | Dice _ -> result.Description
            | DiceDrop (n, _, _) -> "Verbose dice drops are not yet implemented"
            | Constant c -> string c
            | Sum _ | Difference _ -> failwith "Verbose sums are not yet implemented"
            | Multiplier _ -> failwith "Verbose multiplications are not yet implemented"
            | Nothing -> ""

        printfn "%-4d= %s" result.Sum result.Description


let rollSum cmd = roll cmd |> sum

let (~&) str = parseCommand str
let (~&&) str = (roll &str).Sum

