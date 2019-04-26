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
        member this.RollCount =
            match this with
            | Dice (n, _) | DiceDrop (n, _, _) -> n
            | Constant _ | Nothing -> 0
            | Sum (cmd1, cmd2) | Difference (cmd1, cmd2) -> cmd1.RollCount + cmd2.RollCount
            | Multiplier (_, cmd) -> cmd.RollCount

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
        member this.Sum = 
            match this.Command with 
            | Dice _ -> List.sum this.Rolls
            | Constant c -> c
            | DiceDrop (n, _, d) -> this.Rolls |> List.take (n - d) |> List.sum
            | Sum (cmd1, cmd2) -> let left, right = this.Rolls |> List.splitAt (cmd1.RollCount) 
                                  { Command = cmd1; Rolls = left }.Sum + { Command = cmd2; Rolls = right }.Sum
            | Difference (cmd1, cmd2) -> let left, right = this.Rolls |> List.splitAt (cmd1.RollCount) 
                                         { Command = cmd1; Rolls = left }.Sum - { Command = cmd2; Rolls = right }.Sum
            | Multiplier (m, cmd1) -> m * { Command = cmd1; Rolls = this.Rolls }.Sum
            | Nothing -> 0

        member this.Description =
            match this.Command with 
            | Dice _ -> splice this.Rolls
            | Constant c -> string c
            | DiceDrop (n, s, d) -> let left, right = this.Rolls |> List.splitAt (n - d) 
                                    sprintf "(%s // %s)" (splice left) (splice right)
            | Sum (cmd1, cmd2) -> let left, right = this.Rolls |> List.splitAt (cmd1.RollCount)
                                  sprintf "%s + %s" { Command = cmd1; Rolls = left }.Description 
                                                    { Command = cmd2; Rolls = right }.Description
            | Difference (cmd1, cmd2) -> let left, right = this.Rolls |> List.splitAt (cmd1.RollCount)
                                         sprintf "%s - (%s)" { Command = cmd1; Rolls = left }.Description 
                                                             { Command = cmd2; Rolls = right }.Description
            | Multiplier (m, cmd) -> sprintf "%dx(%s)" m { Command = cmd; Rolls = this.Rolls}.Description
            | Nothing -> "-"

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
        | Constant _ | Nothing -> []
        | Sum (cmd1, cmd2) | Difference (cmd1, cmd2) -> (roll cmd1).Rolls @ (roll cmd2).Rolls
        | Multiplier (m, cmd) -> (roll cmd).Rolls
    }

let rec rollVerbose cmd = 
    if cmd <> Nothing then
        let result = roll cmd
        printfn "%-4d= %s" result.Sum result.Description


let rollSum cmd = roll cmd |> sum

let (~&) str = parseCommand str
let (~&&) str = (roll &str).Sum

