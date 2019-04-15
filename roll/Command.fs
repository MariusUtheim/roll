module Command

//----------------------------------------------------------------------

let private attacksCsv = 
    try System.IO.File.ReadAllText("roll.config")
    with :? System.IO.IOException -> 
        printfn "Could not find configuration. Generating config file..."
        try System.IO.File.WriteAllText("roll.config", "Attacks.csv")
        with :? System.IO.IOException -> printfn "Warning: Could not generate configuration file."
        "Attacks.csv"

let (|Either|_|) (this, that) str =
    if str = this || str = that then Some () else None

let (|RollCommand|_|) str = tryParseCommand str

let (|VerboseRollCommand|_|) (str : string) =
    if str.[0] = '!' then tryParseCommand (str.Substring 1)
    else None

let (|String|_|) (str : string) = Some str


let attackerNotFound attacker =
    failwith("Attacker " + attacker + " not found.")

let private getAllAttackers() =
    try CsvLoader.loadAttacks attacksCsv
    with :? System.IO.IOException ->
        failwith("Could not open attacks file " + attacksCsv)


let private getAttacks attacker =
    let d = getAllAttackers()
    match d.TryGetValue attacker with 
    | (false, _) -> attackerNotFound attacker
    | (true, attacks) -> attacks


//----------------------------------------------------------------------

// Statistics

let statistics cmd =
    printfn "Mean: %.3f\nStd.: %.3f" (Statistics.mean cmd) (Statistics.std cmd)

let statisticsDC dc cmd =
    printfn "Mean: %.3f\nStd.: %.3f\npVal: %.3f" (Statistics.mean cmd) (Statistics.std cmd) (Statistics.pValue dc cmd)

let statisticsAC ac attacker =
    let attacks = (getAttacks attacker)
    let mutable totalMeanDmg = 0.
    for attack in attacks do
        let mean = Statistics.meanDamageAgainst ac attack
        totalMeanDmg <- totalMeanDmg + mean
        printfn "%-20s%-3.2f" attack.Name mean
    Displayer.hline()
    printfn "%-20s%.2f\n" "Total mean damage:" totalMeanDmg

//----------------------------------------------------------------------

// File management and listing

let openText (path : string) =
    System.Diagnostics.Process.Start(path) |> ignore

let openFolder () = openText "./"

let openAttackers () = openText attacksCsv

let getConfig config =
    try printfn "%s" <| System.IO.File.ReadAllText("roll.config")
    with :? System.IO.IOException ->
        failwith "Could not read roll.config"

let setConfig config = System.IO.File.WriteAllText("roll.config", config)

let listAttackers () =
    let attackers = getAllAttackers()
    attackers |> Map.iter (fun attacker _ -> printfn "%s" attacker )

let listAttacker attacker =
    let attacks = getAttacks attacker
    for attack in attacks do
        let bonusStr = if attack.Bonus > 0 then sprintf "+%d" attack.Bonus
                       else attack.Bonus.ToString()
        let threatRangeStr = if attack.ThreatRange = 20 then "20"
                             else sprintf "%d-20" attack.ThreatRange
        printfn "%-20s%s (%O), %s/+%O" attack.Name bonusStr attack.Damage threatRangeStr attack.CritDamage

let updateAttackers = CsvLoader.writeAttackers attacksCsv


let newAttacker attacker attackName bonus damage threatRange critDamage =
    let attackers = getAllAttackers()
    let newAttack = Attacks.create attackName bonus damage threatRange critDamage
    let newAttacks = match attackers.TryGetValue(attacker) with
                     | (true, attacks) -> attacks @ [ newAttack ]
                     | (false, _) -> [ newAttack ]
    attackers
    |> Map.add attacker newAttacks
    |> updateAttackers

let removeAttack attacker attack =
    let attackers = getAllAttackers()
    match attackers.TryGetValue(attacker) with 
    | (false, _) -> attackerNotFound attacker
    | (true, attacks) -> let newAttacks = attacks |> List.filter (fun a -> a.Name <> attack)
                         if newAttacks = [] then 
                             attackers |> Map.remove attacker |> updateAttackers
                         else
                             attackers |> Map.add attacker newAttacks |> updateAttackers

let removeAttacker attacker =
    let attackers = getAllAttackers()
    match attackers.TryGetValue attacker with
    | (false, _) -> attackerNotFound attacker
    | (true, attacks) -> attackers |> Map.remove attacker|> updateAttackers


//----------------------------------------------------------------------

// Attacks

let performAttacks attacker =
    let attacks = (getAttacks attacker)
    printfn "%s attacking" attacker
    Displayer.hline()
    Displayer.performAttacks attacks

let performManyAttacks attacker repetitions =
    match repetitions with
    | r when r <= 0 -> printfn "number of repetitions must be positive"
    | r when r >= 100 -> printfn "number of repetitions must be less than 100"
    | 1 -> performAttacks attacker 
    | r -> let attacks = getAttacks attacker
           printfn "%s attacking %d times" attacker repetitions
           Displayer.hline()
           match attacks with
           | [] -> ()
           | [ attack ] -> Attacks.repeated repetitions attack |> Displayer.performAttacks
           | attacks -> for i in 1 .. repetitions do
                            printfn "%2d: " i
                            Displayer.performAttacks attacks
                            printfn ""

let performAttacksAgainst ac attacker = 
    let attacks = getAttacks attacker
    printfn "%s attacking vs. AC %d" attacker ac
    Displayer.hline()
    Displayer.performAttacksAgainst ac attacks

let performManyAttacksAgainst ac attacker repetitions =
    match repetitions with
    | r when r <= 0 -> printfn "number of repetitions must be positive"
    | r when r >= 100 -> printfn "number of repetitions must be less than 100"
    | 1 -> performAttacksAgainst ac attacker
    | r -> let attacks = getAttacks attacker
           printfn "%dx %s attacking vs. AC %d" repetitions attacker ac
           Displayer.hline()
           match attacks with
           | [] -> ()
           | [ attack ] -> Attacks.repeated repetitions attack |> Displayer.performAttacksAgainst ac
           | attacks -> for i in 1 .. repetitions do
                            printfn "%2d: " i
                            Displayer.performAttacksAgainst ac attacks
                            printfn ""

   

let usage () = 
    printfn  <| "Usage:
    roll - roll 1d20
    roll <cmd> - roll the specified command
    roll -c - roll ability scores for a new character
    roll -s [<options>] - show statistics for command or attacker
    roll -f [<options>] - configure files and attackers
    roll -a [<options>] - perform attacks by an attacker"

let rollUsage () =
    printfn <| "Command specification:
    <cmd1>+<cmd2> - sum the results of cmd1 and cmd2
    (n)d(s) - roll n s-sided dice
    (k) - return the constant number k, equivalent to kd1
    (n)d(s)d(d) - roll n s-sided dice, then drop the d smallest ones
    (n)x<cmd> - execute cmd and multiply the result by n
    !<cmd> - roll the command verbosely, displaying all intermediate results.
    "

let rec help cmd =
    match cmd with 
    | "-c" | "--character" -> printfn "Roll ability scores for a new character, rolling 4d6d1 six times"
    | "-s" | "--statistics" -> 
        printfn "%s" <| "Display statistics: \n"
                      + "\t-s <cmd> [-dc <dc] - show statistics for the specified command, against DC if specified\n"
                      + "\t-s <attacker> [-ac <ac>]- show statistics for the specified attacker, against AC if specified"

    | "-fo" -> printfn "\t-fo - open the folder containing configuration files"
    | "-fn" -> printfn "\t-fn <attacker> <attackName> <bonus> <damage> <threatRange> <critDamage> - create a new entry in the attackers list"
    | "-fc" -> printfn "\t-fc [<config>] - get or set the current configuration"
    | "-fa" -> printfn "\t-fa - open file containing attackers configuration"
    | "-fl" -> printfn "\t-fl [<attacker>] - list attackers, or attacks of the specified attacker"
    | "-f" ->
        printfn "File control: "
        help "-fo"
        printfn "\t-fn <specs> - create a new attacker (write -h -fn for more details)"
        help "-fc"
        help "-fa"
        help "-fl"

    | "-a" -> printfn "%s" <| "\t-a [<repetitions>] <attacker> [-ac <ac>] - \n"
                            + "\t\tPerform attacks for the specified attacker.\n"
                            + "\t\tIf repetitions is specified, the attack is repeated that many times.\n"
                            + "\t\tIf AC is specified, the number of hits and total damage will be displayed."

    | _ -> usage()

let parse (argv : string list) =
    match argv with
    | [ ] -> printfn "%d" &&"1d20"
    | [ RollCommand cmd ] -> printfn "%d" <| rollSum cmd
    | [ VerboseRollCommand cmd ] -> rollVerbose cmd

    | [ Either("-h", "--help") ] -> usage ()
    | [ Either("-h", "--help"); "-cmd" ] -> rollUsage()
    | [ Either("-h", "--help"); mode ] -> help mode

    | [ Either("-c", "--character") ] -> for _ in 1 .. 6 do rollVerbose &"4d6d1" |> ignore

    // get statistics for specified roll
    // get statistics when rolling against specified DC
    // get statistics for attacker when rolling against specified AC
    | [ Either("-s", "--stat"); RollCommand cmd ] -> statistics cmd
    | [ Either("-s", "--stat"); RollCommand cmd; "-dc"; Integer dc ] -> statisticsDC dc cmd
    | [ Either("-s", "--stat"); attacker; "-ac"; Integer ac ] -> statisticsAC ac attacker


    // open folder
    // open file
    // set config
    // list attackers
    // display specific attacker
    // new attack for attacker
    // delete attacker
    // delete specific attack of attacker
    | [ "-f" ] -> help "-f"
    | [ "-fo" ] | ["-f"; Either("-f", "--open") ] -> openFolder() 
    | [ "-fc" ] | ["-f"; Either("-c", "--config") ] -> getConfig ()
    | [ "-fc"; config ] | [ "-f"; Either("-c", "--config"); config ] -> setConfig config
    | [ "-fa" ] | [ "-f"; "--attacks" ] -> openAttackers()
    | [ "-fl" ] | [ "-f"; Either("-l", "--list") ] -> listAttackers()
    | [ "-fl"; attacker ] | [ "-f"; Either("-l", "--list"); attacker ] -> listAttacker attacker
    | [ "-f"; "-n"; attacker; attackName; Integer bonus; RollCommand damage; Integer threatRange; RollCommand critDamage ]
    | [ "-f"; "-new"; attacker; attackName; Integer bonus; RollCommand damage; Integer threatRange; RollCommand critDamage ]
    | [ "-fn"; attacker; attackName; Integer bonus; RollCommand damage; Integer threatRange; RollCommand critDamage ]
        -> newAttacker attacker attackName bonus damage threatRange critDamage
    | "-fn" :: _ | "-f" :: Either("-n", "--new") :: _ 
        -> help "-n"
    | [ "-fr"; attacker; ] | [ "-f"; Either("-r", "--remove"); attacker ] 
        -> removeAttacker attacker
    | [ "-fr"; attacker; attack ] 
    | [ "-f"; Either("-r", "--remove"); attacker; attack ] 
        -> removeAttack attacker attack 


    // roll attacks for attacker
    // roll attacks for attacker against specific ac
    // roll multiple attacks for attacker
    // roll multiple attacks for attacker against specific ac
    | [ Either("-a", "--attack"); attacker ] 
        -> performAttacks attacker
    | [ Either("-a", "--attack"); attacker; "-ac"; Integer ac ]
        -> performAttacksAgainst ac attacker
    | [ Either("-a", "--attack"); Integer repetitions; attacker ]
    | [ Either("-a", "--attack"); attacker; Either("-r", "--repeat"); Integer repetitions ]
        -> performManyAttacks attacker repetitions
    | [ Either("-a", "--attack"); attacker; "-ac"; Integer ac; Either("-r", "--repeat"); Integer repetitions ]
    | [ Either("-a", "--attack"); Integer repetitions; attacker; "-ac"; Integer ac ]
        -> performManyAttacksAgainst ac attacker repetitions

    | _ -> usage()
