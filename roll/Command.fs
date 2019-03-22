module Command


let private attacksCsv = 
    try System.IO.File.ReadAllText(".config")
    with :? System.IO.IOException -> "Attacks.csv"

let (|Either|_|) (this, that) str =
    if str = this || str = that then Some () else None

let (|RollCommand|_|) str = tryParseCommand str

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
    (n)x<cmd> - execute cmd and multiply the result by n
    -c - roll a character. Equivalent to 6x4d6d1
    -v <cmd> - execute the command verbosely, displaying all intermedediate rolls.
    -s <cmd> - display mean and standard deviation for the result cmd
    -s <cmd> -dc <dc> - show statistics for cmd and p-value of beating the specified DC
    -s <attacker> -ac <ac> - show statistics for the attacker against the specified AC
    -a <attacker> - make attack rolls and damage rolls for the specified attacker
    -a <attacker> -ac <ac> - make attack rolls and damage rolls for the specified attacker against the specified AC
    -a --list - Show a list of all attacker names
    -a --list <attacker> - Show a list of all attacks made by the specified attacker
    -a --file - Show the name of then file that is being used for attacks
    -a --file <file> - Set name of the file that should be used for attacks
    -a --new <attacker> <name> <bonus> <damage> <threatRange> <critDamage> - Create a new attack
    -o - Open the program folder
    -oa - Open the current csv file defining attackers
    "

let attackerNotFound attacker =
    failwith("Attacker " + attacker + " not found.")

let private getAllAttackers() = CsvLoader.loadAttacks attacksCsv

let private getAttacks attacker =
    let d = getAllAttackers()
    match d.TryGetValue attacker with 
    | (false, _) -> attackerNotFound attacker
    | (true, attacks) -> attacks

let updateAttackers = CsvLoader.writeAttackers attacksCsv

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
    printfn "------------------------------"
    printfn "%-20s%.2f\n" "Total mean damage:" totalMeanDmg


let performAttacks attacker =
    let attacks = (getAttacks attacker)
    printfn "%s attacking" attacker
    printfn "------------------------------"
    Attacks.performAttacks attacks

let performAttacksAgainst ac attacker = 
    let attacks = getAttacks attacker
    printfn "%s attacking vs. AC %d" attacker ac
    printfn "------------------------------"
    Attacks.performAttacksAgainst ac attacks

let performManyAttacksAgainst ac attacker repeat =
    let attacks = getAttacks attacker
    printfn "%dx %s attacking vs. AC %d" repeat attacker ac
    for _ in 1 .. repeat do
        printfn "------------------------------"
        Attacks.performAttacksAgainst ac attacks

let writeAttacksPath path =
    System.IO.File.WriteAllText(".config", path)

let listAttackers () =
    let attackers = getAllAttackers()
    attackers |> Map.iter (fun attacker _ -> printfn "%s" attacker )
        

let listAttacks attacker =
    let attacks = getAttacks attacker
    for attack in attacks do
        let bonusStr = if attack.Bonus > 0 then sprintf "+%d" attack.Bonus
                       else attack.Bonus.ToString()
        let threatRangeStr = if attack.ThreatRange = 20 then "20"
                             else sprintf "%d-20" attack.ThreatRange
        printfn "%-20s%s (%O), %s/+%O" attack.Name bonusStr attack.Damage threatRangeStr attack.CritDamage

let newAttacker attacker attackName bonus damage threatRange critDamage =
    let attackers = getAllAttackers()
    let newAttack = Attacks.create attackName bonus damage threatRange critDamage
    let newAttacks = match attackers.TryGetValue(attacker) with
                     | (true, attacks) -> attacks @ [ newAttack ]
                     | (false, _) -> [ newAttack ]
    attackers
    |> Map.add attacker newAttacks
    |> updateAttackers

let newUsage () =
    printfn "Usage of -a --new:\n<attacker> <attackName> <bonus> <damage> <threatRange> <critDamage>\n Example:\n  roll -a --new Orc Falchion 4 2d4+4 18 2d4+4"

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

let openText (path : string) =
    System.Diagnostics.Process.Start(path) |> ignore


let parse (argv : string list) =
    match argv with
    | [ ] -> printfn "%d" &&"1d20"
    | [ RollCommand cmd ] -> printfn "%d" <| rollSum cmd
    | [ Either("-c", "--character") ] -> for _ in 1 .. 6 do rollVerbose &"4d6d1" |> ignore
    | [ Either("-v", "--verbose"); RollCommand cmd ] -> rollVerbose cmd
    | [ Either("-s", "--stat"); RollCommand cmd ] -> statistics cmd
    | [ Either("-s", "--stat"); RollCommand cmd; "-dc"; Integer dc ] -> statisticsDC dc cmd
    | [ Either("-s", "--stat"); attacker; "-ac"; Integer ac ] -> statisticsAC ac attacker
    | [ "-af" ] | [ Either("-a", "--attack"); Either("-f", "--file") ] -> printfn "%s" attacksCsv
    | [ "-af"; path ] | [ Either("-a", "--attack"); Either("-f", "--file"); path ] -> writeAttacksPath path
    | [ Either("-a", "--attack"); "--list" ] -> listAttackers ()
    | [ Either("-a", "--attack"); "--list"; attacker ] -> listAttacks attacker
    | [ Either("-a", "--attack"); "--new"; attacker; attackName; Integer bonus; RollCommand damage; Integer threatRange; RollCommand critDamage ] -> newAttacker attacker attackName bonus damage threatRange critDamage
    | Either("-a", "--attack") :: "--new" :: _ -> newUsage()
    | [ Either("-a", "--attack"); "--remove"; attacker; attack ] -> removeAttack attacker attack 
    | [ Either("-a", "--attack"); "--removeAll"; attacker ] -> removeAttacker attacker
    | [ Either("-a", "--attack"); attacker ] -> performAttacks attacker
    | [ Either("-a", "--attack"); attacker; "-ac"; Integer ac ] -> performAttacksAgainst ac attacker
    | [ Either("-a", "--attack"); attacker;"-ac"; Integer ac; Either("-r", "--repeat"); Integer repetitions ] -> performManyAttacksAgainst ac attacker repetitions
    | [ "-o" ] -> openText "./" 
    | [ "-oa" ] | [ "-o"; Either("-a", "--attack") ] -> openText attacksCsv
    | _ -> usage()
