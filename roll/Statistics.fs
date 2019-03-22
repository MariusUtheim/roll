module Statistics
open Attacks

let pD20 dc =
    if dc <= 2 then 0.95
    elif dc >= 20 then 0.05
    else (21. - float dc) / 20.

let rec mean cmd = 
    match cmd with
    | Dice (n, s) -> (float <| n * (s + 1)) / 2.
    | DiceDrop _ -> failwith "Calculating statistics on DiceDrop is not implemented"
    | Constant c -> float c
    | Sum (cmd1, cmd2) -> mean cmd1 + mean cmd2
    | Difference (cmd1, cmd2) -> mean cmd1 - mean cmd2
    | Multiplier (n, cmd) -> float n * mean cmd
    | Nothing -> 0.

let rec variance cmd =
    match cmd with 
    | Dice (n, s) -> float(n * (s * s - 1)) / 12.
    | DiceDrop _ -> failwith "Calculating statistics on DiceDrop is not implemented"
    | Constant _ -> 0.
    | Sum (cmd1, cmd2) | Difference (cmd1, cmd2) -> variance cmd1 + variance cmd2
    | Multiplier (n, cmd) -> float n * variance cmd
    | Nothing -> 0.

let rec maximum cmd =
    match cmd with
    | Dice (n, s) -> n * s
    | DiceDrop (n, s, d) -> (n - d) * s
    | Constant c -> c
    | Sum (cmd1, cmd2) -> maximum cmd1 + maximum cmd2
    | Difference (cmd1, cmd2) -> maximum cmd1 - minimum cmd2
    | Multiplier (n, cmd) -> n * maximum cmd
    | Nothing -> 0
and minimum cmd =
    match cmd with
    | Dice (n, s) -> n
    | DiceDrop (n, s, d) -> (n - d)
    | Constant c -> c
    | Sum (cmd1, cmd2) -> minimum cmd1 + minimum cmd2
    | Difference (cmd1, cmd2) -> minimum cmd1 - maximum cmd2
    | Multiplier (n, cmd) -> n * minimum cmd
    | Nothing -> 0
  
let std cmd = sqrt(variance cmd)

let private sqrt2 = 1.4142135623730951 

// From http://www.fssnip.net/bD/title/Statistical-functions
let private erfc x =
    if (System.Double.IsNegativeInfinity x) then 2.0
    elif (System.Double.IsPositiveInfinity x) then 0.0
    else
        let z = abs x
        let t = 1.0 / (1.0 + 0.5 * z)
        let res = t * exp (-z * z - 1.26551223 + t * (1.00002368 + t * (0.37409196 + t * (0.09678418 + t * (-0.18628806 + t * (0.27886807 + t * (-1.13520398 + t * (1.48851587 + t * (-0.82215223 + t * 0.17087277))))))))) 
        if (x >= 0.0) then res else 2.0 - res


let zValue dc cmd = (float dc - mean cmd) / std cmd

let pValue dc cmd = 
    let t = -zValue dc cmd
    1. - 0.5 * (2. - erfc (-t / sqrt2))


let meanDamageAgainst ac attack =
    let hitChance = pD20 (ac - attack.Bonus)

    let meanDamage = mean attack.Damage

    let critConfirmChance = 
        if attack.ThreatRange + attack.Bonus >= ac then pD20 attack.ThreatRange
        else hitChance

    let meanCritDamage = mean attack.CritDamage

    hitChance * (meanDamage + critConfirmChance * meanCritDamage)


