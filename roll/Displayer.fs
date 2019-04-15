module Displayer

open Attacks

let hline () = printfn "------------------------------------------------"

let exclamationMarks roll threat =
    match roll with 
    | 20 -> "!!"
    | 1 -> if &&"1d5" = 1 then "??" else "?"
    | roll when roll >= threat -> "!"
    | _ -> ""

let display record =
    printfn "%-20s%3d%-2s %3d       (= %s)" record.Attack.Name record.AttackToHit (exclamationMarks record.AttackResult.AttackRoll record.Attack.ThreatRange)
                                 record.AttackResult.DamageRoll.Sum record.AttackResult.DamageRoll.Description
    record.CritResult |> Option.iter (
        fun crit -> printfn "%-18s+ %3d%-2s %3d       (= %3s)" "!" record.CritToHit (exclamationMarks crit.AttackRoll record.Attack.ThreatRange)
                                                         crit.DamageRoll.Sum crit.DamageRoll.Description
    )


let performAttacks attacks =
    printfn "Attack             Roll   Dmg       DmgRolls"
    hline()
    List.iter (rollAttack >> display) attacks


let performAttacksAgainst ac attacks =

    printfn "Attack             Roll   Dmg CDmg  DmgRolls"
    hline()

    let attackResults = List.map rollAttack attacks
    let mutable totalDamage = 0
    let mutable nHits = 0
    
    for result in attackResults do
        if result.AttackResult.AttackRoll = 1
         || (result.AttackToHit < ac && result.AttackResult.AttackRoll <> 20) then
            // On miss
            printfn "%-20s%3d    --" result.Attack.Name result.AttackToHit
        else
            totalDamage <- totalDamage + result.AttackResult.DamageRoll.Sum
            nHits <- nHits + 1

            if result.AttackResult.AttackRoll >= result.Attack.ThreatRange && result.CritToHit >= ac then
                printfn "%-20s%3d%-2s %3d [%3d] (= %s)" result.Attack.Name result.AttackToHit 
                                                  (if result.AttackResult.AttackRoll = 20 then "!!" else "!")
                                                  result.AttackResult.DamageRoll.Sum totalDamage result.AttackResult.DamageRoll.Description

                let crit = Option.get result.CritResult
                totalDamage <- totalDamage + crit.DamageRoll.Sum
                printfn "%-18s+ %3d%-2s %3d [%3d] (= %3s)" "!" result.CritToHit (exclamationMarks crit.AttackRoll result.Attack.ThreatRange)
                                                                 crit.DamageRoll.Sum totalDamage crit.DamageRoll.Description
            else
                printfn "%-20s%3d   %3d [%3d] (= %s)" result.Attack.Name result.AttackToHit 
                                                      result.AttackResult.DamageRoll.Sum totalDamage result.AttackResult.DamageRoll.Description


    hline()
    printfn "%-26s%3d" "Number of hits:" nHits
    printfn "%-26s%3d\n" "Total damage:" totalDamage