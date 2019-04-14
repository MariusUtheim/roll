module Displayer

open Attacks

let hline () = printfn "----------------------------------------"

let display record =
    let exclamationMarks = if record.AttackResult.AttackRoll = 20 then "!!"
                           elif record.AttackResult.AttackRoll = 1 then (if &&"1d5" = 1 then "??" else "?")
                           elif record.AttackResult.AttackRoll >= record.Attack.ThreatRange then "!"
                           else ""
    printfn "%-20s%3d%-2s %3d (= %s)" record.Attack.Name record.AttackToHit exclamationMarks
                                 record.AttackResult.DamageRoll.Sum record.AttackResult.DamageRoll.Description
    record.CritResult |> Option.iter (
        fun crit -> printfn "%-18s+ %3d   %3d (= %3s)" "!" record.CritToHit crit.DamageRoll.Sum crit.DamageRoll.Description
    )


let performAttacks attacks =
    List.iter (rollAttack >> display) attacks


let performAttacksAgainst ac attacks =
    let attackResults = List.map rollAttack attacks
    let mutable totalDamage = 0
    let mutable nHits = 0

    for result in attackResults do
        if result.AttackResult.AttackRoll = 1
         || (result.AttackToHit < ac && result.AttackResult.AttackRoll <> 20) then
            printfn "%-20s%2d  %s" result.Attack.Name result.AttackResult.AttackRoll
                                   (if result.AttackResult.AttackRoll > 1 then "Miss"
                                    elif &&"1d5" = 1 then "Miss??" else "Miss?")
        else
            totalDamage <- totalDamage + result.AttackResult.DamageRoll.Sum
            nHits <- nHits + 1

            if result.AttackResult.AttackRoll >= result.Attack.ThreatRange && result.CritToHit >= ac then
                printfn "%-20s%2d  Crit! %3d [%3d] (= %s)" result.Attack.Name result.AttackResult.DamageRoll.Sum
                                                          result.AttackResult.AttackRoll totalDamage
                                                          result.AttackResult.DamageRoll.Description
                let crit = Option.get result.CritResult
                totalDamage <- totalDamage + crit.DamageRoll.Sum
                printfn "%22d%4s %3d [%3d] (= %s)" crit.AttackRoll "!" crit.DamageRoll.Sum totalDamage crit.DamageRoll.Description
            else
                printfn "%-20s%2d  Hit   %3d [%3d] (= %s)" result.Attack.Name result.AttackResult.AttackRoll 
                                                          result.AttackResult.DamageRoll.Sum totalDamage
                                                          result.AttackResult.DamageRoll.Description


    hline()
    printfn "%-26s%3d" "Number of hits:" nHits
    printfn "%-26s%3d\n" "Total damage:" totalDamage