module Attacks

type Attack = {
    Name : string;
    Bonus : int;
    Damage : RollCommand;
    ThreatRange : int;
    CritDamage : RollCommand;
}

type AttackRoll = {
    AttackRoll : int;
    DamageRoll : RollResult;
}


type AttackRecord = {
    Attack : Attack;
    AttackResult : AttackRoll;
    CritResult : AttackRoll option;
} with 
    member this.AttackToHit = this.AttackResult.AttackRoll + this.Attack.Bonus
    member this.CritToHit = match this.CritResult with
                            | None -> 0
                            | Some result -> result.AttackRoll + this.Attack.Bonus

let rollAttack attack = 
    let attackRoll = &&"1d20"
    { Attack = attack;
      AttackResult = { AttackRoll = attackRoll; DamageRoll = roll attack.Damage };
      CritResult = if attackRoll < attack.ThreatRange 
                   then None
                   else Some { AttackRoll = &&"1d20"; DamageRoll = roll attack.CritDamage }
    }

let display record =
    let exclamationMarks = if record.AttackResult.AttackRoll = 20 then "!!"
                           elif record.AttackResult.AttackRoll = 1 then (if &&"1d5" = 1 then "??" else "?")
                           elif record.AttackResult.AttackRoll >= record.Attack.ThreatRange then "!"
                           else ""
    printfn "%-20s%3d%-2s %3d (= %s)" record.Attack.Name record.AttackToHit exclamationMarks
                                 record.AttackResult.DamageRoll.Sum record.AttackResult.DamageRoll.Description
    record.CritResult |> Option.iter (
        fun crit -> printfn "%-18s+ %3d   %3d (= %s)" "!" record.CritToHit crit.DamageRoll.Sum crit.DamageRoll.Description
    )

let performAttacks attacks =
    List.iter (rollAttack >> display) attacks
    printfn ""

let performAttacksAgainst ac attacks =
    let attackResults = List.map rollAttack attacks
    let mutable totalDamage = 0
    let mutable nHits = 0

    for result in attackResults do
        if result.AttackResult.AttackRoll = 1
           || (result.AttackToHit < ac && result.AttackResult.AttackRoll <> 20) then
            printfn "%-20s%s" result.Attack.Name (if result.AttackResult.AttackRoll > 1 then "Miss"
                                                  elif &&"1d5" = 1 then "Miss??"
                                                  else "Miss?")
        else
            totalDamage <- totalDamage + result.AttackResult.DamageRoll.Sum
            nHits <- nHits + 1

            if result.AttackResult.AttackRoll >= result.Attack.ThreatRange && result.CritToHit >= ac then
                printfn "%-20sCrit! %3d (= %s) [%d]" result.Attack.Name result.AttackResult.DamageRoll.Sum
                                                     result.AttackResult.DamageRoll.Description totalDamage
                let crit = Option.get result.CritResult
                totalDamage <- totalDamage + crit.DamageRoll.Sum
                printfn "%-24s+ %3d (= %s) [%d]" "!" crit.DamageRoll.Sum crit.DamageRoll.Description totalDamage
            else
                printfn "%-20sHit   %3d (= %s) [%d]" result.Attack.Name 
                                                    result.AttackResult.DamageRoll.Sum result.AttackResult.DamageRoll.Description
                                                    totalDamage

    printfn "------------------------------"
    printfn "%-26s%3d" "Number of hits:" nHits
    printfn "%-26s%3d\n" "Total damage:" totalDamage

let fullAttack nAttacks attack =
    (attack.Bonus, 1)
    |> List.unfold (fun (bonus, n) -> match n with
                                      | n when n > nAttacks -> None
                                      | _ -> Some ( { attack with Name = sprintf "%s (%d)" attack.Name n; Bonus = bonus },
                                                    (bonus - 5, n + 1) ))

let powerAttack n attack = 
    { attack with Bonus = attack.Bonus - n; Damage = attack.Damage + Constant n }

let powerAttack2H n attack = 
    { attack with Bonus = attack.Bonus - n; Damage = attack.Damage + Constant (2 * n) }