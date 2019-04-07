module Attacks

type Attack = {
    Name : string;
    Bonus : int;
    Damage : RollCommand;
    ThreatRange : int;
    CritDamage : RollCommand;
}

let create name bonus damage threatRange critDamage = 
    { Name = name; Bonus = bonus; Damage = damage; ThreatRange = threatRange; CritDamage = critDamage }

let repeated nRepetitions attack = 
    List.init nRepetitions (fun n -> { attack with Name = sprintf "%d: %s" n attack.Name })


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


