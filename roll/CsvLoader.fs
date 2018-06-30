module CsvLoader
open System.Collections.Generic
open System.IO
open Attacks


type AttackerCsvRecord = { 
    AttackerName : string; 
    AttackName : string;
    Bonus : int;
    Damage : string;
    ThreatRange : int;
    CritDamage : string;
}


let writeAttackers (attackers : IDictionary<string, Attack list>) (csvFile : string) = 

    let records =
        attackers
        |> Seq.collect(fun kv -> [ for attack in kv.Value ->
                                      { AttackerName = kv.Key; AttackName = attack.Name; Bonus = attack.Bonus;
                                        Damage = attack.Damage.ToString(); ThreatRange = attack.ThreatRange; 
                                        CritDamage = attack.CritDamage.ToString() } ])

    use stream = new CsvHelper.CsvWriter(new StreamWriter(csvFile))
    stream.WriteHeader<AttackerCsvRecord>()
    stream.NextRecord()

    for record in records do
        stream.WriteRecord(record)
        stream.NextRecord()

let loadAttacks (csvFile : string) =
    use stream = new CsvHelper.CsvReader(new StreamReader(csvFile))
    stream.Configuration.PrepareHeaderForMatch <- fun h -> System.Globalization.CultureInfo.CurrentCulture.TextInfo.ToTitleCase(h)
    let records = stream.GetRecords<AttackerCsvRecord>()

    records
    |> Seq.groupBy(fun r -> r.AttackerName)
    |> Seq.map(fun (key, records) -> let attacks = [ for record in records -> 
                                                     { Name = record.AttackName; Bonus = record.Bonus; 
                                                       Damage = &record.Damage; ThreatRange = record.ThreatRange;
                                                       CritDamage = &record.CritDamage } ]
                                     (key, attacks))
    |> dict

