[<AutoOpen>]
module Util

let (|Integer|_|) str =
    match System.Int32.TryParse(str) with
    | (true, value) -> Some value
    | (false, _) -> None


let (|Regex|_|) pattern str =
    let regex = System.Text.RegularExpressions.Regex("^" + pattern + "$")
    let m = regex.Match(str)
    if not m.Success then None
    else Some (List.tail [ for g in m.Groups -> g.Value ])

