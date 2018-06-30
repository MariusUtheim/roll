module Main
open Command


[<EntryPoint>]
let main argv = 
    try Command.parse (List.ofArray argv) with
    | exn -> printfn "%s" exn.Message
    0
