module Main
open Command

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
    (m)x<cmd> - execute cmd m times and sum the results
    !<cmd> - execute the command verbosely, showing intermediate results
    -c, --character - roll a character. Equivalent to 6x4d6d1
    -v, --verbose <cmd> - execute the command verbosely. Equivalent to !cmd
    "


[<EntryPoint>]
let main argv = 
    if argv.Length = 0 then Roll (1, 20) |> calculate |> printfn "%d"
    else match argv.[0] with
         | "-c" | "--character" -> for _ in 1 .. 6 do calculate (Verbose <| RollDrop(4, 6, 1)) |> ignore
         | "-v" | "--verbose" -> let cmd = parseCommand argv.[1]
                                 executeVerbose cmd
         | cmd -> let cmd = parseCommand argv.[0]
                  let result = calculate cmd
                  if result <> 0 then printfn "%d" <| calculate cmd

    #if DEBUG
    System.Console.ReadKey() |> ignore
    #endif
    0
