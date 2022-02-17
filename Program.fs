module Juri.Internal

[<EntryPoint>]
let main argv =
    Internal.CLI.run argv
    //printfn "juri repl (juri version 0.1.0)"
    //Repl.startRepl() |> ignore