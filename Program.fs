module Juri.Internal.Main

[<EntryPoint>]
let main argv =
    CLI.run argv
    //printfn "juri repl (juri version 0.1.0)"
    //Repl.startRepl() |> ignore