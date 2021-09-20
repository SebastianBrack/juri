// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open Repl


[<EntryPoint>]
let main argv =
    printfn "juri repl (juri version 0.1.0)"
    Repl.startRepl() |> ignore
    0