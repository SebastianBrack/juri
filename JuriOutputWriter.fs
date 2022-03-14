module Juri.Internal.OutputWriter


open Output
open System


type IOutputWriter =
    abstract member WriteSTD: string -> unit
    abstract member WriteERR: string -> unit
    abstract member WriteMET: string -> unit
    

type ConsoleWriter() =
    interface IOutputWriter with
        member this.WriteSTD(msg) =
            Console.ResetColor()
            printf $"juri > {msg}"
        member this.WriteERR(msg) =
            Console.ForegroundColor <- ConsoleColor.Red
            printf $"juri > {msg}"
            Console.ResetColor()
        member this.WriteMET(msg) =
            Console.ForegroundColor <- ConsoleColor.DarkCyan
            printf $"juri > {msg}"
            Console.ResetColor()
            
            
type StreamWriter(streams: InterpreterOutputStreams) =
    let streams = streams
    interface IOutputWriter with
        member this.WriteSTD(msg) = streams.Standard.Write(msg)
        member this.WriteERR(msg) = streams.Error.Write(msg)
        member this.WriteMET(msg) = streams.MetaInfo.Write(msg)
