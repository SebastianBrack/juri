module Juri.Internal.Output


type OutputStream() =
    let mutable content = ""
    let mutable position = 0
    member this.Write(tail) =
        content <- content + tail
    member this.CanRead() =
        position < content.Length
    member this.ReadToEnd() =
        let out = content[position..]
        position <- content.Length - 1
        out
        
    
    
type InterpreterOutput() =
    let standard = OutputStream()
    let error = OutputStream()
    let metaInfo = OutputStream()
    member this.Standard = standard
    member this.Error = error
    member this.MetaInfo = metaInfo