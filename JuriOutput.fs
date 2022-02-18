module Juri.Internal.Output


type OutputStream() =
    let mutable content = ""
    let mutable position = 0
    member this.write(tail) =
        content <- content + tail
    member this.canRead() =
        position < content.Length
    member this.readToEnd() =
        let out = content[position..]
        position <- content.Length - 1
        out
        
    
    
type InterpreterOutput() =
    let standard = OutputStream()
    let error = OutputStream()
    let metaInfo = OutputStream()