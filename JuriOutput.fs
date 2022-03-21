module Juri.Internal.Output



type OutputStream<'T>() =
    let mutable content : 'T ResizeArray = ResizeArray()
    let mutable position = 0
    member this.Write(elem) =
        content.Add(elem)
    member this.CanRead() =
        position < content.Count
    member this.ReadToEnd() =
        let out = [ for i in position .. content.Count - 1 -> content[i] ]
        position <- content.Count - 1
        out
        
        
        
type ErrorMessage =
    {
        Message : string
        Line : int
    }
    
    
    
type InterpreterOutputStreams() =
    let standard = OutputStream<string>()
    let error = OutputStream<ErrorMessage>()
    let metaInfo = OutputStream<string>()
    member this.Standard = standard
    member this.Error = error
    member this.MetaInfo = metaInfo
    

