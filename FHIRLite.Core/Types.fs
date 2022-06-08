module FHIRLite.Core.Types

type TypeId =
    {
        Type: string
        Id: string
    }
    member this.TypeId = $"{this.Type}/{this.Id}"

    static member From (_type: string) (_id: string) =
        { Type = _type; Id = _id }

type TokenValue = { System: string; Code: string }
