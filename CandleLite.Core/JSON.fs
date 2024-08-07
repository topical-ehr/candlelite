﻿module CandleLite.Core.JSON

open CandleLite.Core.Types


[<AllowNullLiteral>]
type IJsonElement =

    abstract GetString: path: string list -> string
    abstract GetStrings: path: string list -> string list

    abstract GetElements: path: string list -> IJsonElement list

    abstract WalkAndModify: func: (string -> string -> string option) -> unit

    abstract SetString: path: string list * value: string -> unit

type MetaInfo =
    {
        VersionId: string
        LastUpdated: string
        LastUpdatedUnixTime: int64
    }

let resourceType (elt: IJsonElement) =
    elt.GetString [ "resourceType" ]

let resourceId (elt: IJsonElement) =
    {
        Type = resourceType elt
        Id = elt.GetString [ "id" ]
    }


type HashSetOfStrings = System.Collections.Generic.HashSet<string>

let collectReferences (set: HashSetOfStrings) (resource: IJsonElement) =
    resource.WalkAndModify(fun prop value ->
        if prop = "reference" then
            set.Add value |> ignore

        None
    )
