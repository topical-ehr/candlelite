module FHIRLite.Core.JSON

open FHIRLite.Core.Types


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
    }

let metaInfo (elt: IJsonElement) =
    {
        VersionId = elt.GetString [ "meta"; "versionId" ]
        LastUpdated = elt.GetString [ "meta"; "lastUpdated" ]
    }

let resourceType (elt: IJsonElement) =
    elt.GetString [ "resourceType" ]

let resourceId (elt: IJsonElement) =
    {
        Type = resourceType elt
        Id = elt.GetString [ "id" ]
    }


type HashSetOfStrings = System.Collections.Generic.HashSet<string>

let allReferences (resource: IJsonElement) =
    let set = HashSetOfStrings()

    resource.WalkAndModify(fun prop value ->
        if prop = "reference" then
            set.Add value |> ignore

        None
    )

    set
