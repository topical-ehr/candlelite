module FHIRLite.Core.JSON

open FHIRLite.Core.Types


[<AllowNullLiteral>]
type IJsonElement =

    abstract GetString: path: string list -> string
    abstract GetStrings: path: string list -> string list

    abstract GetElements: path: string list -> IJsonElement list

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
