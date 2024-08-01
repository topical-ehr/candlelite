module CandleLite.Core.ServerUtils 

open CandleLite.Core.Types

type PreferReturn =
    | Minimal
    | Representation
    | OperationOutcome

type Request =
    {
        URL: URL.FhirURL
        Body: JSON.IJsonElement option

        IfMatch: string option
        IfModifiedSince: string option
        IfNoneExist: string option
        PreferReturn: PreferReturn option
    }
    static member forURL url =
        {
            URL = url
            Body = None
            IfMatch = None
            IfModifiedSince = None
            IfNoneExist = None
            PreferReturn = None
        }

type Response =
    {
        Status: int

        // IJsonElement is used when the resource needs to be added to a bundle
        // string is used when it can get sent directly to the client (saving need to serialise again)
        BodyResource: JSON.IJsonElement
        BodyString: string

        Location: string option
        TypeId: TypeId option
        ETag: string option
        LastUpdated: string option
    }

type StorageMode =
    | CheckRefsIndexAndStore
    | PreliminaryIndexing of allReferences: JSON.HashSetOfStrings
    | StoreOnly
    | IndexAndStore


let respondWith status bodyResource bodyString =
    {
        Status = status
        BodyResource = bodyResource
        BodyString = bodyString
        Location = None
        TypeId = None
        ETag = None
        LastUpdated = None
    }

let addETagAndLastUpdated (resource: JSON.IJsonElement) response =
    let VersionId = resource.GetString [ "meta"; "versionId" ]
    let LastUpdated = resource.GetString [ "meta"; "lastUpdated" ]

    { response with
        ETag = Some $"W/\"{VersionId}\""
        LastUpdated = Some(LastUpdated)
    }

let addLocation (id: TypeId) (meta: JSON.MetaInfo) response =
    let location = $"%s{id.Type}/%s{id.Id}/_history/%s{meta.VersionId}"

    { response with
        Response.Location = Some location
        Response.TypeId = Some id
    }

type GetHeader = delegate of string -> string
type SetHeader = delegate of string * string -> unit
type SetStatus = delegate of int -> unit
type SetBody = delegate of string -> unit

