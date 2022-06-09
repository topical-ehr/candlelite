module FHIRLite.Core.Server

open FHIRLite.Core.Types

type IFHIRLiteDB =
    // Run SQL on an instance of sqlite or perhaps soon some other DBMS
    abstract member RunSQL: statement: SQL.Statement -> (obj array) seq

type IFHIRLiteJSON =
    // JSON can be parsed using platform libs (e.g. System.Text.Json or JSON.parse)
    abstract member ParseJSON: json: string -> JSON.IJsonElement

type IFHIRLiteConfig =
    // allow customisation of parameters
    abstract member SearchParameters: Search.ParametersMap

    // returns time that's used to set lastUpdated - can be overriden to use a fixed value (e.g. for tests)
    abstract member CurrentDateTime: System.DateTime

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

type Response =
    {
        Status: int
        Body: string

        Location: string option
        ETag: string option
        LastUpdated: string option
    }

let respondWith status body =
    {
        Status = status
        Body = body
        Location = None
        ETag = None
        LastUpdated = None
    }

let addETagAndLastUpdated resource response =
    let metaInfo = JSON.metaInfo resource

    { response with
        ETag = Some $"W/\"{metaInfo.VersionId}\""
        LastUpdated = Some(metaInfo.LastUpdated)
    }

let addLocation (id: TypeId) (meta: JSON.MetaInfo) response =
    let location = $"%s{id.Type}/%s{id.Id}/_history/%s{meta.VersionId}"

    { response with
        Location = Some location
    }

type GetHeader = delegate of string -> string
type SetHeader = delegate of string * string -> unit
type SetStatus = delegate of int -> unit
type SetBody = delegate of string -> unit

[<AbstractClass>]
type IFHIRLiteServer() =
    // defined as an interface to prevent Fable's name mangling
    abstract member HandleRequest: method: string * url: string * body: string * getHeader: GetHeader * setHeader: SetHeader -> Response


type FHIRLiteServer(config: IFHIRLiteConfig, dbImpl: IFHIRLiteDB, jsonImpl: IFHIRLiteJSON) =

    let runQuery = dbImpl.RunSQL >> Seq.toList
    let runCommands = dbImpl.RunSQL >> Seq.toList >> ignore

    let nextCounter name =
        let results = SQL.updateCounter name |> runQuery

        match results with
        | [] ->
            // counter not present
            let results = SQL.insertCounter name |> runQuery
            let one = box 1L

            match results with
            | [ [| x |] ] when x = one -> ()
            | _ -> failwithf "invalid result for insertCounter: %A" results

            "1"

        | [ [| value |] ] -> value :?> int64 |> string
        | _ -> failwithf "invalid result for updateCounter: %A" results

    let nextVersionId () =
        nextCounter "versionId"

    let respondWithSingleResource expectedId (results: obj array list) =
        match results.Length with
        | 0 -> respondWith 404 "not found"
        | 1 ->
            let json = results[0][0] |> string
            let resource = jsonImpl.ParseJSON json

            let idFromResource = JSON.resourceId resource

            if idFromResource <> expectedId then
                respondWith 404 "version corresponds to a different resource"
            else
                json |> respondWith 200 |> addETagAndLastUpdated resource
        | _ -> failwithf "multiple entries!"

    let read _type _id req =
        let id = TypeId.From _type _id

        SQL.readResourcesViaIndex [ (SQL.IndexConditions._id id) ]
        |> runQuery
        |> respondWithSingleResource id

    let vread _type _id versionId req =
        let id = TypeId.From _type _id

        SQL.readVersion versionId |> runQuery |> respondWithSingleResource id

    let historyForId _type _id req =
        let id = TypeId.From _type _id
        failwithf "not implemented"

    let historyForType _type req =
        failwithf "not implemented"

    let historyForServer req =
        failwithf "not implemented"

    let searchParams (req: Request) = req.URL.Parameters

    let search _type req =
        let pr = searchParams req

        let conditions =
            [
                for p in pr do
                    SQL.IndexConditions.valueEqual _type p.Name p.Value
            ]
            |> SQL.readResourcesViaIndex
            |> runQuery

        failwithf "not implemented"

    let respondAsClientPrefers status (req: Request) (body: string) =
        match req.PreferReturn with
        | None
        | Some Minimal -> respondWith status ""
        | Some Representation -> respondWith status body
        | Some OperationOutcome -> respondWith status "TODO..."

    let updateMeta (resource: JSON.IJsonElement) =
        let newVersionId = nextVersionId ()

        let newLastUpdated =
            config
                .CurrentDateTime
                .ToUniversalTime()
                .ToString("o")

        resource.SetString([ "meta"; "versionId" ], newVersionId)

        resource.SetString([ "meta"; "lastUpdated" ], newLastUpdated)

        {
            JSON.VersionId = newVersionId
            JSON.LastUpdated = newLastUpdated
        }

    member this.PUT(req: Request) =
        (*
            Spec extracts:

            Creates a new current version for an existing resource or creates an initial version if no resource already exists for the given id.
            The request body SHALL be a Resource with an id element that has an identical value to the [id] in the URL.
            If the request body includes a meta, the server SHALL ignore the provided versionId and lastUpdated values.
            If the interaction is successful, the server SHALL return either a 200 OK HTTP status code if the resource was updated, or a 201 Created status code if the resource was created
            Servers MAY choose to allow clients to PUT a resource to a location that does not yet exist on the server - effectively, allowing the client to define the id of the resource.
            -- https://www.hl7.org/fhir/http.html#update
        *)
        match req.URL.PathSegments with
        | [| _type; _id |] ->
            let id = TypeId.From _type _id

            match req.Body with
            | None -> respondWith 400 "not JSON body in PUT (update) request"
            | Some resource ->
                // for now we don't allow client-generated IDs
                let idFromResource = JSON.resourceId resource

                if id <> idFromResource then
                    respondWith 400 "type and id in URL don't match the resource"
                else
                    // find existing versionId (to delete old index entries)
                    let versionIdResult = SQL.indexQuery (SQL.IndexConditions._id id) |> runQuery

                    match versionIdResult with
                    | [ [| existingVersionId |] ] ->
                        // TODO: update unchanged index entries instead of deleting?
                        Search.deleteIndexForVersion (string existingVersionId) |> runCommands

                        // create and set a new versionId
                        let meta = updateMeta resource

                        // store in DB
                        Search.indexResource config.SearchParameters resource _type meta |> runCommands

                        let json = resource.ToString()

                        SQL.insertResourceVersion { Type = _type; Id = _id } meta json |> runCommands

                        // respond
                        respondAsClientPrefers 200 req json
                        |> addETagAndLastUpdated resource
                        |> addLocation id meta

                    | _ -> respondWith 404 $"existing resource not found ({id.TypeId})"


        | _ -> respondWith 400 "invalid path in URL"


    member this.POST(req: Request) =
        (*
            Spec extracts:

            creates a new resource in a server-assigned location
            If an id is provided, the server SHALL ignore it.
            The server SHALL populate the id, meta.versionId and meta.lastUpdated with the new correct values
            -- https://www.hl7.org/fhir/http.html#create
        *)

        // TODO: check URL path?

        match req.Body with
        | None -> respondWith 400 "not JSON body in POST (create) request"
        | Some resource ->
            // create and set IDs
            let _type = JSON.resourceType resource
            let newId = { Type = _type; Id = nextCounter _type }
            resource.SetString([ "id" ], newId.Id)
            let meta = updateMeta resource

            // store in DB
            Search.indexResource config.SearchParameters resource _type meta |> runCommands

            let json = resource.ToString()
            printfn "inserting version"
            SQL.insertResourceVersion newId meta json |> runCommands
            printfn "inserted version: %A" (SQL.insertResourceVersion newId meta json)

            // respond
            (respondAsClientPrefers 201 req json |> addETagAndLastUpdated resource)
            |> addLocation newId meta


    member this.GET(req: Request) =

        match req.URL.PathSegments with
        | [| _type; _id; "_history" |] -> historyForId _type _id req
        | [| _type; "_history" |] -> historyForType _type req
        | [| "_history" |] -> historyForServer req

        | [| _type; _id; "_history"; versionId |] -> vread _type _id versionId req
        | [| _type; _id |] -> read _type _id req
        | [| _type |] -> search _type req

        | _ -> respondWith 400 "invalid path in URL"

    member this.HandleRequest(method: string, url: string, body: string, getHeader: GetHeader, setHeader: SetHeader) =

        let header name =
            match getHeader.Invoke(name) with
            | null
            | "" -> None
            | str -> Some str

        let req =
            {
                URL = URL.parse url
                Body =
                    if System.String.IsNullOrEmpty body then
                        None
                    else
                        Some <| jsonImpl.ParseJSON body
                IfMatch = header "if-match"
                IfModifiedSince = None
                IfNoneExist = None
                PreferReturn = None

            }

        let res =
            match method with
            | "GET" -> this.GET(req)
            | "POST" -> this.POST(req)
            | "PUT" -> this.PUT(req)
            | _ -> respondWith 405 "method not allowed"

        for v, name in
            [
                res.ETag, "ETag"
                res.Location, "Location"
                res.LastUpdated, "Last-Modified"
            ] do
            v |> Option.iter (fun v -> setHeader.Invoke(name, v))

        res
