module FHIRLite.Core.Server

open FHIRLite.Core.Types

type IServerDependencies =
    // JSON can be parsed using platform libs (e.g. System.Text.Json or JSON.parse)
    abstract member ParseJSON: json: string -> JSON.IJsonElement

    // Run SQL on an imstance of sqlite or perhaps soon some other DBMS
    abstract member RunSQL: statement: SQL.Statement -> (obj array) seq

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

type GetHeader = delegate of string -> string
type SetHeader = delegate of string * string -> unit
type SetStatus = delegate of int -> unit
type SetBody = delegate of string -> unit

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

let addLocation location response =
    { response with
        Location = Some location
    }

type FHIRLiteServer(deps: IServerDependencies) =

    let runQuery = deps.RunSQL >> Seq.toList
    let runCommands = deps.RunSQL >> ignore

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

    let read _type _id req =
        let id = { Type = _type; Id = _id }

        let results = SQL.readResourceViaIndex (SQL.IndexConditions._id id) |> runQuery

        let reply =
            match results.Length with
            | 0 -> respondWith 404 "not found"
            | 1 ->
                let json = results[0][0] |> string
                let resource = deps.ParseJSON json
                json |> respondWith 200 |> addETagAndLastUpdated resource
            | _ -> failwithf "multiple entries!"

        reply

    let vread _type _id versionId req =
        failwithf "not implemented"

    let historyForId _type _id req =
        failwithf "not implemented"

    let historyForType _type req =
        failwithf "not implemented"

    let historyForServer req =
        failwithf "not implemented"

    let search _type req =
        failwithf "not implemented"

    let respondAsClientPrefers status (req: Request) (body: string) =
        match req.PreferReturn with
        | None
        | Some Minimal -> respondWith status ""
        | Some Representation -> respondWith status body
        | Some OperationOutcome -> respondWith status "TODO..."

    let updateMeta (resource: JSON.IJsonElement) =
        let newVersionId = nextVersionId ()
        let newLastUpdated = deps.CurrentDateTime.ToString("o")

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
            let id = IdValue.From(_type, _id)

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
                        Search.indexResource deps.SearchParameters resource _type meta |> runCommands

                        let json = resource.ToString()

                        SQL.insertResourceVersion { Type = _type; Id = _id } meta json |> runCommands

                        // respond
                        let location = $"{_type}/{_id}/_history/{meta.VersionId}"

                        respondAsClientPrefers 200 req json
                        |> addETagAndLastUpdated resource
                        |> addLocation location

                    | _ -> respondWith 404 $"existing resource not found ({id.RefString})"


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
            Search.indexResource deps.SearchParameters resource _type meta |> runCommands

            let json = resource.ToString()
            SQL.insertResourceVersion newId meta json |> runCommands

            // respond
            let location = $"{_type}/{newId}/_history/{meta.VersionId}"

            (respondAsClientPrefers 201 req json |> addETagAndLastUpdated resource)
            |> addLocation location


    member this.GET(req: Request) =

        match req.URL.PathSegments with
        | [| _type; _id; "_history" |] -> historyForId _type _id req
        | [| _type; "_history" |] -> historyForType _type req
        | [| "_history" |] -> historyForServer req

        | [| _type; _id; "_history"; versionId |] -> vread _type _id versionId req
        | [| _type; _id |] -> read _type _id req
        | [| _type |] -> search _type req

        | _ -> respondWith 400 "invalid path in URL"

    member this.HTTP(method: string, url: string, body: JSON.IJsonElement, getHeader: GetHeader, setHeader: SetHeader) =

        let header name =
            match getHeader.Invoke(name) with
            | null
            | "" -> None
            | str -> Some str

        let req =
            {
                URL = URL.parse url
                Body = Option.ofObj body
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
