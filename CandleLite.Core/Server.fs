module CandleLite.Core.Server

open CandleLite.Core.Types
open CandleLite.Core.Bundle

type ICandleLiteDB =
    // Run SQL on an instance of sqlite or perhaps soon some other DBMS
    abstract member RunSql: statement: SQL.Statement -> (obj array) list
    abstract member RunSqlLazily: statement: SQL.Statement -> (obj array) seq

type ICandleLiteJSON =
    // JSON can be parsed using platform libs (e.g. System.Text.Json or JSON.parse)
    abstract member ParseJSON: json: string -> JSON.IJsonElement

    abstract member ToJSON: bundle: Bundle -> string
    abstract member ToJSON: oo: OperationOutcome -> string

    abstract member ParseBundle: json: string -> Bundle
    abstract member ParseBundle: resource: JSON.IJsonElement -> Bundle

type ICandleLiteConfig =
    // allow customisation of parameters
    abstract member SearchParameters: Indexes.ParametersMap

    // returns time that's used to set lastUpdated - can be overriden to use a fixed value (e.g. for tests)
    abstract member CurrentDateTime: System.DateTime

module Private =
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

    let addETagAndLastUpdated resource response =
        let metaInfo = JSON.metaInfo resource

        { response with
            ETag = Some $"W/\"{metaInfo.VersionId}\""
            LastUpdated = Some(metaInfo.LastUpdated)
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

open Private


[<AbstractClass>]
type ICandleLiteServer() =
    // defined as an interface to prevent Fable's name mangling
    abstract member HandleRequest:
        method: string * url: string * body: string * getHeader: GetHeader * setHeader: SetHeader ->
            Response


type CandleLiteServer(config: ICandleLiteConfig, dbImpl: ICandleLiteDB, jsonImpl: ICandleLiteJSON) =

    let runQuery = dbImpl.RunSql
    let runCommand = dbImpl.RunSql >> ignore

    let currentTimestamp () =
        config.CurrentDateTime.ToUniversalTime().ToString("o")

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

    let respondWithOO httpStatus (oo: OperationOutcome) =
        let json = jsonImpl.ToJSON oo
        let resource = jsonImpl.ParseJSON json // TODO: can be a bit more efficient?
        respondWith httpStatus resource json

    let raiseOO httpStatus code msg =
        let oo = operationOutcome Error code msg
        raise <| OperationOutcomeException(httpStatus, oo)

    let respondWithSingleResource expectedId (results: obj array list) =
        match results.Length with
        | 0 -> raiseOO 404 Not_Found "not found"
        | 1 ->
            let json = results[0][0] |> string
            let deleted = results[0][1] |> unbox<int64>

            if deleted = 1 then
                raiseOO 410 Deleted "deleted"
            else
                let resource = jsonImpl.ParseJSON json
                let idFromResource = JSON.resourceId resource

                if idFromResource <> expectedId then
                    raiseOO 404 Value "version corresponds to a different resource"
                else
                    respondWith 200 resource json |> addETagAndLastUpdated resource
        | _ -> failwithf "multiple entries!"

    let respondWithBundle status (bundle: Bundle) =
        let json = jsonImpl.ToJSON bundle
        // TODO: avoid re-parsing?
        respondWith status (jsonImpl.ParseJSON json) json

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

        let addTypeClauseIfNeeded (conditions: SQL.WhereCondition list list) =
            // adds an index search clause for "WHERE name = type._id"
            // in cases where there are no other type-specific criteria
            // e.g. when just doing a "GET /Patient"
            let typeDot = _type + "."

            let alreadyHas =
                conditions
                |> List.exists (fun clause ->
                    clause
                    |> List.exists (fun col ->
                        match col.Column, col.Condition with
                        | "name", SQL.Equal (SQL.StringValue name) -> name.StartsWith typeDot
                        | _ -> false

                    )
                )

            if alreadyHas then
                conditions
            else
                conditions @ [ SQL.IndexConditions._type _type ]

        let results =
            [
                for p in pr do
                    SQL.IndexConditions.valueEqual _type p.Name p.Value
            ]
            |> addTypeClauseIfNeeded
            |> SQL.readResourcesViaIndex
            |> runQuery

        let bundle =
            {
                ResourceType = "Bundle"
                Total = results.Length
                Type = BundleType.SearchSet
                Timestamp = currentTimestamp ()
                Link = [||]
                Entry =
                    [|
                        for row in results do
                            let json = row[0] |> string
                            let resource = jsonImpl.ParseJSON json
                            let idFromResource = JSON.resourceId resource

                            {
                                FullUrl = Some <| idFromResource.TypeId
                                Resource = Some resource
                                Request = None
                                Response = None
                            }
                    |]
            }

        bundle, respondWithBundle 200 bundle

    let respondAsClientPrefers status (req: Request) resource json =
        match req.PreferReturn with
        | None
        | Some Representation -> respondWith status resource json
        | Some Minimal -> respondWith status null ""
        | Some OperationOutcome -> raiseOO 400 Value "PreferReturn OperationOutcome not implemented" // TODO

    let updateVersionId (resource: JSON.IJsonElement) =
        let newVersionId = nextVersionId ()
        let newLastUpdated = currentTimestamp ()

        resource.SetString([ "meta"; "versionId" ], newVersionId)
        resource.SetString([ "meta"; "lastUpdated" ], newLastUpdated)

        {
            JSON.VersionId = newVersionId
            JSON.LastUpdated = newLastUpdated
        }

    let checkTypeIdReference typeId =
        let rows = SQL.readIsDeletedViaIndex [ (SQL.IndexConditions._id typeId) ] |> runQuery
        let ref = typeId.TypeId

        match rows with
        | [] -> raiseOO 404 Not_Found $"reference doesn't exist (%s{ref})"
        | [ row ] ->
            let deleted = (unbox<int64> row[0]) > 0

            if deleted then
                raiseOO 404 Deleted $"referenced resource is deleted (%s{ref})"

        | _ -> failwithf $"multiple resources for reference (%s{ref})!"

    let checkReferencesExist references =
        for ref in references do
            let {
                    URL.PathSegments = segments
                    URL.Parameters = parameters
                } =
                URL.parse ref

            if parameters.Length > 0 then
                raiseOO 400 Value $"reference should not have parameters (%s{ref})"

            match segments with
            | [| _type; _id |] ->
                let id = TypeId.From _type _id
                checkTypeIdReference id
            | _ -> raiseOO 400 Value $"invalid reference (%s{ref})"


    let storeResource mode id meta (resource: JSON.IJsonElement) =
        // get references
        let references = JSON.HashSetOfStrings()

        match mode with
        | CheckRefsIndexAndStore
        | PreliminaryIndexing _
        | IndexAndStore -> JSON.collectReferences references resource
        | StoreOnly -> ()

        // process references if required
        match mode with
        | CheckRefsIndexAndStore -> checkReferencesExist references
        | PreliminaryIndexing allReferences -> allReferences.UnionWith references
        | IndexAndStore
        | StoreOnly -> ()

        // store index entries
        match mode with
        | CheckRefsIndexAndStore
        | PreliminaryIndexing _
        | IndexAndStore ->
            Indexes.indexResource config.SearchParameters resource id meta references |> runCommand
        | StoreOnly -> ()


        // store json
        match mode with
        | PreliminaryIndexing _ -> ""
        | CheckRefsIndexAndStore
        | IndexAndStore
        | StoreOnly ->
            let json = resource.ToString()
            SQL.insertResourceVersion id meta json |> runCommand
            json

    let DELETE (req: Request) =
        // https://www.hl7.org/fhir/http.html#delete

        match req.URL.PathSegments with
        | [| _type; _id |] ->
            let id = TypeId.From _type _id

            // find existing versionId (to delete old index entries)
            let versionIdResult = SQL.indexQuery (SQL.IndexConditions._id id) |> runQuery

            match versionIdResult with
            | [ [| existingVersionId |] ] ->
                Indexes.deleteIndexForVersion (string existingVersionId) |> runCommand

                let newVersionId = nextVersionId ()
                let newLastUpdated = currentTimestamp ()

                let meta =
                    {
                        JSON.VersionId = newVersionId
                        JSON.LastUpdated = newLastUpdated
                    }

                SQL.insertDeletion id meta |> runCommand

                respondWith 204 null ""

            | _ -> raiseOO 404 Not_Found $"existing resource not found ({id.TypeId})"


        | _ -> raiseOO 400 Value "invalid path in URL"

    let PUT (req: Request) storeResource =
        // https://www.hl7.org/fhir/http.html#update

        match req.URL.PathSegments with
        | [| _type; _id |] ->
            let id = TypeId.From _type _id

            match req.Body with
            | None -> raiseOO 400 Structure "not JSON body in PUT (update) request"
            | Some resource ->
                // for now we don't allow client-generated IDs
                let idFromResource = JSON.resourceId resource

                if id <> idFromResource then
                    raiseOO 400 Value "type and id in URL don't match the resource"
                else
                    // find existing versionId (to delete old index entries)
                    let versionIdResult = SQL.indexQuery (SQL.IndexConditions._id id) |> runQuery

                    match versionIdResult with
                    | [ [| existingVersionId |] ] ->
                        // TODO: update unchanged index entries instead of deleting?
                        Indexes.deleteIndexForVersion (string existingVersionId) |> runCommand

                        let meta = updateVersionId resource
                        let json = storeResource id meta resource

                        // respond
                        respondAsClientPrefers 200 req resource json
                        |> addETagAndLastUpdated resource
                        |> addLocation id meta

                    | _ -> raiseOO 404 Not_Found $"existing resource not found ({id.TypeId})"


        | _ -> raiseOO 400 Value "invalid path in URL"

    let GET (req: Request) =

        match req.URL.PathSegments with
        | [| _type; _id; "_history" |] -> historyForId _type _id req
        | [| _type; "_history" |] -> historyForType _type req
        | [| "_history" |] -> historyForServer req

        | [| _type; _id; "_history"; versionId |] -> vread _type _id versionId req
        | [| _type; _id |] -> read _type _id req
        | [| _type |] -> search _type req |> snd

        | _ -> raiseOO 400 Value "invalid path in URL"

    let create _type (req: Request) storeResource =
        // https://www.hl7.org/fhir/http.html#create

        match req.Body with
        | None -> raiseOO 400 Structure "not JSON body in POST (create) request"
        | Some resource ->
            // create and set IDs
            let typeFromResource = JSON.resourceType resource

            if typeFromResource <> _type then
                raiseOO
                    400
                    Value
                    $"type in URL doesn't match that of resource: from URL '%s{_type}', form resource '%s{typeFromResource}'"

            let newId = { Type = _type; Id = nextCounter _type }
            resource.SetString([ "id" ], newId.Id)

            let meta = updateVersionId resource
            let json = storeResource newId meta resource

            // respond
            respondAsClientPrefers 201 req resource json
            |> addETagAndLastUpdated resource
            |> addLocation newId meta

    let createFromBundle (req: Request) storeResource =

        match req.URL.PathSegments with
        | [| _type |] -> create _type req storeResource
        | _ -> raiseOO 400 Value "invalid path for bundled POST request"


    let transaction (req: Request) =
        match req.Body with
        | None -> raiseOO 400 Value "missing request body"
        | Some body ->
            let bundle = jsonImpl.ParseBundle body

            let processEntry (entry: Bundle.BundleEntry) storeResource =
                let res =
                    match entry.Request with
                    | Some request ->
                        let req =
                            {
                                Body = entry.Resource
                                URL = URL.parse request.Url
                                IfMatch = request.IfMatch
                                IfModifiedSince = request.IfModifiedSince
                                IfNoneExist = request.IfNoneExist
                                PreferReturn = req.PreferReturn
                            }

                        match request.Method with
                        | "GET" -> GET req
                        | "POST" -> createFromBundle req storeResource
                        | "PUT" -> PUT req storeResource
                        | _ -> raiseOO 405 Value "method not allowed"

                    | _ ->
                        raiseOO 400 Value "transaction/batch entries should have a request element"

                {
                    FullUrl = None
                    Resource = Some res.BodyResource
                    Request = None
                    Response =
                        Some
                            {
                                Status = res.Status.ToString()
                                Location = res.Location
                                Etag = res.ETag
                                LastModified = res.LastUpdated
                                Outcome = None
                            }
                },
                res.TypeId

            let isTransaction =
                match bundle.Type with
                | "transaction" -> true
                | "batch" -> false
                | _ -> raiseOO 400 Value "expected batch or transaction bundle"

            // figure out execution order - needs special sorting for transactions
            let entryExecutionOrder =
                let incrementingIndices =
                    Array.zeroCreate bundle.Entry.Length |> Array.mapi (fun i _ -> i)

                if isTransaction then
                    // https://www.hl7.org/fhir/http.html#trules
                    let methodOrder = [| "DELETE"; "POST"; "PUT"; "PATCH"; "GET"; "HEAD" |]

                    let orderForTransactionEntry index =
                        match bundle.Entry[index].Request with
                        | Some request -> Array.findIndex ((=) request.Method) methodOrder
                        | None ->
                            raiseOO 400 Value "transaction/batch entries should have a request"

                    incrementingIndices
                    |> Array.sortWith (fun a b ->
                        (orderForTransactionEntry a) - (orderForTransactionEntry b)
                    )
                else
                    incrementingIndices

            if isTransaction then
                SQL.TransactionBeginImmediate |> runCommand

            try
                let storageFunction =
                    if isTransaction then
                        // make preliminary updates to the index and collect all references
                        // this lets searches find in-bundle resources

                        // take savepoint to roll back afterwards because we need to roll back
                        // updates to the id/versionId counters
                        // TODO: make more efficient?
                        let preliminaryIndex cmd =
                            dbImpl.RunSql(cmd "preliminary-index") |> ignore

                        SQL.Savepoint |> preliminaryIndex

                        let allReferences = JSON.HashSetOfStrings()

                        let fullUrlToResolvedId =
                            System.Collections.Generic.Dictionary<string, TypeId>()

                        entryExecutionOrder
                        |> Array.iter (fun index ->
                            let entry = bundle.Entry[index]

                            match entry.Request with
                            // ignore GETs
                            | Some req when req.Method = "GET" || req.Method = "HEAD" -> ()
                            | _ ->
                                let (_, typeId) =
                                    processEntry
                                        bundle.Entry[index]
                                        (storeResource (PreliminaryIndexing allReferences))

                                match typeId, entry.FullUrl with
                                | Some typeId, Some fullUrl ->
                                    try
                                        fullUrlToResolvedId.Add(fullUrl, typeId)
                                    with
                                    | :? System.ArgumentException ->
                                        raiseOO 400 Value $"Bundle has duplicate fullUrl {fullUrl}"
                                | None, _ -> invalidOp "processEntry did not return a type/id"
                                | _, None ->
                                    raiseOO
                                        400
                                        Value
                                        "transaction/batch entries should have fullUrl"
                        )

                        // update conditional and placeholder references
                        // check normal references
                        let referencesToUpdate =
                            System.Collections.Generic.Dictionary<string, TypeId>()

                        for reference in allReferences do
                            let parsed = URL.parse reference

                            match parsed.Parameters.Length, parsed.PathSegments with
                            | numParams, [| _type |] when numParams > 0 ->
                                // conditional reference
                                let (searchResultsBundle, _) = search _type (Request.forURL parsed)

                                match searchResultsBundle.Entry.Length with
                                | 1 ->
                                    let resolvedId =
                                        searchResultsBundle.Entry[0].Resource.Value
                                        |> JSON.resourceId

                                    referencesToUpdate.Add(reference, resolvedId)
                                | 0 ->
                                    raiseOO
                                        400
                                        Not_Found
                                        $"no matches for conditional reference (%s{reference})"
                                | _ ->
                                    raiseOO
                                        400
                                        Multiple_Matches
                                        $"multiple matches for conditional reference (%s{reference})"

                            | numParams, [| _type; _id |] when numParams = 0 ->
                                // normal resource reference
                                checkTypeIdReference (TypeId.From _type _id)

                            | numParams, [| oid |] when numParams = 0 && oid.StartsWith("urn:uuid:") ->
                                // placeholder UUID
                                match fullUrlToResolvedId.TryGetValue oid with
                                | true, typeId -> referencesToUpdate.Add(reference, typeId)
                                | false, _ ->
                                    raiseOO
                                        400
                                        Value
                                        $"placeholder reference not present as a fullUrl (%s{reference})"
                            | numParams, [| hashtag |] when numParams = 0 && hashtag.StartsWith("#") ->
                                // TODO: verify existence of contained resource
                                ()
                            | _ -> raiseOO 400 Value $"invalid reference (%s{reference})"

                        if referencesToUpdate.Count > 0 then
                            for entry in bundle.Entry do
                                entry.Resource
                                |> Option.iter (fun resource ->
                                    resource.WalkAndModify(fun prop value ->
                                        if prop = "reference" then
                                            match referencesToUpdate.TryGetValue value with
                                            | true, resolvedId -> Some resolvedId.TypeId
                                            | _ -> None
                                        else
                                            None
                                    )
                                )
                            // indexed references need to be re-done
                            // TODO: may be able to only undo affected resources
                            SQL.SavepointRollback |> preliminaryIndex
                            storeResource IndexAndStore
                        else
                            // preliminary index should be good
                            // SQL.SavepointRelease |> preliminaryIndex
                            // storeResource StoreOnly

                            // need to roll-back to restore counters
                            SQL.SavepointRollback |> preliminaryIndex
                            storeResource IndexAndStore
                    else
                        // batch
                        storeResource CheckRefsIndexAndStore

                // store resources and get response bundle entries
                let responseEntries =
                    Array.create<BundleEntry ValueOption> bundle.Entry.Length ValueNone

                entryExecutionOrder
                |> Array.iter (fun index ->
                    let entry = bundle.Entry[index]

                    let continueTransactionOnFailure () =
                        // Not sure if spec-compliant but HAPI currently doesn't fail whole transaction when a GET returns a 400 or 404

                        let isGET =
                            (entry.Request |> Option.map (fun r -> r.Method = "GET")) = Some true

                        let getsFailWholeTransaction = false
                        isGET && (getsFailWholeTransaction = false)

                    try
                        let (bundleEntry, _) = processEntry entry storageFunction
                        Array.set responseEntries index (ValueSome bundleEntry)
                    with
                    // For batches store an OperationOutcome as the response for each error.
                    // For transactions (expect perhaps GET entries..),
                    // the exception is unhandled so the transaction is rolled back
                    // and the error is returned by itself.
                    | OperationOutcomeException (status, oo) when
                        isTransaction = false || continueTransactionOnFailure ()
                        ->
                        let bundleEntry =
                            {
                                FullUrl = None
                                Resource = Some (respondWithOO status oo).BodyResource
                                Request = None
                                Response =
                                    Some
                                        {
                                            Status = string status
                                            Location = None
                                            Etag = None
                                            LastModified = None
                                            Outcome = None
                                        }
                            }

                        Array.set responseEntries index (ValueSome bundleEntry)
                )

                if isTransaction then
                    SQL.TransactionCommit |> runCommand

                // respond
                {
                    ResourceType = "Bundle"
                    Total = bundle.Entry.Length
                    Type = bundle.Type + "-response"
                    Timestamp = currentTimestamp ()
                    Link = [||]
                    Entry =
                        responseEntries
                        |> Array.map (
                            ValueOption.defaultWith (fun _ -> failwith "response entry is None")
                        )
                }
                |> respondWithBundle 200
            with
            | OperationOutcomeException (status, oo) ->
                SQL.TransactionRollback |> runCommand
                respondWithOO status oo
            | ex ->
                SQL.TransactionRollback |> runCommand
                raiseOO 500 Exception (ex.ToString())


    let POST (req: Request) storeResource =

        match req.URL.PathSegments with
        | [||] -> transaction req
        | [| _type |] -> create _type req storeResource
        | _ -> raiseOO 400 Value "invalid path for POST request"

    member this.HandleRequest
        (
            method: string,
            urlPath: string,
            basePath: string,
            body: string,
            getHeader: GetHeader,
            setHeader: SetHeader
        ) =

        let header name =
            match getHeader.Invoke(name) with
            | null
            | "" -> None
            | str -> Some str

        if not <| urlPath.StartsWith(basePath) then
            failwithf "URL (%s) doesn't start with base prefix (%s)" urlPath basePath

        let urlPathWithoutBase = urlPath.Substring(basePath.Length).Trim('/')

        try
            let req =
                {
                    URL = URL.parse urlPathWithoutBase
                    Body =
                        if System.String.IsNullOrEmpty body then
                            None
                        else
                            Some <| jsonImpl.ParseJSON body
                    IfMatch = header "if-match"
                    IfModifiedSince = header "if-modified-since"
                    IfNoneExist = header "if-none-exist"
                    PreferReturn =
                        match header "prefer" with
                        | Some "return=minimal" -> Some Minimal
                        | Some "return=representation" -> Some Representation
                        | Some "return=OperationOutcome" ->
                            raiseOO 400 Value "Prefer: OperationOutcome not yet supported"
                        | Some _ -> raiseOO 400 Value "invalid value for Prefer header"
                        | None -> None
                }

            let res =
                let storageFunction = storeResource CheckRefsIndexAndStore

                match method with
                | "GET" -> GET req
                | "POST" -> POST req storageFunction
                | "PUT" -> PUT req storageFunction
                | "DELETE" -> DELETE req
                | _ -> raiseOO 405 Value "method not allowed"

            for v, name in
                [ res.ETag, "ETag"; res.Location, "Location"; res.LastUpdated, "Last-Modified" ] do
                v |> Option.iter (fun v -> setHeader.Invoke(name, v))

            res

        with
        | OperationOutcomeException (status, oo) -> respondWithOO status oo
        | ex -> respondWithOO 500 (operationOutcome Error Exception (ex.ToString()))
