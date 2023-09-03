namespace CandleLite.JS.JSON

open System
open Fable.Core
open Fable.Core.JsInterop
open Thoth.Json

open CandleLite.Core
open CandleLite.Core.Types

/// Implements CandleLite's IJsonElement over a
/// plain JavaScript object (the output of JSON.parse)
type JsonViaJSObj(root: obj) =

    do
        if root = null then
            invalidArg "root" "root object is null"

    static member Parse(json: string) =
        JsonViaJSObj (JS.JSON.parse json)

    override this.ToString() = JS.JSON.stringify(root)

    member _.Object = root

    interface JSON.IJsonElement with

        member this.WalkAndModify callback =

            let rec walk (node: obj) =
                if Utils.isArray node then
                    let x : obj array = unbox node
                    Array.iter walk x

                elif Utils.isObject node then

                    for k, v in JS.Constructors.Object.entries node do
                        if Utils.isArray v || Utils.isObject v then
                            walk v
                           
                        elif Utils.isString v then

                            match callback k (unbox v) with
                            | Some newValue -> node?(k) <- newValue
                            | None -> ()
               
            walk root

        member this.GetString(path: string list) : string =

            let mutable node = root

            for prop in path do
                if node <> null then node <- node?(prop)

            if node <> null then
                node.ToString()
            else
                ""

        member this.GetStrings(path: string list) : string list =

            match path with
            | [] -> invalidArg "path" "empty path"
            | [ prop ] ->
                match root?(prop) with
                | null -> []
                | arr when Utils.isArray arr ->
                    arr |> Seq.map string |> Seq.toList
                | n -> [ n.ToString() ]
            | _ ->
                let (heads, last) = path |> List.splitAt (path.Length - 1)
                let elts = (this :> JSON.IJsonElement).GetElements(heads)
                elts |> List.collect (fun e -> e.GetStrings last)

        member this.GetElements(path: string list) : JSON.IJsonElement list =

            let rec toNodeList (path: string list) (node: obj) =

                match path with
                | prop1 :: rest ->
                    match node?(prop1) with
                    | arr when Utils.isArray arr -> arr |> Seq.collect (toNodeList rest)
                    | null -> []
                    | _ as node -> toNodeList rest node
                | [] -> [ node ]

            toNodeList path root
            |> Seq.map (JsonViaJSObj >> (fun x -> x :> JSON.IJsonElement))
            |> Seq.toList

        member this.SetString(path: string list, value: string) : unit =
            let mutable node = root

            for i, prop in List.indexed path do
                if i = path.Length - 1 then
                    node?(prop) <- value
                else
                    if node?( prop ) = null then
                        node?( prop ) <- createObj []

                    node <- node?( prop )

module ThothCodec =
    let decoder : Decoder<JSON.IJsonElement> =
        fun path value -> Ok <| JsonViaJSObj value

    let encoder (elt: JSON.IJsonElement) =
        elt.ToString() |> box

/// Implements ICandleLiteJSON for JavaScript environments
/// using the natively available JSON.parse
/// 
/// (gets compiled to JavaScript using Fable)
type JsJSON(?indent: bool) =

    let toJSON obj =
        if indent = Some true then
            JS.JSON.stringify(obj, space=4)
        else
            JS.JSON.stringify(obj)

    let extraDecoders = Extra.empty |> Extra.withCustom ThothCodec.encoder ThothCodec.decoder
    let bundleDecoder = Decode.Auto.generateDecoderCached<Bundle.Bundle>(caseStrategy = CaseStrategy.CamelCase, extra = extraDecoders)

    interface Server.ICandleLiteJSON with
        member _.ParseJSON(json: string) =
            JsonViaJSObj.Parse json

        member _.BundleToJSON(bundle: Bundle.Bundle) =
            
            let encodeRequest (req: Bundle.BundleRequest) =
                createObj [
                    "method" ==> req.Method
                    "url" ==> req.Url
                    "ifNoneMatch" ==> req.IfNoneMatch
                    "ifModifiedSince" ==> req.IfModifiedSince
                    "ifMatch" ==> req.IfMatch
                    "ifNoneExist" ==> req.IfNoneExist
                ]

            let encodeNodeOrNull (maybe: JSON.IJsonElement option) =
                match maybe with
                | Some node ->
                    let node: JsonViaJSObj = unbox node
                    node.Object
                | None -> null

            let encodeResponse (res: Bundle.BundleResponse) =
                createObj [
                    "status" ==> res.Status
                    "location" ==> res.Location
                    "etag" ==> res.Etag
                    "lastModified" ==> res.LastModified
                    "outcome" ==> encodeNodeOrNull res.Outcome
                ]

            let encodeEntry (entry: Bundle.BundleEntry) =
                let resource =
                    match entry.Resource with
                    | Some resource -> (resource :?> JsonViaJSObj).Object
                    | None -> null

                createObj [
                    "fullUrl" ==> entry.FullUrl
                    "resource" ==> resource
                    "request" ==> (Option.map encodeRequest entry.Request |> Option.defaultValue null)
                    "response" ==> (Option.map encodeResponse entry.Response |> Option.defaultValue null)
                ]
            
            let encodeLink (link: Bundle.BundleLink) =
                createObj [
                    "relation" ==> link.Relation
                    "uri" ==> link.Uri
                ]

            createObj [
                "resourceType" ==> bundle.ResourceType
                "type" ==> bundle.Type
                "total" ==> bundle.Total
                "timestamp" ==> bundle.Timestamp
                "link" ==> (bundle.Link |> Option.defaultValue [||] |> Array.map encodeLink)
                "entry" ==> (bundle.Entry |> Option.defaultValue [||] |> Array.map encodeEntry)
            ] |> toJSON

        member _.OutcomeToJSON(oo: OperationOutcome) =
            createObj [
                "resourceType" ==> oo.ResourceType
                "issue" ==> (Array.ofList oo.Issue |>
                    Array.map(
                        fun issue -> createObj [
                            "severity" ==> issue.Severity
                            "code" ==> issue.Code
                            "diagnostics" ==> issue.Diagnostics
                        ]
                    )
                )
            ] |> toJSON

        member this.ParseBundle(resource: JSON.IJsonElement) : Bundle.Bundle =
            let obj = (resource :?> JsonViaJSObj).Object

            match Decode.fromValue "$" bundleDecoder obj with
            | Ok decoded -> decoded
            | Result.Error msg -> Utils.raiseOO 400 Structure msg


