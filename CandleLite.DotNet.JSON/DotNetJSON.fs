module CandleLite.DotNet.JsonViaJsonNode

open System
open System.Text.Json
open System.Text.Json.Nodes
open System.Text.Json.Serialization

open CandleLite.Core
open CandleLite.Core.Types

type JsonViaJsonNode(root: JsonNode) =

    do
        if root = null then
            invalidArg "root" "root JsonNode is null"

    static member Parse(json: string) =
        let node = JsonNode.Parse json
        JsonViaJsonNode node

    override this.ToString() = root.ToJsonString()

    member _.Node = root

    interface JSON.IJsonElement with

        member this.WalkAndModify func =

            let rec walk (node: JsonNode) =
                match node with
                | :? JsonArray as arr -> arr |> Seq.iter walk
                | :? JsonObject as object ->
                    object
                    |> Seq.toArray
                    |> Array.iter (fun pair ->
                        match pair.Value with
                        | :? JsonArray
                        | :? JsonObject -> walk pair.Value
                        | :? JsonValue as value ->
                            match value.TryGetValue<string>() with
                            | true, stringValue ->
                                match func pair.Key stringValue with
                                | Some newValue -> object[pair.Key] <- newValue
                                | None -> ()
                            | _ -> ()
                        | _ -> failwithf $"unexpected type %A{pair.Value}"
                    )
                | _ -> ()

            walk root

        member this.GetString(path: string list) : string =

            let mutable node = root

            try
                for prop in path do
                    if node <> null then node <- node.[prop]

                if node <> null then
                    let v = node.AsValue()

                    match v.TryGetValue<bool>() with
                    | true, v -> if v then "true" else "false"
                    | _ -> node.GetValue()
                else
                    ""
            with
            | :? InvalidOperationException ->
                invalidArg "path" (sprintf "could not get string at path %A" path)

        member this.GetStrings(path: string list) : string list =

            match path with
            | [] -> invalidArg "path" "empty path"
            | [ prop ] ->
                match root[prop] with
                | null -> []
                | :? JsonArray as arr ->
                    arr |> Seq.map (fun n -> n.GetValue<string>()) |> Seq.toList
                | n -> [ n.GetValue() ]
            | _ ->
                let (heads, last) = path |> List.splitAt (path.Length - 1)
                let elts = (this :> JSON.IJsonElement).GetElements(heads)
                elts |> List.collect (fun e -> e.GetStrings last)

        member this.GetElements(path: string list) : JSON.IJsonElement list =

            let rec toNodeList (path: string list) (node: JsonNode) =

                match path with
                | prop1 :: rest ->
                    match node.[prop1] with
                    | :? JsonArray as arr -> arr |> Seq.collect (toNodeList rest)
                    | null -> []
                    | _ as node -> toNodeList rest node
                | [] -> [ node ]

            toNodeList path root
            |> Seq.map (JsonViaJsonNode >> (fun x -> x :> JSON.IJsonElement))
            |> Seq.toList

        member this.SetString(path: string list, value: string) : unit =
            let mutable node = root

            for i, prop in List.indexed path do
                if i = path.Length - 1 then
                    node[prop] <- value
                else

                    if node.[prop] = null then
                        node.[prop] <- JsonObject()

                    node <- node.[prop]

type SerializationConverter() =
    inherit JsonConverter<JSON.IJsonElement>()

    override _.Read(reader, _type, options) =
        let node = JsonNode.Parse &reader
        JsonViaJsonNode node

    override _.Write(writer, value, options) =
        (value :?> JsonViaJsonNode).Node.WriteTo writer


type DotNetJSON(?indent: bool) =
    let opts = JsonSerializerOptions()
    do opts.Converters.Add(SerializationConverter())
    do opts.DefaultIgnoreCondition <- JsonIgnoreCondition.WhenWritingNull
    do opts.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
    do opts.WriteIndented <- defaultArg indent false

    interface Server.ICandleLiteJSON with
        member _.ParseJSON(json: string) =
            JsonViaJsonNode.Parse json

        member _.BundleToJSON(bundle: Bundle.Bundle) =
            JsonSerializer.Serialize(bundle, opts)

        member _.OutcomeToJSON(oo: OperationOutcome) =
            JsonSerializer.Serialize(oo, opts)

        member _.ParseBundle(resource: JSON.IJsonElement) : Bundle.Bundle =
            let node = (resource :?> JsonViaJsonNode).Node
            JsonSerializer.Deserialize(node, opts)
