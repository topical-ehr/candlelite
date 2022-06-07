module FHIRLite.DotNet.JsonViaJsonNode

open System.Text.Json.Nodes

open FHIRLite.Core

type JsonViaJsonNode(root: JsonNode) =

    static member Parse(json: string) =
        let node = JsonNode.Parse json
        JsonViaJsonNode node

    interface JSON.IJsonElement with

        member this.GetString(path: string list) : string =

            let mutable node = root

            for prop in path do
                if node <> null then node <- node.[prop]

            if node <> null then
                let v = node.AsValue()

                match v.TryGetValue<bool>() with
                | true, v -> if v then "true" else "false"
                | _ -> node.GetValue()
            else
                ""

        member this.GetStrings(path: string list) : string list =

            let mutable node = root

            for prop in path do
                node <- node.[prop]

            node.GetValue()

        member this.GetElements(path: string list) : JSON.IJsonElement list =

            let rec toNodeList (path: string list) (node: JsonNode) =

                match path with
                | head :: tail ->
                    match node.[head] with
                    | :? JsonArray as arr -> arr |> List.ofSeq |> List.collect (toNodeList tail)
                    | _ as node -> toNodeList tail node
                | [] -> [ node ]

            toNodeList path root
            |> List.map (JsonViaJsonNode >> (fun x -> x :> JSON.IJsonElement))

        member this.SetString(path: string list, value: string) : unit =
            let mutable node = root
            printfn "SetString: %A" path

            for i, prop in List.indexed path do
                if i = path.Length - 1 then
                    printfn "  setting %s to %s (%A) (%A)" prop value node (node[prop])
                    node[prop] <- value
                else
                    printfn "  navigating down through %s" prop

                    if node.[prop] = null then
                        node.[prop] <- JsonObject()

                    node <- node.[prop]
