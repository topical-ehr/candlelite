﻿module CandleLite.Core.Indexes

open CandleLite.Core.Types
open CandleLite.Core.SQL

type Type =
    | Id
    | Number
    | Reference
    | DateTime
    | String
    | StringFuzzy // for parameters like "name" that can match on prefixes/typos/etc
    | Bool
    | Token

type IndexedValues =
    | Id of string
    | Number of decimal
    | Reference of TypeId
    | DateTime of string
    | String of string
    | Bool of bool
    | Token of TokenValue
    member this.ToValueAndSystemAndIsRef() =

        let normalise (str: string) =
            str.Trim().ToLower()

        let boxOrNull str =
            if System.String.IsNullOrEmpty str then
                null // will not be indexed
            else
                box str

        match this with
        // return (value, system, isRef)
        | Id _id -> box _id, null, null
        | Number n -> box n, null, null
        | Reference ref -> box ref.TypeId, null, box 1
        | DateTime str -> boxOrNull str, null, null
        | String str -> boxOrNull <| normalise str, null, null
        | Bool b -> box (if b then "true" else "false"), null, null
        | Token t -> boxOrNull t.Code, t.System, null


type SearchParameter =
    {
        Type: Type
        Indexer: JSON.IJsonElement -> IndexedValues list
    }

type ResourceTypeOrALL = string
type ParameterName = string

// Making ParametersMap a wrapper DU to make it visible from C#
// Otherwise one has to write FSharpMap<....>
type ParametersMap = ParametersMap of Map<ResourceTypeOrALL, list<ParameterName * SearchParameter>>

let indexer _type func = { Type = _type; Indexer = func }

let indexString path =
    indexer Type.String <| fun elt -> [ elt.GetString path |> String ]

let indexStrings path =
    indexer Type.String <| fun elt -> elt.GetStrings path |> List.map String

let indexDateTime path =
    indexer Type.DateTime <| fun elt -> [ elt.GetString path |> DateTime ]

let indexId = "_id", indexer Type.Id <| fun elt -> [ Id <| (JSON.resourceId elt).Id ]

let getElements path (elt: JSON.IJsonElement) =
    elt.GetElements path

let getString path (elt: JSON.IJsonElement) =
    elt.GetString path |> String

let getSystemValue (elt: JSON.IJsonElement) =
    Token
        {
            System = elt.GetString [ "system" ]
            Code = elt.GetString [ "value" ]
        }

let getSystemCode (elt: JSON.IJsonElement) =
    Token
        {
            System = elt.GetString [ "system" ]
            Code = elt.GetString [ "code" ]
        }

let identifier =
    "identifier", indexer Type.Token <| (getElements [ "identifier" ] >> (List.map (getSystemValue)))

let getReference name (elt: JSON.IJsonElement) =
    let parseReference (str: string) =
        match str.Split "/" with
        | [| _type; id |] -> [ Reference { Type = _type; Id = id } ]
        | [| placeholder |] when placeholder.StartsWith("urn:uuid:") -> []
        | [| hashtag |] when hashtag.StartsWith("#") -> [] // TODO: verify is a contained resource

        | _ -> failwithf "unable to parse reference in %s: %s" name str

    elt.GetString [ "reference" ] |> parseReference

let reference name =
    name, indexer Type.Reference <| (getElements [ name ] >> (List.collect (getReference name)))

let referenceWithPath name elementPath =
    name, indexer Type.Reference <| (getElements elementPath >> (List.collect (getReference name)))

let codeableConcept name =
    indexer Type.Token <| (getElements [ name; "coding" ] >> (List.map (getSystemCode)))


let getStrings path (elt: JSON.IJsonElement) =
    elt.GetStrings path |> List.map String

let indexElementStrings elementPath stringPath =
    indexer Type.String <| (getElements elementPath >> (List.collect (getStrings stringPath)))

let indexElementString elementPath stringPath =
    indexer Type.String <| (getElements elementPath >> (List.map (getString stringPath)))

let indexBool path =
    {
        Type = Type.Bool
        Indexer = fun elt -> [ (elt.GetString path) = "true" |> Bool ]
    }

let indexTrueOrDateExists path =
    let pathBool = [ (path + "Boolean") ]
    let pathDate = [ (path + "DateTime") ]

    {
        Type = Type.Bool
        Indexer =
            fun elt ->
                let bool = elt.GetString pathBool

                if bool = "true" then
                    [ Bool true ]
                else
                    [ Bool((elt.GetString pathDate) <> "") ]
    }

let contactPoints path filterForSystem =
    let filter =
        match filterForSystem with
        | None -> id
        | Some system ->
            List.filter (fun (e: JSON.IJsonElement) -> (e.GetString [ "system" ]) = system)

    {
        Type = Type.String
        Indexer =
            fun elt ->
                elt.GetElements path
                |> filter
                |> List.map (fun e -> e.GetString [ "value" ] |> String)
    }

let indexAddress path =
    {
        Type = Type.String
        Indexer =
            fun elt ->
                elt.GetElements path
                |> List.collect (fun e ->
                    ([
                        e.GetString [ "text" ]
                        e.GetString [ "city" ]
                        e.GetString [ "district" ]
                        e.GetString [ "state" ]
                        e.GetString [ "postalCode" ]
                        e.GetString [ "country" ]
                     ]
                     @ (e.GetStrings [ "line" ]))
                )
                |> List.map String
    }

let humanName path =
    {
        Type = Type.StringFuzzy
        Indexer =
            fun elt ->
                elt.GetElements path
                |> List.collect (fun e ->
                    ([ e.GetString [ "text" ]; e.GetString [ "family" ] ]
                     @ (e.GetStrings [ "given" ]))
                )
                |> List.map String
    }

let deleteIndexForVersion (versionId: string) =
    Delete
        {
            Table = Table.indexes
            Where =
                [
                    {
                        Column = "versionId"
                        Condition = Equal(StringValue versionId)
                    }
                ]
        }

let indexResource
    (ParametersMap paramsMap)
    (resource: JSON.IJsonElement)
    (id: TypeId)
    (meta: JSON.MetaInfo)
    (references: JSON.HashSetOfStrings) // NB: HashSet will be mutated!
    =

    let paramsToIndex =
        let paramsForType =
            match Map.tryFind id.Type paramsMap with
            | Some list -> list
            | None -> []

        let paramsForAll = (Map.find "ALL" paramsMap)

        paramsForAll @ paramsForType

    let allRows =
        paramsToIndex |> List.map (fun (name, sp) -> name, sp.Indexer resource)

    let versionId = box meta.VersionId

    let sql =
        Insert
            {
                Table = Table.indexes
                Columns = [
                    "name"
                    "value"
                    "system"
                    "isRef"
                    "id"
                    "versionId"
                    "lastUpdated"
                ]
                Values =
                    [
                        for name, rows in allRows do
                            let indexName = id.Type + "." + name
                            let boxedName = box indexName

                            let uniqueRows = rows |> Set.ofList |> Set.toArray

                            for indexRow in uniqueRows do
                                match indexRow with
                                // NB: mutating references HashSet!
                                | Reference typeId -> references.Remove typeId.TypeId |> ignore
                                | _ -> ()

                                let (v, sys, isRef) = indexRow.ToValueAndSystemAndIsRef()

                                if v <> null then
                                    [
                                        boxedName
                                        v
                                        sys
                                        isRef
                                        id.Id
                                        versionId
                                        meta.LastUpdatedUnixTime
                                    ]

                        // add unindexed references to enforce referential integrity
                        // (i.e. prevent deletion of the referenced resources)
                        for ref in references do
                            [
                                box "ref"
                                ref
                                null
                                1
                                id.Id
                                versionId
                                meta.LastUpdatedUnixTime
                            ]

                    ]
                Returning = []
            }

    sql
