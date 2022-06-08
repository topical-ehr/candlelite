module FHIRLite.Core.Search

open FHIRLite.Core.Types
open FHIRLite.Core.SQL

type IndexedValues =
    | Id of string
    | Number of decimal
    | Reference of TypeId
    | DateTime of string
    | String of string
    | Bool of bool
    | Token of TokenValue
    member this.ToValueAndSystem() =

        let normalise (str: string) =
            str.Trim().ToLower()

        let boxOrNull str =
            if System.String.IsNullOrEmpty str then
                null // will not be indexed
            else
                box str

        match this with
        | Id _id -> box _id, null
        | Number n -> box n, null
        | Reference ref -> box ref.TypeId, null
        | DateTime str -> boxOrNull str, null
        | String str -> boxOrNull <| normalise str, null
        | Bool b -> box (if b then "true" else "false"), null
        | Token t -> boxOrNull t.Code, t.System


type SearchParameter =
    {
        Indexer: JSON.IJsonElement -> IndexedValues list
    }

let indexer func = { Indexer = func }

let indexString path =
    indexer <| fun elt -> [ elt.GetString path |> String ]

let indexStrings path =
    indexer <| fun elt -> elt.GetStrings path |> List.map String

let indexDateTime path =
    indexer <| fun elt -> [ elt.GetString path |> DateTime ]

let indexId = "_id", indexer <| fun elt -> [ Id <| (JSON.resourceId elt).Id ]

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
    elt.GetString [ "system" ] + "|" + elt.GetString [ "code" ] |> String

let identifier =
    "identifier", indexer <| (getElements [ "identifier" ] >> (List.map (getSystemValue)))

let reference name =
    let getReference (elt: JSON.IJsonElement) =
        let parseReference (str: string) =
            match str.Split "/" with
            | [| _type; id |] -> Reference { Type = _type; Id = id }
            | _ -> failwithf "unable to parse reference: %s" str

        elt.GetString [ "reference" ] |> parseReference

    name, indexer <| (getElements [ name ] >> (List.map (getReference)))

let codeableConcept name =
    indexer <| (getElements [ name; "coding" ] >> (List.map (getSystemCode)))


let getStrings path (elt: JSON.IJsonElement) =
    elt.GetStrings path |> List.map String

let indexElementStrings elementPath stringPath =
    indexer <| (getElements elementPath >> (List.collect (getStrings stringPath)))

let indexElementString elementPath stringPath =
    indexer <| (getElements elementPath >> (List.map (getString stringPath)))

let indexBool path =
    {
        Indexer =
            fun elt ->
                [
                    (elt.GetString path) = "true" |> Bool
                ]
    }

let indexTrueOrDateExists path =
    let pathBool = [ (path + "Boolean") ]
    let pathDate = [ (path + "DateTime") ]

    {
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
        | Some system -> List.filter (fun (e: JSON.IJsonElement) -> (e.GetString [ "system" ]) = system)

    {
        Indexer =
            fun elt ->
                elt.GetElements path
                |> filter
                |> List.map (fun e -> e.GetString [ "value" ] |> String)
    }

let indexAddress path =
    {
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
                     @ (e.GetStrings [ "line" ])))
                |> List.map String
    }

let humanName path =
    {
        Indexer =
            fun elt ->
                elt.GetElements path
                |> List.collect (fun e ->
                    ([
                        e.GetString [ "text" ]
                        e.GetString [ "family" ]
                     ]
                     @ (e.GetStrings [ "given" ])))
                |> List.map String
    }


let parameters =
    [
        "ALL", [ indexId ]

        "Patient",
        [
            identifier

            "active", indexBool [ "active" ]
            "birthdate", indexString [ "birthDate" ]
            "gender", indexString [ "gender" ]
            "death-date", indexDateTime [ "deceasedDateTime" ]
            "deceased", indexTrueOrDateExists "deceased"

            "email", contactPoints [ "telecom" ] (Some "email")
            "phone", contactPoints [ "telecom" ] (Some "phone")
            "telecom", contactPoints [ "telecom" ] None
            "address", indexAddress [ "address" ]

            "given", indexStrings [ "name"; "given" ]
            "family", indexStrings [ "name"; "family" ]
            "name", humanName [ "name" ]
        ]

        "Condition",
        [
            identifier

            "code", codeableConcept "code"
            "category", codeableConcept "category"
            "verification-status", codeableConcept "verificationStatus"
            "clinical-status", codeableConcept "clinicalStatus"

            reference "encounter"
            reference "subject"
        ]
    ]

type ResourceTypeOrALL = string
type ParameterName = string
type ParametersMap = Map<ResourceTypeOrALL, list<ParameterName * SearchParameter>>

let defaultParametersMap: ParametersMap = parameters |> Map.ofList

let deleteIndexForVersion (versionId: string) =
    Delete
        {
            Table = Table.Idx
            Where =
                [
                    {
                        Column = "versionId"
                        Condition = Equal(StringValue versionId)
                    }
                ]
        }

let indexResource (paramsMap: ParametersMap) (resource: JSON.IJsonElement) (_type: string) (meta: JSON.MetaInfo) =

    let paramsForType =
        match Map.tryFind _type paramsMap with
        | Some list -> list
        | None -> []

    let paramsForAll = (Map.find "ALL" paramsMap)

    let allRows =
        (paramsForAll @ paramsForType)
        |> List.map (fun (name, sp) -> name, sp.Indexer resource)

    let versionId = box meta.VersionId

    let sql =
        Insert
            {
                Table = Table.Idx
                Columns =
                    [
                        "name"
                        "value"
                        "system"
                        "versionId"
                    ]
                Values =
                    [
                        for name, rows in allRows do
                            let indexName = _type + "." + name
                            let boxedName = box indexName

                            let uniqueRows = rows |> Set.ofList |> Set.toArray

                            for indexRow in uniqueRows do
                                let (v, sys) = indexRow.ToValueAndSystem()

                                if v <> null then
                                    [ boxedName; v; sys; versionId ]

                    ]
                Returning = []
            }

    sql
