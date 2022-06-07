module FHIRLite.Core.Search

open FHIRLite.Core.Types
open FHIRLite.Core.SQL

type IndexedValues =
    | Number of decimal
    | Reference of IdValue
    | String of string
    | Bool of bool
    | Token of TokenValue
    member this.ToValueAndSystem() =
        match this with
        | Number n -> box n, None
        | Reference ref -> box ref.Id, Some ref.Type
        | String str -> box str, None
        | Bool b -> box (if b then "true" else "false"), None
        | Token t -> box t.Code, Some t.System


type SearchParameter =
    {
        Indexer: JSON.IJsonElement -> IndexedValues list
    }

let indexer func = { Indexer = func }

let indexString path =
    indexer <| fun elt -> [ elt.GetString path |> String ]

let indexId =
    indexer
    <| fun elt ->
        [
            ([ [ "resourceType" ]; [ "id" ] ] |> List.map elt.GetString |> String.concat "/")
            |> String
        ]

let getElements path (elt: JSON.IJsonElement) =
    elt.GetElements path

let getString path (elt: JSON.IJsonElement) =
    elt.GetString path |> String

let getSystemValue (elt: JSON.IJsonElement) =
    elt.GetString [ "system" ] + "|" + elt.GetString [ "value" ] |> String

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

let indexContactPoints path filterForSystem =
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

let indexHumanName path =
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
        "ALL",
        [
            "_id", indexId
            "_lastUpdated", indexString [ "meta"; "lastUpdated" ]
        ]

        "Patient",
        [
            identifier

            "active", indexBool [ "active" ]
            "birthdate", indexString [ "birthDate" ]
            "gender", indexString [ "gender" ]
            "death-date", indexString [ "deceasedDateTime" ]
            "deceased", indexTrueOrDateExists "deceased"

            "email", indexContactPoints [ "telecom" ] (Some "email")
            "phone", indexContactPoints [ "telecom" ] (Some "phone")
            "telecom", indexContactPoints [ "telecom" ] None
            "address", indexAddress [ "address" ]

            "given",
            indexElementStrings [ "name" ] [
                "given"
            ]
            "family",
            indexElementString [ "name" ] [
                "family"
            ]
            "name", indexHumanName [ "name" ]
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
                            let boxedName = box name

                            for indexRow in rows do
                                let (v, sys) = indexRow.ToValueAndSystem()
                                [ boxedName; v; sys; versionId ]

                    ]
                Returning = []
            }

    sql
