module CandleLite.Core.Search

open CandleLite.Core.Indexes
open CandleLite.Core.Types
open CandleLite.Core.SQL
open CandleLite.Core.URL

module Helpers =
    let addTypeClauseIfNeeded _type (conditions: SQL.WhereCondition list list) =
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


let raiseOO httpStatus code msg =
    let oo = operationOutcome Error code msg
    raise <| OperationOutcomeException(httpStatus, oo)

let conditionsForParam
    (resourceType: string)
    (paramType: Type)
    (p: FhirParameter)
    =
    match paramType with
    | Type.Reference -> 
        IndexConditions.valueEqual resourceType p.Name p.Value

    | Type.String -> 
        IndexConditions.valueEqual resourceType p.Name (p.Value.ToLower())

    | Type.Token ->
        match p.Value.Split("|") with
        | [| code |] ->
            IndexConditions.valueEqual resourceType p.Name code

        | [| system; code |] ->
            let systemCond = IndexConditions.systemEqual resourceType p.Name system
            let valueCond =
                if code.Length > 0 then
                    IndexConditions.valueEqual resourceType p.Name code
                else
                    []

            valueCond @ systemCond

        | _ -> raiseOO 400 OperationOutcomeCodes.Value (sprintf "invalid token parameter for %s/%s" resourceType p.Name)

    | _ -> raiseOO 404 Not_Supported (sprintf "search parameter type %A not supported (%s/%s)" paramType resourceType p.Name)


let conditionsFromUrl
    (paramsMap: ParametersMap)
    (_type: string)
    (parameters: FhirParameter array)
    =

    let paramsForType =
        match Map.tryFind _type paramsMap with
        | Some list -> list
        | None -> []
        
    let conditions =
        [
            for p in parameters do
                let searchParam: SearchParameter =
                    match List.tryFind (fun (name,_) -> name = p.Name) paramsForType with
                    | Some x -> snd x
                    | None -> raiseOO 404 Not_Supported (sprintf "search parameter not supported (%s/%s)" _type p.Name)

                conditionsForParam _type searchParam.Type p
        ]
        |> Helpers.addTypeClauseIfNeeded _type
    
    conditions