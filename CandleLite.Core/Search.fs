module CandleLite.Core.Search

open System

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

    | Type.StringFuzzy -> 
        IndexConditions.startsWith resourceType p.Name (p.Value.ToLower())

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

type FhirInclude =
    {
        Resource: string
        Parameter: string
    }

type SummaryType = Count

type DateParameter =
    {
        Operator: string
        Date: DateTimeOffset
    }

type ParsedParameters =
    {
        Includes: FhirInclude list
        Conditions: list<list<WhereCondition>>
        LastUpdated: DateParameter option
        Summary: SummaryType option
    }

let parseUrl
    (ParametersMap paramsMap)
    (_type: string)
    (parameters: FhirParameter array)
    =

    let paramsForType = Map.tryFind _type paramsMap |> Option.defaultValue []

    let initialState = {
        Includes = []
        Conditions = []
        LastUpdated = None
        Summary = None
    }

    let results = 
        (initialState, parameters)
        ||> Array.fold (fun state p ->
            if p.Name.StartsWith("_") then

                match p.Name with
                | "_include" ->
                    let parts = p.Value.Split(":")
                    match parts with
                    | [| resource; parameter |] ->
                        let fhirInclude = { Resource = resource; Parameter = parameter }
                        { state with Includes = fhirInclude :: state.Includes }
                    | _ ->
                        raiseOO 400 OperationOutcomeCodes.Value (sprintf "invalid/unsupported _include: %s" p.Value)
                | "_lastUpdated" ->
                    {
                        state
                        with
                            LastUpdated = Some {
                                Operator = p.Value.Substring(0, 2)
                                Date = DateTimeOffset.Parse(p.Value.Substring(2))
                            }
                    }
                | "_summary" ->
                    match p.Value with
                    | "count" -> { state with Summary = Some Count }
                    | _ -> raiseOO 400 OperationOutcomeCodes.Value (sprintf "unsupported _summary: %s" p.Value)
                | name ->
                    raiseOO 400 OperationOutcomeCodes.Value (sprintf "unsupported parameter: %s" name)

            else
                let searchParam: SearchParameter =
                    match paramsForType  |> List.tryFind (fun (name,_) -> name = p.Name) with
                    | Some x -> snd x
                    | None -> raiseOO 404 Not_Supported (sprintf "search parameter not supported (%s/%s)" _type p.Name)

                let conditions = conditionsForParam _type searchParam.Type p
                { state with Conditions = conditions :: state.Conditions }
        ) 

    {
        results
        with
            Conditions = Helpers.addTypeClauseIfNeeded _type results.Conditions
    }


let makeSearchSQL (parameters: ParsedParameters) =

    let addLastUpdatedConditions = 
        match parameters.LastUpdated with
        | None -> id
        | Some param ->
            let date = param.Date
            let operator = param.Operator

            let condition =
                match operator with
                | "gt" -> GreaterThan (date.ToUnixTimeMilliseconds())
                | "lt" -> LessThan (date.ToUnixTimeMilliseconds())
                | _ -> raiseOO 400 OperationOutcomeCodes.Value (sprintf "unsupported _lastUpdated operator: %s" operator)

            fun conditions -> conditions @ [ IndexConditions.lastUpdated condition ]

    SelectWithCTE
        {
            CTEs = [
                yield "searchVersionIds", SelectIntersect [
                    for condition in parameters.Conditions do
                        {
                            Columns = [ "versionId" ]
                            From = Table.indexes
                            Where = condition |> addLastUpdatedConditions
                            Order = []
                        }
                ]
                for (i, {FhirInclude.Resource = resource; Parameter = param}) in Seq.indexed parameters.Includes do
                    (*
                        e.g.
                        include1_ids AS (
                            SELECT
                                substring(value, 0, instr(value, "/"))||"._id" name,
                                substring(value, instr(value, "/")+1) value
                            FROM indexes
                            WHERE name = 'Encounter.subject' and versionId = 1675
                        )
                    *)
                    yield $"include{i}_ids",
                        Select {
                            Columns = [
                                "substring(value, 0, instr(value, '/')) || '._id' AS name"
                                "substring(value, instr(value, '/')+1) AS value"
                            ]
                            From = Table.indexes
                            Where = [
                                {
                                    Column = "name"
                                    Condition = Equal(StringValue(resource + "." + param))
                                }
                                {
                                    Column = "versionId"
                                    Condition = InCTE "searchVersionIds"
                                }
                            ]
                            Order = []
                        }
                    yield $"include{i}_versionIds",
                        Select {
                            Columns = [ "versionId" ]
                            From = Table.Expression $"""indexes a INNER JOIN include{i}_ids b ON a.name=b.name AND a.value=b.value"""
                            Where = []
                            Order = []
                        }

            ]
            Select = SelectUnion [
                {
                    Columns = ["'match'"; "json"; "deleted"]
                    From = Table.Versions
                    Where = [ { Column = "versionId"; Condition = InCTE "searchVersionIds" } ]
                    Order = []
                }
                for i = 0 to (List.length parameters.Includes) - 1 do
                    {
                        Columns = ["'include'"; "json"; "deleted"]
                        From = Table.Versions
                        Where = [ { Column = "versionId"; Condition = InCTE $"include{i}_versionIds" } ]
                        Order = []
                    }
            ]
        }