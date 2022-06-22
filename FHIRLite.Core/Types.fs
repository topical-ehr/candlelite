module FHIRLite.Core.Types

type TypeId =
    {
        Type: string
        Id: string
    }

    member this.TypeId = $"{this.Type}/{this.Id}"

    static member From (_type: string) (_id: string) =
        { Type = _type; Id = _id }

type TokenValue = { System: string; Code: string }

type OperationOutcomeIssue =
    {
        Severity: string
        Code: string
        Diagnostics: string

    }

type OperationOutcomeSeverity =
    | Fatal
    | Error
    | Warning
    | Information

type OperationOutcomeCodes =
    | Structure
    | Required
    | Value
    | Invariant
    | Not_Supported
    | Duplicate
    | Multiple_Matches
    | Not_Found
    | Deleted
    | Conflict
    | Exception
    | Informational

type OperationOutcome =
    {
        ResourceType: string
        Issue: OperationOutcomeIssue list
    }

let operationOutcome
    (severity: OperationOutcomeSeverity)
    (code: OperationOutcomeCodes)
    diagnosticInfo
    =
    let toCode x =
        (string x).Replace("_", "-").ToLowerInvariant()

    {
        ResourceType = "OperationOutcome"
        Issue =
            [
                {
                    Severity = toCode severity
                    Code = toCode code
                    Diagnostics = diagnosticInfo
                }
            ]
    }


exception OperationOutcomeException of int * OperationOutcome
