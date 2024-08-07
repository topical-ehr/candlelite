module CandleLite.Core.SQL

open CandleLite.Core.Types

module Table =
    type T = private T of string
    let toString (T t) = t

    let Versions = T "versions"
    let indexes = T "indexes"
    let Sequences = T "sequences"

    let Expression expr = T expr

// type Table = Table of string
// let versionsTable = Table "versions"

type Value =
    | StringValue of string
    | IntValue of int

let valueToObj =
    function
    | StringValue x -> x :> obj
    | IntValue x -> x :> obj

type UpdateValue =
    | Value of Value
    | Increment of int

type ColumnValue = { Column: string; Value: Value }
type ColumnUpdateValue = { Column: string; Value: UpdateValue }

type Condition =
    | Equal of Value
    | StartsWith of string
    | GreaterThan of int64
    | LessThan of int64
    | InSubquery of Select
    | InCTE of string

and WhereCondition =
    {
        Column: string
        Condition: Condition
    }

and Order =
    {
        Column: string
        Ascending: bool
    }

and Select =
    {
        Columns: string list
        From: Table.T
        Where: WhereCondition list
        Order: Order list
    }


type Insert =
    {
        Table: Table.T
        Columns: string list
        Values: obj list list
        Returning: string list
    }

type Update =
    {
        Table: Table.T
        Update: ColumnUpdateValue list
        Where: WhereCondition list
        Returning: string list
    }

type Delete =
    {
        Table: Table.T
        Where: WhereCondition list
    }

type Statement =
    | Select of Select
    | Insert of Insert
    | Update of Update
    | Delete of Delete
    | SelectWithCTE of SelectWithCTE
    | SelectIntersect of Select list
    | SelectUnion of Select list
    | Savepoint of string
    | SavepointRelease of string
    | SavepointRollback of string
    | TransactionBeginImmediate
    | TransactionCommit
    | TransactionRollback

and SelectWithCTE =
    {
        CTEs: (string * Statement) list
        Select: Statement
    }


type GeneratedSQL =
    {
        SQL: string
        Parameters: (string * obj) list
    }

module IndexConditions =
    let private nameColumn _type name =
        {
            Column = "name"
            Condition = Equal(StringValue(_type + "." + name))
        }

    let private column op col _type name value =
        [
            nameColumn _type name
            {
                Column = col
                Condition = op value 
            }
        ]

    let private stringColumn op col _type name value =
        column op col _type name (StringValue value)

    let valueEqual = stringColumn Equal "value"
    let systemEqual = stringColumn Equal "system"

    let startsWith = column StartsWith "value"

    let _id (id: TypeId) =
        valueEqual id.Type "_id" id.Id

    let _type (_type: string) =
        [ nameColumn _type "_id" ]

    let lastUpdated condition =
        {
            Column = "lastUpdated"
            Condition = condition
        }

let indexSubquery condition =
    InSubquery
        {
            Columns = [ "versionId" ]
            From = Table.indexes
            Where = condition
            Order = []
        }


let indexQuery conditions =
    Select
        {
            Columns = [ "versionId" ]
            From = Table.indexes
            Where = conditions
            Order = []
        }

let readVersionsViaIndex columns conditions =
    SelectWithCTE
        {
            CTEs = [
                "searchVersionIds",
                SelectIntersect [
                    for condition in conditions do
                        {
                            Columns = [ "versionId" ]
                            From = Table.indexes
                            Where = condition
                            Order = []
                        }
                ]
            ]
            Select = Select {
                Columns = columns
                From = Table.Versions
                Where =
                    [
                        {
                            Column = "versionId"
                            Condition = InCTE "searchVersionIds"
                        }
                    ]
                Order = []
            }
        }

let readResourcesViaIndex conditions =
    readVersionsViaIndex [ "json"; "deleted" ] conditions

let readIsDeletedViaIndex conditions =
    readVersionsViaIndex [ "deleted" ] conditions


let readVersion versionId =
    Select
        {
            Columns = [ "json"; "deleted" ]
            From = Table.Versions
            Where =
                [
                    {
                        Column = "versionId"
                        Condition = Equal <| StringValue versionId
                    }
                ]
            Order = []
        }


let readResourceHistory (id: TypeId) =
    Select
        {
            Columns = [ "versionId"; "lastUpdated"; "deleted"; "json" ]
            From = Table.Versions
            Where =
                [
                    {
                        Column = "type"
                        Condition = Equal <| StringValue id.Type
                    }
                    {
                        Column = "id"
                        Condition = Equal <| StringValue id.Id
                    }
                ]
            Order = [{ Column="versionId"; Ascending=true; }]
        }

let updateCounter name =
    // e.g. UPDATE sequences SET value = value + 1 WHERE name = 'test' returning value;

    Update
        {
            Table = Table.Sequences
            Update =
                [
                    {
                        Column = "value"
                        Value = Increment 1
                    }

                ]
            Where =
                [
                    {
                        Column = "name"
                        Condition = Equal(StringValue name)
                    }
                ]
            Returning = [ "value" ]
        }

let insertCounter name =
    Insert
        {
            Table = Table.Sequences
            Columns = [ "name"; "value" ]
            Values = [ [ name; 1 ] ]
            Returning = [ "value" ]
        }

let insertResourceVersion (id: TypeId) (meta: JSON.MetaInfo) (json: string) =
    if json.Length = 0 then
        invalidArg "json" "json is empty"

    Insert
        {
            Table = Table.Versions
            Columns = [ "versionId"; "type"; "id"; "lastUpdated"; "deleted"; "json" ]
            Values =
                [
                    [ meta.VersionId; id.Type; id.Id; meta.LastUpdated; 0; json ]

                ]
            Returning = []
        }

let insertDeletion (id: TypeId) (meta: JSON.MetaInfo) =
    Insert
        {
            Table = Table.Versions
            Columns = [ "versionId"; "type"; "id"; "lastUpdated"; "deleted"; "json" ]
            Values =
                [
                    [ meta.VersionId; id.Type; id.Id; meta.LastUpdated; 1; null ]

                ]
            Returning = []
        }
