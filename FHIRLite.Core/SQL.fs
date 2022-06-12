module FHIRLite.Core.SQL

open FHIRLite.Core.Types

module Table =
    type T = private T of string
    let toString (T t) = t

    let Versions = T "versions"
    let indexes = T "indexes"
    let Sequences = T "sequences"

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
    | InSubquery of Select

and WhereCondition =
    {
        Column: string
        Condition: Condition
    }

and Select =
    {
        Columns: string list
        From: Table.T
        Where: WhereCondition list
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
    | Savepoint of string
    | SavepointRelease of string
    | SavepointRollback of string
    | TransactionBeginImmediate
    | TransactionCommit


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

    let private columnEqual column _type name value =
        [
            nameColumn _type name
            {
                Column = column
                Condition = Equal(StringValue value)
            }
        ]

    let valueEqual = columnEqual "value"
    let systemEqual = columnEqual "system"

    let _id (id: TypeId) =
        valueEqual id.Type "_id" id.Id

    let _type (_type: string) =
        [ nameColumn _type "_id" ]

let indexSubquery condition =
    InSubquery
        {
            Columns = [ "versionId" ]
            From = Table.indexes
            Where = condition
        }


let indexQuery conditions =
    Select
        {
            Columns = [ "versionId" ]
            From = Table.indexes
            Where = conditions
        }

let readVersionsViaIndex columns conditions =
    Select
        {
            Columns = columns
            From = Table.Versions
            Where =
                [
                    for condition in conditions do
                        {
                            Column = "versionId"
                            Condition = indexSubquery condition
                        }
                ]
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
    let deleted = if json.Length = 0 then 1 else 0

    let sql =
        Insert
            {
                Table = Table.Versions
                Columns = [ "versionId"; "type"; "id"; "lastUpdated"; "deleted"; "json" ]
                Values =
                    [
                        [ meta.VersionId; id.Type; id.Id; meta.LastUpdated; deleted; json ]

                    ]
                Returning = []
            }

    sql
