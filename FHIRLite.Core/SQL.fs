module FHIRLite.Core.SQL

open FHIRLite.Core.Types

module Table =
    type T = private T of string
    let toString (T t) = t

    let Versions = T "versions"
    let Idx = T "idx"
    let Sequences = T "sequences"

// type Table = Table of string
// let versionsTable = Table "versions"

type Column = { Name: string; As: string }

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


type GeneratedSQL =
    {
        SQL: string
        Parameters: (string * obj) list
    }

module IndexConditions =
    let _id (id: IdValue) =
        [
            {
                Column = "name"
                Condition = Equal(StringValue "_id")
            }
            {
                Column = "value"
                Condition = Equal(StringValue id.RefString)
            }
        ]

let indexSubquery conditions =
    InSubquery
        {
            Columns = [ "versionId" ]
            From = Table.Idx
            Where = conditions
        }


let indexQuery conditions =
    Select
        {
            Columns = [ "versionId" ]
            From = Table.Idx
            Where = conditions
        }

let readResourceViaIndex conditions =
    Select
        {
            Columns = [ "json" ]
            From = Table.Versions
            Where =
                [
                    {
                        Column = "versionId"
                        Condition = indexSubquery conditions
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




let insertResourceVersion (id: IdValue) (meta: JSON.MetaInfo) (json: string) =
    let sql =
        Insert
            {
                Table = Table.Versions
                Columns =
                    [
                        "versionId"
                        "type"
                        "id"
                        "lastUpdated"
                        "json"
                    ]
                Values =
                    [
                        [
                            meta.VersionId
                            id.Type
                            id.Id
                            meta.LastUpdated
                            json
                        ]

                    ]
                Returning = []
            }

    sql
