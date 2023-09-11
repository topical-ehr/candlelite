namespace CandleLite.JS.SQLite

open CandleLite.Core
open CandleLite.Core.Server

open Fable.Core.JsInterop
open Fable.Core

type ISQLite3CApi =
    abstract sqlite3_libversion: unit -> string
    abstract sqlite3_sourceid: unit -> string

type ISQLite3 =
    abstract capi: ISQLite3CApi
    abstract oo1: obj

module Object =
    [<Emit("Object.fromEntries($0)")>]
    let fromEntries (entries: (string * obj) array): obj = jsNative


/// Implements ICandleLiteDB for SQLite's JS/WASM API
/// Compiled to JS using Fable
/// 
/// See https://sqlite.org/wasm/doc/ckout/index.md
type JsSQLiteImpl(db: obj, sqlite3: ISQLite3) =

    let log = LMLogger.Logger()

    do log.Info("Opening sqlite db")

    let createDB () =
        // check if have tables
        let resultRows = [||]
        db?exec(createObj [
            "sql" ==> "PRAGMA table_info('versions');"
            "resultRows" ==> resultRows
        ])

        if resultRows.Length = 0 then
            // execute schema creation commands
            log.Debug("creating schema")
            db?exec(Sqlite.schema) |> ignore

    do createDB ()

    interface ICandleLiteDB with

        member this.RunSqlLazily(statement: SQL.Statement) : seq<obj array> =

            let sql = Sqlite.GenerateSQL statement
            let _params = sql.Parameters |> Array.ofList |> Object.fromEntries

            let resultRows = [||]

            try
                db?exec(createObj [
                    "sql" ==> sql.SQL
                    "rowMode" ==> "array"
                    "bind" ==> _params
                    "resultRows" ==> resultRows
                ])
                log.Debug("query results", [
                    "sql" => sql.SQL
                    "params" => (sql.Parameters |> List.toArray)
                    "rows" => resultRows
                ])
            with
            | exn ->
                log.Debug("query error", [
                    "sql" => sql
                    "error" => exn
                ])
                JS.debugger()

            resultRows

        member this.RunSql(statement: SQL.Statement) : list<obj array> =
            (this :> ICandleLiteDB).RunSqlLazily statement |> Seq.toList