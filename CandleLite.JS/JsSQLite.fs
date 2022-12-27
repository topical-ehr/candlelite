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
type JsSQLiteImpl(filename: string, sqlite3: ISQLite3) =

    do printfn "SQLite version %s (%s)" (sqlite3.capi.sqlite3_libversion()) (sqlite3.capi.sqlite3_sourceid())

    let db = createNew sqlite3.oo1?DB ("/" + filename, "ct")

    let createDB () =
        // check if have tables
        let resultRows = [||]
        db?exec(createObj [
            "sql" ==> "PRAGMA table_info('versions');"
            "resultRows" ==> resultRows
        ])

        if resultRows.Length = 0 then
            // execute schema creation commands
            db?exec(Sqlite.schema) |> ignore

    do createDB ()

    interface ICandleLiteDB with

        member this.RunSqlLazily(statement: SQL.Statement) : seq<obj array> =

            let sql = Sqlite.GenerateSQL statement
            let _params = sql.Parameters |> Array.ofList |> Object.fromEntries

            printfn "SQL: %s: %A" sql.SQL sql.Parameters

            let resultRows = [||]

            try
                db?exec(createObj [
                    "sql" ==> sql.SQL
                    "rowMode" ==> "array"
                    "bind" ==> _params
                    "resultRows" ==> resultRows
                ])
            with
            | exn ->
                eprintf "SQLite query error: %A" exn
                JS.debugger()

            resultRows

        member this.RunSql(statement: SQL.Statement) : list<obj array> =
            (this :> ICandleLiteDB).RunSqlLazily statement |> Seq.toList