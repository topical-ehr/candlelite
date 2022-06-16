module FHIRLite.DotNet.SQLite

open Microsoft.Data.Sqlite

open FHIRLite.Core
open FHIRLite.Core.Server

type DotNetSQLiteImpl(connectionString: string) =

    let conn = new SqliteConnection(connectionString)

    let runNonQuery sql =
        let cmd = conn.CreateCommand()
        cmd.CommandText <- sql
        cmd.ExecuteNonQuery() |> ignore

    let createDB () =

        // check if have tables
        let cmd = conn.CreateCommand()
        cmd.CommandText <- "PRAGMA table_info('versions');"
        let reader = cmd.ExecuteReader()

        if not reader.HasRows then
            // execute schema creation commands
            runNonQuery Sqlite.schema

    do conn.Open()
    do createDB ()

    static member InMemory() =
        let cs = "Data Source=in_memory_for_process;Mode=Memory;Cache=Shared"
        DotNetSQLiteImpl(cs)

    static member UseFile(filename: string) =
        let cs = $"Data Source=%s{filename}"
        DotNetSQLiteImpl(cs)

    member _.GoDangerouslyFast() =
        runNonQuery "PRAGMA synchronous = OFF;"
        runNonQuery "PRAGMA journal_mode = MEMORY;"

    member _.UseWAL() =
        runNonQuery "PRAGMA journal_mode = WAL;"

    interface IFHIRLiteDB with

        member this.RunSQL(statement: SQL.Statement) : seq<obj array> =
            seq {
                let sql = Sqlite.GenerateSQL statement

                // TODO: cache the command for better performance
                // https://www.bricelam.net/2017/07/20/sqlite-bulk-insert.html

                use cmd = conn.CreateCommand()
                cmd.CommandText <- sql.SQL
                printfn "SQL: %s" sql.SQL

                for name, value in sql.Parameters do
                    let withDbNull =
                        if value <> null then
                            value
                        else
                            System.DBNull.Value

                    cmd.Parameters.AddWithValue(name, withDbNull) |> ignore
                //printfn "  %s -> %A" name withDbNull

                use reader = cmd.ExecuteReader()
                //printfn "  rows=%A affected=%d" reader.HasRows reader.RecordsAffected

                while reader.Read() do
                    let values = Array.create reader.FieldCount null
                    reader.GetValues values |> ignore
                    //printfn "  row: %A" values
                    yield values
            }
