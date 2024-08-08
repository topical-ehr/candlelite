module CandleLite.DotNet.SQLite

open Microsoft.Data.Sqlite

open CandleLite.Core
open CandleLite.Core.Server

let inline (=>) (name: string) (value) = struct (name, box value)

type DotNetSQLiteImpl(connectionString: string) =
    let log = LMLogger.Logger()

    let connect() =
        let conn = new SqliteConnection(connectionString)
        conn.Open()
        conn

    let runNonQuery sql =
        log.Trace("executing commands", [
            "sql" => sql
        ])
        use conn = connect()
        let cmd = conn.CreateCommand()
        cmd.CommandText <- sql
        cmd.ExecuteNonQuery() |> ignore

    let createDB () =

        // check if have tables
        use conn = connect()
        let cmd = conn.CreateCommand()
        cmd.CommandText <- "PRAGMA table_info('versions');"
        let reader = cmd.ExecuteReader()

        if not reader.HasRows then
            // execute schema creation commands
            log.Info("creating schema")
            runNonQuery Sqlite.schema

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

    interface ICandleLiteDB with

        member this.RunSqlLazily(statement: SQL.Statement) : seq<obj array> =
            seq {
                let sql = Sqlite.GenerateSQL statement

                // TODO: cache the command for better performance
                // https://www.bricelam.net/2017/07/20/sqlite-bulk-insert.html

                use conn = connect()
                use cmd = conn.CreateCommand()
                cmd.CommandText <- sql.SQL

                for name, value in sql.Parameters do
                    let withDbNull =
                        if value <> null then
                            value
                        else
                            System.DBNull.Value

                    cmd.Parameters.AddWithValue(name, withDbNull) |> ignore
                    // printfn "  %s -> %A" name withDbNull

                use reader = cmd.ExecuteReader()
                log.Trace("executing query", [
                    "sql" => sql.SQL
                    "parameters" => sql.Parameters
                    "rows_affected" => reader.RecordsAffected
                    "has_rows" => reader.HasRows
                ])

                while reader.Read() do
                    let values = Array.create reader.FieldCount null
                    reader.GetValues values |> ignore
                    // printfn "  row: %A" values
                    yield values
            }

        member this.RunSql(statement: SQL.Statement) : list<obj array> =
            (this :> ICandleLiteDB).RunSqlLazily statement |> Seq.toList