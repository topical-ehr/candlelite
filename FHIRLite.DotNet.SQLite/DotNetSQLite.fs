module FHIRLite.DotNet.SQLite

open Microsoft.Data.Sqlite

open FHIRLite.Core
open FHIRLite.Core.Server

type DotNetSQLiteImpl(connectionString: string) =

    let conn = new SqliteConnection(connectionString)

    let createDB () =

        // check if have tables
        let cmd = conn.CreateCommand()
        cmd.CommandText <- "PRAGMA table_info('versions');"
        let reader = cmd.ExecuteReader()

        if not reader.HasRows then
            // execute schema creation commands
            let cmd = conn.CreateCommand()
            cmd.CommandText <- Sqlite.schema
            cmd.ExecuteNonQuery() |> ignore

    do conn.Open()
    do createDB ()

    static member InMemory() =
        let cs = "Data Source=in_memory_for_process;Mode=Memory;Cache=Shared"
        DotNetSQLiteImpl(cs)

    static member UseFile(filename: string) =
        let cs = $"Data Source=%s{filename}"
        DotNetSQLiteImpl(cs)

    interface IFHIRLiteDB with

        member this.RunSQL(statement: SQL.Statement) : seq<obj array> =
            seq {
                let sql = Sqlite.GenerateSQL statement

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
                    printfn "  %s -> %A" name withDbNull

                use reader = cmd.ExecuteReader()
                printfn "  rows=%A affected=%d" reader.HasRows reader.RecordsAffected

                let values = Array.create reader.FieldCount null

                while reader.Read() do
                    reader.GetValues values |> ignore
                    printfn "  row: %A" values
                    yield values
            }
