module CandleLite.Core.Sqlite

open CandleLite.Core.SQL

let schema =
    """
CREATE TABLE versions (
    versionId   INTEGER PRIMARY KEY,
    type        TEXT NOT NULL,
    id          TEXT NOT NULL,      -- text as can be client-assigned
    lastUpdated TEXT NOT NULL,      -- text to make _history a bit faster
    deleted     TINYINT NOT NULL,
    json        TEXT                -- null when deleted
);
CREATE INDEX version_history_by_type_id ON versions (type, id);
CREATE INDEX version_history_by_type    ON versions (type, lastUpdated);
CREATE INDEX version_history_all        ON versions (lastUpdated);


CREATE TABLE indexes (
    name      TEXT NOT NULL,    -- index name, e.g. Patient._id
    
    value     BLOB NOT NULL,    -- includes numbers, codes, values, references
    system    BLOB,             -- includes system field for codes (optional)
    isRef     TINYINT,          -- whether the value is a reference to another resource

    lastUpdated INTEGER NOT NULL, -- for _lastUpdated searches (unix time in milliseconds)
    id          TEXT  NOT NULL,   -- resource id for chained searches
    versionId   INTEGER NOT NULL  -- version id for getting the latest JSON
);
CREATE INDEX index_value        ON indexes (name, value, versionId); -- versionId included for cover
CREATE INDEX index_system_value ON indexes (name, system, value, versionId) WHERE system IS NOT NULL; -- for searches including the system
CREATE INDEX index_references   ON indexes (value) WHERE isRef = 1; -- to prevent deletion of referenced resources
CREATE INDEX index_versionId    ON indexes (versionId); -- to delete index entries


CREATE TABLE sequences (
    name  TEXT PRIMARY KEY,
    value INTEGER NOT NULL
) WITHOUT ROWID;


"""

let migration_add_lastUpdated_column = """
    ALTER TABLE indexes ADD COLUMN lastUpdated INTEGER NOT NULL DEFAULT 0;
"""

let GenerateSQL (statement: Statement) =

    let paramName i = $"$p%d{i}"

    let parameters = ResizeArray<obj>()

    let newParam (v: obj) =
        parameters.Add(v)
        paramName (parameters.Count - 1)

    let rec toSQL (st: Statement) =

        let convertWhere (where: WhereCondition list) =
            match where with
            | [] -> ""
            | where ->
                let str =
                    where
                    |> List.map (fun cond ->
                        cond.Column
                        + (
                            match cond.Condition with
                            | Equal v -> $" = {newParam <| valueToObj v}"
                            | StartsWith str -> $""" LIKE {(str + "%") |> newParam}"""
                            | GreaterThan num -> $""" > {num |> newParam}"""
                            | LessThan num -> $""" < {num |> newParam}"""
                            | InSubquery x -> $" IN ({toSQL <| Select x})"
                            | InCTE cteName -> $" IN {cteName}"
                        )
                    )
                    |> String.concat " AND "
                "WHERE " + str

        let convertOrderBy (order: Order list) =
            match order with
            | [] -> ""
            | order ->
                let cols =
                    order
                    |> List.map (fun col -> col.Column + (if col.Ascending then "" else " DESC"))
                    |> String.concat ", "
                "ORDER BY " + cols


        let generateReturning returning =
            match returning with
            | [] -> ""
            | list -> "RETURNING " + (String.concat "," list)


        match st with
        | Select select ->
            let cols = select.Columns |> String.concat ", "

            $"SELECT {cols} FROM {Table.toString select.From} {convertWhere select.Where} {convertOrderBy select.Order}"

        | SelectWithCTE selectWithCTE ->

            let ctes =
                selectWithCTE.CTEs
                |> List.map (fun (name, select) -> $"%s{name} AS (%s{toSQL select})")
                |> String.concat ", "
            
            $"WITH {ctes} {toSQL selectWithCTE.Select}"

        | SelectIntersect select ->
            select |> List.map (fun s -> toSQL (Select s)) |> String.concat " INTERSECT "
        | SelectUnion select ->
            select |> List.map (fun s -> toSQL (Select s)) |> String.concat " UNION "

        | Insert insert ->
            let vals =
                [
                    for row in insert.Values do
                        row |> List.map (fun c -> newParam c) |> String.concat ","
                ]
                |> List.map (fun r -> $"({r})")
                |> String.concat ","

            let cols = insert.Columns |> String.concat ", "

            $"INSERT INTO %s{Table.toString insert.Table} (%s{cols}) VALUES %s{vals} %s{generateReturning insert.Returning}"

        | Delete delete ->
            $"DELETE FROM {Table.toString delete.Table} {convertWhere delete.Where}"

        | Update update ->
            let set =
                update.Update
                |> List.map (fun ud ->
                    match ud.Value with
                    | Value value -> $"{ud.Column} = {newParam <| valueToObj value}"
                    | Increment inc -> $"%s{ud.Column} = %s{ud.Column} + %d{inc}"
                )
                |> String.concat ","

            $"UPDATE %s{Table.toString update.Table} SET %s{set} %s{convertWhere update.Where} %s{generateReturning update.Returning}"
        | Savepoint name -> $@"SAVEPOINT ""%s{name}"""
        | SavepointRelease name -> $@"RELEASE SAVEPOINT ""%s{name}"""
        | SavepointRollback name -> $@"ROLLBACK TO SAVEPOINT ""%s{name}"""
        | TransactionBeginImmediate -> "BEGIN IMMEDIATE TRANSACTION"
        | TransactionCommit -> "COMMIT"
        | TransactionRollback -> "ROLLBACK"


    let sql = toSQL statement

    let parametersList = parameters |> Seq.mapi (fun i v -> paramName i, v) |> Seq.toList

    {
        SQL = sql
        Parameters = parametersList
    }
