module FHIRLite.Core.Sqlite

open FHIRLite.Core.SQL

let schema =
    """

CREATE TABLE versions (
    versionId   INTEGER PRIMARY KEY,
    type        TEXT NOT NULL,
    id          INTEGER NOT NULL,
    lastUpdated TEXT NOT NULL,
    json        TEXT NOT NULL
);
CREATE INDEX version_history_type_id ON versions (type, id);
CREATE INDEX version_history_type    ON versions (type, lastUpdated);
CREATE INDEX version_history         ON versions (lastUpdated);

CREATE TABLE idx (
    name TEXT NOT NULL,
    
    value BLOB NOT NULL, -- includes numbers, codes, values, references
    system BLOB, -- includes system field for codes (optional)

    versionId INTEGER NOT NULL
);
CREATE INDEX index_value ON idx (name, value, versionId); -- versionId included for cover
CREATE INDEX index_system_value ON idx (name, system, value, versionId) WHERE system IS NOT NULL;
CREATE INDEX index_versionId ON idx (versionId); -- used to delete index entries


CREATE TABLE sequences (
    name  TEXT PRIMARY KEY,
    value INTEGER NOT NULL
) WITHOUT ROWID;


"""

let GenerateSQL (statement: Statement) =

    let paramName i = $"$p%d{i}"

    let parameters = ResizeArray<obj>()

    let newParam (v: obj) =
        parameters.Add(v)
        paramName (parameters.Count - 1)

    let rec toSQL (st: Statement) =

        let convertWhere (where: WhereCondition list) =
            where
            |> List.map (fun cond ->
                cond.Column
                + (match cond.Condition with
                   | Equal v -> $" = {newParam <| valueToObj v}"
                   | InSubquery x -> $" IN ({toSQL <| Select x})"))
            |> String.concat " AND "


        let generateReturning returning =
            match returning with
            | [] -> ""
            | list -> "RETURNING " + (String.concat "," list)


        match st with
        | Select select ->
            let cols = select.Columns |> String.concat ", "

            $"SELECT {cols} FROM {Table.toString select.From} WHERE {convertWhere select.Where}"

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

        | Delete delete -> $"DELETE FROM {Table.toString delete.Table} WHERE {convertWhere delete.Where}"

        | Update update ->
            let set =
                update.Update
                |> List.map (fun ud ->
                    match ud.Value with
                    | Value value -> $"{ud.Column} = {newParam <| valueToObj value}"
                    | Increment inc -> $"%s{ud.Column} = %s{ud.Column} + %d{inc}")
                |> String.concat ","

            $"UPDATE %s{Table.toString update.Table} SET %s{set} WHERE %s{convertWhere update.Where} %s{generateReturning update.Returning}"

    let sql = toSQL statement

    let parametersList =
        parameters |> Seq.mapi (fun i v -> paramName i, v) |> Seq.toList

    {
        SQL = sql
        Parameters = parametersList
    }
