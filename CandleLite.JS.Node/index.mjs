import { JsJSON } from "../CandleLite.JS/candlelite_js/JsJSON.js";

import { CandleLiteServer } from "../CandleLite.JS/candlelite_js/CandleLite.Core/Server.js";
import { defaultParametersMap } from "../CandleLite.JS/candlelite_js/CandleLite.Core/SearchParameters.js";
import { JsSQLiteImpl } from "../CandleLite.JS/candlelite_js/JsSQLite.js";

import sqlite3InitModule from "@sqlite.org/sqlite-wasm";

import fs from "node:fs";
import http from "node:http";

function log(str) {
    console.log(str);
}
log("main.js loaded");

function getHeader() {
    return "";
}
function setHeader() {}

sqlite3InitModule({
    print: console.log,
    printErr: console.error,
}).then(async (sqlite3) => {
    const db = new sqlite3.oo1.DB(":memory:", "ct");
    const dbImpl = new JsSQLiteImpl(db);

    const config = {
        SearchParameters: defaultParametersMap,
        CurrentDateTime: () => new Date(),
    };

    const server = new CandleLiteServer(config, dbImpl, new JsJSON(true));
    log("CandleLite Server created");
    server.SetLogDestination("http://localhost:10000/logs/candlelite.js");

    let resp = server.HandleRequest(
        "GET",
        "/fhir/Patient",
        "/fhir/",
        "",
        getHeader,
        setHeader
    );
    console.log(resp);
    log("/Patient: " + resp.BodyString);

    //  let sampleData = await (await fetch("/fhir-sample-data/Aaron697_Stiedemann542_41166989-975d-4d17-b9de-17f94cb3eec1.json")).text();
    let sampleData = fs.readFileSync(
        "../CandleLite.JS.Browser/public/fhir-sample-data/Aaron697_Stiedemann542_small.json",
        "utf8"
    );
    resp = server.HandleRequest(
        "POST",
        "/fhir/",
        "/fhir/",
        sampleData,
        getHeader,
        setHeader
    );
    log("Load sample data: " + resp.BodyString);

    const httpServer = http.createServer((req, res) => {
        if (!req.url?.startsWith("/fhir")) {
            return res.writeHead(404).end("URL should start with /fhir/");
        }

        // read req body
        let body = "";
        req.on("data", (chunk) => {
            body += chunk;
        });

        // handle request
        req.on("end", () => {
            var response = server.HandleRequest(
                req.method,
                req.url,
                "/fhir",
                body,
                (header) => req.headers[header],
                (header, value) => res.setHeader(header, value)
            );

            if (response.Status == 204) {
                return res.writeHead(204).end();
            } else {
                return res
                    .writeHead(response.Status, {
                        "Content-Type": "application/fhir+json",
                    })
                    .end(response.BodyString);
            }
        });
    });

    httpServer.listen(5353);
});
