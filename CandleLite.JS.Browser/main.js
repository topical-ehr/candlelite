import "./style.css";

import { CandleLiteServer } from "./candlelite_js/CandleLite.Core/Server";
import { defaultParametersMap } from "./candlelite_js/CandleLite.Core/SearchParameters";
import { JsJSON } from "./candlelite_js/JsJSON";
import { JsSQLiteImpl } from "./candlelite_js/JsSQLite";

document.querySelector("#app").innerHTML = `
  <div>
    <h1>Hello CandleLite!</h1>
    <div id="logs" />
  </div>
`;
function log(str) {
    const elt = document.createElement("pre");
    elt.textContent = str;

    document.getElementById("logs").appendChild(elt);
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
    const db = new sqlite3.oo1.DB("/mydb.sqlite3", "ct");
    const dbImpl = new JsSQLiteImpl(db);

    const config = {
        SearchParameters: defaultParametersMap,
        CurrentDateTime: new Date(),
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
    let sampleData = await (
        await fetch("/fhir-sample-data/Aaron697_Stiedemann542_small.json")
    ).text();
    resp = server.HandleRequest(
        "POST",
        "/fhir/",
        "/fhir/",
        sampleData,
        getHeader,
        setHeader
    );
    log("Load sample data: " + resp.BodyString);
});
