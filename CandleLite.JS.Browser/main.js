import './style.css';

import { CandleLiteServer } from "./candlelite_js/CandleLite.Core/Server";
import { defaultParametersMap } from "./candlelite_js/CandleLite.Core/SearchParameters";
import { JsJSON } from "./candlelite_js/JsJSON";
import { JsSQLiteImpl } from "./candlelite_js/JsSQLite";
import DateTime from './candlelite_js/fable_modules/fable-library.4.0.0-theta-018/Date';

document.querySelector('#app').innerHTML = `
  <div>
    <h1>Hello CandleLite!</h1>
    <div id="logs" />
  </div>
`;
function log(str) {
  const elt = document.createElement("div");
  elt.textContent = str;

  document.getElementById("logs").appendChild(elt);
}
log("main.js loaded");

function getHeader() {
  return "";
}
function setHeader() {
}

sqlite3InitModule({
  print: console.log,
  printErr: console.error
}).then(async (sqlite3) => {
  const dbImpl = new JsSQLiteImpl("candlelite.sqlite3", sqlite3);

  const config = {
    SearchParameters: defaultParametersMap,
    CurrentDateTime: new Date(),
  };

  const server = new CandleLiteServer(config, dbImpl, new JsJSON(true));
  log("CandleLite Server created");

  let resp = server.HandleRequest("GET", "/fhir/Patient", "/fhir/", "", getHeader, setHeader);
  console.log(resp);
  log("/Patient: " + resp.BodyString);

  let sampleData = await (await fetch("/fhir-sample-data/Aaron697_Stiedemann542_41166989-975d-4d17-b9de-17f94cb3eec1.json")).text();
  resp = server.HandleRequest("POST", "/fhir/", "/fhir/", sampleData, getHeader, setHeader);
  log("Load sample data: " + resp.BodyString);

});