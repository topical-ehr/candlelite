# Candlelite

Candlelite is a FHIR server written in F# that can run as a typical ASP.NET HTTP server, and also be compiled to JavaScript using [Fable](https://fable.io/).

SQLite is used for the database - and it can [also run in a modern web browser](https://sqlite.org/wasm/doc/trunk/index.md) thanks to WebAssembly and OPFS.

## Aims

* Ease of deployment & operation for research purposes - not for clinical use. When run inside browsers the database is serverless and fully isoalated. The single-file approach of SQLite makes it easy to save/restore/share database.
* Lightweight memory & CPU footprint.

## Features

It has been built to power the web-based [Topical-EHR prototype](https://github.com/topical-ehr/topical-ehr) and has fairly basic support for FHIR features.
