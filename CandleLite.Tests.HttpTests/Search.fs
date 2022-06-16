module Search

open System
open Expecto
open Hl7.Fhir.Model
open Hl7.Fhir.Rest
open Utils
open ServerRunners

type ObservationsFixture(fhir:FhirClient) =
    let autoDelete = new FhirAutoDelete(fhir)
    let create = autoDelete.Create
    interface IDisposable with member __.Dispose() = autoDelete.DeleteAll()
    
    member val S1C1 =
        Observation(
            Code = CodeableConcept(Coding = L [Coding("http://example.org/1", "1")])
        ) |> create
    member val S1C2 =
        Observation(
            Code = CodeableConcept(Coding = L [Coding("http://example.org/1", "2")])
        ) |> create
    member val S2C1 =
        Observation(
            Code = CodeableConcept(Coding = L [Coding("http://example.org/2", "1")])
        ) |> create
    member val S2C2 =
        Observation(
            Code = CodeableConcept(Coding = L [Coding("http://example.org/2", "2")])
        ) |> create
    member val C1 =
        Observation(
            Code = CodeableConcept(Coding = L [Coding(Code="1")])
        ) |> create
    member val C2 =
        Observation(
            Code = CodeableConcept(Coding = L [Coding(Code="2")])
        ) |> create

let searchTests searchFuncStr = [
    sprintf "token - Observation.code - %s" searchFuncStr,
        fun (fhirClient, searchFunc) () ->
            use obs = new ObservationsFixture(fhirClient)

            let expectObservations query (obs: Observation list) =
                let msg str = String.Join(";", [str] @ query)
                let bundle = searchFunc fhirClient "Observation" query :> Bundle
                match bundle.Total |> Option.ofNullable with
                | Some total -> 
                    Expect.equal total (Seq.length obs) (msg "bundle.Total")
                | None ->
                    ()
                Expect.equal bundle.Entry.Count (Seq.length obs) (msg "bundle.Entry.Count")
                let expectedIds = obs |> List.map (fun r -> r.Id)
                let actualIds = bundle.Entry |> Seq.map (fun r -> (r.Resource :?> Observation).Id)
                Expect.containsAll actualIds expectedIds (msg "expected IDs")

            expectObservations ["code=1"] [obs.S1C1; obs.S2C1; obs.C1]
            expectObservations ["code=2"] [obs.S1C2; obs.S2C2; obs.C2]

            expectObservations ["code=http://example.org/1|1"] [obs.S1C1]
            expectObservations ["code=http://example.org/1|2"] [obs.S1C2]
            expectObservations ["code=http://example.org/2|1"] [obs.S2C1]
            expectObservations ["code=http://example.org/2|2"] [obs.S2C2]

            expectObservations ["code=|1"] [obs.C1]
            expectObservations ["code=|2"] [obs.C2]
        
            expectObservations ["code=http://example.org/1|"] [obs.S1C1; obs.S1C2]
            expectObservations ["code=http://example.org/2|"] [obs.S2C1; obs.S2C2]
    ]

[<Tests>]
let searchTest =
    let fhir = ServerRunners.FhirServer.Current.FhirClient()
    let tests = [
        testParam (fhir, Requests.Search.ViaGET) (searchTests "GET" |> List.toSeq )
        testParam (fhir, Requests.Search.ViaBatch) (searchTests "Batch" |> List.toSeq )
        testParam (fhir, Requests.Search.ViaTransaction) (searchTests "Transaction" |> List.toSeq )
    ]
    testList "Search" (List.collect id (tests |> List.map Seq.toList))
