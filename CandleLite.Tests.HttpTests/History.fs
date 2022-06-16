module History

open System
open System.Text.RegularExpressions
open Expecto
open Hl7.Fhir.Model
open Hl7.Fhir.Rest
open Utils

type CarePlanFixture(fhir: FhirClient) =
    let autoDelete = new FhirAutoDelete(fhir)
    let create = autoDelete.Create

    interface IDisposable with
        member __.Dispose() =
            autoDelete.DeleteAll()

    member val CP1 =
        CarePlan(
            Category = L [ CodeableConcept(Coding = L [ Coding("http://example.org/1", "1") ]) ],
            Status = N RequestStatus.Draft
        )
        |> create

let assertResourcesEqual (r1: #Resource) (r2: #Resource) =
    let removeSource json =
        let re = Regex(@",""source"":""#[a-zA-Z0-9]+""")
        re.Replace(json, "")

    let jsonSerialiser = Hl7.Fhir.Serialization.FhirJsonSerializer()
    let j1 = jsonSerialiser.SerializeToString r1 |> removeSource
    let j2 = jsonSerialiser.SerializeToString r2 |> removeSource

    Expect.equal j1 j2 "assertResourcesEqual"

let historyTests getFuncStr =
    [
        sprintf "create/get/update/history - CarePlan - %s" getFuncStr,
        fun (fhirClient, getFunc, historyFync) () ->
            use fx = new CarePlanFixture(fhirClient)
            let cp1 = fx.CP1

            let getFunc = getFunc fhirClient "CarePlan"
            let historyFync = historyFync fhirClient "CarePlan"

            // read initial version by id
            let cp1_read = (getFunc cp1.Id None :> Resource) :?> CarePlan
            Expect.equal cp1_read.Status cp1.Status "status correct"
            Expect.isTrue cp1.HasVersionId "initial version has version id"

            // read initial version by vid
            let cp1_read_vid = (getFunc cp1.Id (Some cp1.VersionId) :> Resource) :?> CarePlan
            Expect.equal cp1_read_vid.Status cp1.Status "status correct (read by vid)"

            // get history
            let history1 = historyFync cp1.Id :> Bundle
            Expect.equal history1.Entry.Count 1 "history count 1"
            Expect.equal history1.Total (N 1) "history total 1"
            let cp1_from_res = history1.Entry.[0].Resource :?> CarePlan
            Expect.equal cp1_read.Status cp1_from_res.Status "status correct from history"
            Expect.equal cp1_read.VersionId cp1_from_res.VersionId "versionid correct from history"
            Expect.isGreaterThan cp1_read.VersionId.Length 0 "resource has version"

            // update
            let cp2 = cp1.DeepCopy() :?> CarePlan
            cp2.Status <- N RequestStatus.Active
            let cp2_updated = fhirClient.Update(cp2, versionAware = true)
            Expect.equal cp2_updated.Status cp2.Status "status correct after update"
            Expect.equal cp2_updated.Id cp1.Id "id the same"
            Expect.notEqual cp2_updated.Status cp1.Status "status correct after update"
            Expect.isGreaterThan cp2_updated.VersionId.Length 0 "updated resource has version"

            Expect.notEqual
                cp2_updated.VersionId
                cp1_read.VersionId
                "updated resource has different version"

            // get with id
            let cp2_read_id = (getFunc cp1.Id None :> Resource) :?> CarePlan
            Expect.equal cp2_read_id.Id cp1.Id "id the same"
            Expect.equal cp2_read_id.VersionId cp2_updated.VersionId "versionid the same"
            assertResourcesEqual cp2_read_id cp2_updated

            // get with id & vid
            let cp2_read_vid =
                (getFunc cp1.Id (Some cp2_updated.VersionId) :> Resource) :?> CarePlan

            Expect.equal cp2_read_vid.Id cp1.Id "id the same"
            Expect.equal cp2_read_vid.VersionId cp2_updated.VersionId "versionid the same"
            assertResourcesEqual cp2_read_vid cp2_updated


            // history after update
            let history2 = historyFync cp1.Id :> Bundle
            Expect.equal history2.Entry.Count 2 "history count 2"
            Expect.equal history2.Total (N 2) "history total 2"
            let cp1_from_his2 = history2.Entry.[1].Resource :?> CarePlan
            let cp2_from_his2 = history2.Entry.[0].Resource :?> CarePlan
            let his2_vid1 = cp1_from_his2.VersionId
            let his2_vid2 = cp2_from_his2.VersionId
            Expect.equal cp2_read_vid.Status cp2_from_his2.Status "status correct from history"

            Expect.equal
                cp2_read_vid.VersionId
                cp2_from_his2.VersionId
                "versionid correct from history"

            Expect.isGreaterThan cp2_from_his2.VersionId.Length 0 "resource has version"
            assertResourcesEqual cp2_read_vid cp2_from_his2
            assertResourcesEqual cp1_read_vid cp1_from_his2

            // delete
            fhirClient.Delete(cp2_read_vid)

            // history after delete
            let history3 = historyFync cp1.Id :> Bundle
            Expect.equal history3.Entry.Count 3 "history count 3"
            Expect.equal history3.Total (N 3) "history total 3"
            let cp1_from_his3 = history3.Entry.[2].Resource :?> CarePlan
            let cp2_from_his3 = history3.Entry.[1].Resource :?> CarePlan
            let cp3_from_his3 = history3.Entry.[0]
            Expect.equal cp2_read_vid.Status cp2_from_his3.Status "status correct from history"

            Expect.equal
                cp2_read_vid.VersionId
                cp2_from_his3.VersionId
                "versionid correct from history"

            Expect.isGreaterThan cp2_from_his3.VersionId.Length 0 "resource has version"

            assertResourcesEqual cp2_read_vid cp2_from_his3
            assertResourcesEqual cp1_read_vid cp1_from_his3
            // resource is null or only contains meta.lastUpdated
            match history3.Entry.[0].Resource with
            | null -> ()
            | :? CarePlan as resource ->
                Expect.equal resource.Category.Count 0 "DELETEd history resource categories empty"
            | _ -> failwith "??"

            Expect.equal
                history3.Entry.[0].Request.Method
                (N Bundle.HTTPVerb.DELETE)
                "DELETE history method"

            do
                let url = history3.Entry.[0].Request.Url
                let ending = sprintf "CarePlan/%s" cp1_from_his3.Id
                // TODO: need to have version-specific url ???
                Expect.stringContains url ending "DELETE history url"




    ]

[<Tests>]
let historyTest =
    let fhir = ServerRunners.FhirServer.Current.FhirClient()

    let tests =
        [
            testParam
                (fhir, Requests.GetResource.ViaGET, Requests.History.ViaGET)
                (historyTests "GET" |> List.toSeq)
            testParam
                (fhir, Requests.GetResource.ViaBatch, Requests.History.ViaBatch)
                (historyTests "Batch" |> List.toSeq)
            testParam
                (fhir, Requests.GetResource.ViaTransaction, Requests.History.ViaTransaction)
                (historyTests "Transaction" |> List.toSeq)
        ]

    testList "History" (List.collect id (tests |> List.map Seq.toList))
