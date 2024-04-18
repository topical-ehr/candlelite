module ConcurrencyStress

open System
open Expecto
open Hl7.Fhir.Model
open Hl7.Fhir.Rest
open Hl7.Fhir.Serialization
open Utils

type Task = System.Threading.Tasks.Task

type OrganizationsFixture(fhir: FhirClient) =
    let autoDelete = new FhirAutoDelete(fhir)
    let create = autoDelete.Create

    let createOrg num =
        Organization(
            Identifier = L [ Identifier("http://example.org/1", string num)],
            Active = N true
        ) |> create

    let orgs = [1..20] |> List.map (createOrg)

    interface IDisposable with member __.Dispose() = autoDelete.DeleteAll()
    
    member val Orgs = orgs |> List.toArray



let transactionTests = [
    sprintf "concurrent transaction / update",
        fun (fhir: FhirClient) () ->

            use organisations = new OrganizationsFixture(fhir)
            let org1 = organisations.Orgs.[1]

            for i in [0..100] do
                printfn "%d" i

                let modifiedForTransaction = org1.DeepCopy() :?> Organization
                modifiedForTransaction.Name <- sprintf "modified in transaction %d" i

                let modifiedForUpdate = org1.DeepCopy() :?> Organization
                modifiedForUpdate.Name <- sprintf "modified in update %d" i

                let transaction =
                    Bundle(
                        Type = N Bundle.BundleType.Transaction,
                        Entry = L [
                            Bundle.EntryComponent(
                                Request = Bundle.RequestComponent(
                                    Method = N Bundle.HTTPVerb.PUT,
                                    Url =  "Organization/" + org1.Id
                                ),
                                Resource = modifiedForTransaction
                            )
                        ]
                    )

                let tasks = [|
                    fhir.UpdateAsync(modifiedForUpdate, versionAware = false) :> Task
                    fhir.TransactionAsync(transaction) :> Task
                |]

                // let updateResp = fhir.UpdateAsync(modifiedForUpdate, versionAware = false)
                // System.Threading.Tasks.Task.WaitAll(updateResp)
                // System.Threading.Thread.Sleep(2000)
                // let transactionResp = fhir.TransactionAsync(transaction)

                let isHttp409 (e: exn )=
                    match e with
                    | :? FhirOperationException as e when e.Status = Net.HttpStatusCode.Conflict ->
                        printfn "   HTTP Conflict (as expected)"
                        true
                    | _ -> false

                try
                    System.Threading.Tasks.Task.WaitAll (tasks)
                with
                | :? AggregateException as e ->
                    let inner = Seq.exactlyOne e.InnerExceptions
                    if not <| isHttp409 inner then reraise()

                | :? FhirOperationException as e ->
                    if not <| isHttp409 e then reraise()
                | e ->
                    printfn "  EXCEPTION: %A" e
                    reraise()
                ()


    ]


// [<Tests>]
let transactionsTest =
    let fhir = ServerRunners.FhirServer.Current.FhirClient()
    let tests = [
        testParam (fhir) (transactionTests |> List.toSeq )
    ]
    testList "Concurrency Stress" (List.collect id (tests |> List.map Seq.toList))
