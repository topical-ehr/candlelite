module Utils

open System
open Hl7.Fhir.Model
open Hl7.Fhir.Rest

let N = Nullable
type L<'T> = ResizeArray<'T>

let flatten x =
    Seq.collect id x |> L

let newPlaceholderUrl () =
    "urn:uuid:" + Guid.NewGuid().ToString()

type FhirAutoDelete(fhir: FhirClient) =
    // do fhir.OnBeforeRequest.Add (fun r -> r.RawRequest.Proxy <- null)
    let created = L []

    member __.Create(r: 'R :> Resource) : 'R =
        let c = fhir.Create(r :> Resource)
        created.Add c
        c :?> 'R

    interface IDisposable with
        member __.Dispose() =
            for r in created do
                // printfn "Deleting: %A/%s" r.ResourceType r.Id
                fhir.Delete r
                ()

    member this.DeleteAll() =
        (this :> IDisposable).Dispose()
