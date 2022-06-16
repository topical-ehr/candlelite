module Requests

open Utils
open Expecto
open Hl7.Fhir.Rest
open Hl7.Fhir.Model

let private splitLeft (text: string) (separator: string) =
    match text.IndexOf(separator) with
    | -1 -> text, null
    | pos ->
        let key = text.Substring(0, pos)
        let value = text.Substring(pos + 1)
        key, value

let private expectedBundleResponse requestType =
    match requestType with
    | Bundle.BundleType.Batch -> Bundle.BundleType.BatchResponse
    | Bundle.BundleType.Transaction -> Bundle.BundleType.TransactionResponse
    | _ -> failwith "unexpected requestType"


module History =
    let ViaGET (fhir: FhirClient) (resource: string) (id: string) =
        fhir.History(sprintf "%s/%s" resource id)

    let private ViaBundle (bundleType: Bundle.BundleType) (fhir: FhirClient) (resource: string) (id: string) =
        let b =
            Bundle(
                Type = N bundleType,
                Entry = L [
                    Bundle.EntryComponent(
                        Request = Bundle.RequestComponent(
                            Url =  sprintf "%s/%s/_history" resource id,
                            Method = N Bundle.HTTPVerb.GET
                        )
                    )
                ]
        )
        let result = fhir.Transaction b
        Expect.equal result.Type (N <| expectedBundleResponse bundleType) "result bundle type"
        Expect.equal result.Entry.Count 1 "result bundle count"
        result.Entry.[0].Resource :?> Bundle

    let ViaBatch (fhir:FhirClient) = ViaBundle Bundle.BundleType.Batch fhir
    let ViaTransaction (fhir:FhirClient) = ViaBundle Bundle.BundleType.Transaction fhir



module GetResource =
    let private makeUrl resource id vid =
        match vid with
        | None -> sprintf "%s/%s" resource id
        | Some vid -> sprintf "%s/%s/_history/%s" resource id vid

    let ViaGET (fhir: FhirClient) (resource: string) (id: string) (vid: string option) =
        fhir.Read(makeUrl resource id vid)

    let private ViaBundle (bundleType: Bundle.BundleType) (fhir: FhirClient) (resource: string) (id: string) (vid: string option) : Resource =
        let b =
            Bundle(
                Type = N bundleType,
                Entry = L [
                    Bundle.EntryComponent(
                        Request = Bundle.RequestComponent(
                            Url =  makeUrl resource id vid,
                            Method = N Bundle.HTTPVerb.GET
                        )
                    )
                ]
        )
        
        let result = fhir.Transaction b
        Expect.equal result.Type (N <| expectedBundleResponse bundleType) "result bundle type"
        Expect.equal result.Entry.Count 1 "result bundle count"
        result.Entry.[0].Resource

    let ViaBatch (fhir:FhirClient) = ViaBundle Bundle.BundleType.Batch fhir
    let ViaTransaction (fhir:FhirClient) = ViaBundle Bundle.BundleType.Transaction fhir

module Search =

    let ViaGET (fhir: FhirClient) (resource: string) (criteria: string list) =
        fhir.Search(resource, criteria |> List.toArray)

    let private ViaBundle (bundleType: Bundle.BundleType) (fhir: FhirClient) (resource: string) (criteria: string list) =
        let searchParams = SearchParams()
        criteria |> List.iter (fun c -> searchParams.Add(splitLeft c "=") |> ignore)
        let url = RestUrl(fhir.Endpoint)
        url.AddPath resource |> ignore
        url.AddParams (searchParams.ToUriParamList()) |> ignore
        let b =
            Bundle(
                Type = N bundleType,
                Entry = L [
                    Bundle.EntryComponent(
                        Request = Bundle.RequestComponent(
                            Url =  resource + url.Uri.Query,
                            Method = N Bundle.HTTPVerb.GET
                        )
                    )
                ]
        )
       
        let result = fhir.Transaction b
        Expect.equal result.Type (N <| expectedBundleResponse bundleType) "result bundle type"
        Expect.equal result.Entry.Count 1 "result bundle count"
        result.Entry.[0].Resource :?> Bundle
    
    let ViaBatch (fhir:FhirClient) = ViaBundle Bundle.BundleType.Batch fhir
    let ViaTransaction (fhir:FhirClient) = ViaBundle Bundle.BundleType.Transaction fhir

