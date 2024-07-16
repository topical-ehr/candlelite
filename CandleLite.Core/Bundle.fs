module CandleLite.Core.Bundle

type BundleLink = { Relation: string; Uri: string }

type BundleRequest =
    {
        Method: string
        Url: string
        IfNoneMatch: string option
        IfModifiedSince: string option
        IfMatch: string option
        IfNoneExist: string option
    }

type BundleResponse =
    {
        Status: string
        Location: string option
        Etag: string option
        LastModified: string option
        Outcome: JSON.IJsonElement option
    }

type BundleSearchMode = Match | Include
type BundleSearch = {
    Mode: BundleSearchMode
}

type BundleEntry =
    {
        FullUrl: string option
        Resource: JSON.IJsonElement option
        Request: BundleRequest option
        Response: BundleResponse option
        Search:  BundleSearch option
    }

type Bundle =
    {
        ResourceType: string
        Type: string
        Total: int option
        Timestamp: string option
        Link: BundleLink array option
        Entry: BundleEntry array option
    }

module BundleType =
    let SearchSet = "searchset"
    let Document = "document"
    let Message = "message"
    let Transaction = "transaction"
    let TransactionResponse = "transaction-response"
    let Batch = "batch"
    let BatchResponse = "batch-response"
    let History = "history"
    let Collection = "collection"
    let SubscriptionNotification = "subscription-notification"

