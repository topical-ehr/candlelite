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

type BundleEntry =
    {
        FullUrl: string option
        Resource: JSON.IJsonElement option
        Request: BundleRequest option
        Response: BundleResponse option
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
