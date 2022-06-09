module FHIRLite.Core.Bundle

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
        Resource: JSON.IJsonElement
        Request: BundleRequest option
        Response: BundleResponse option
    }

type Bundle =
    {
        ResourceType: string
        Type: string
        Total: int
        Timestamp: string
        Link: BundleLink array
        Entry: BundleEntry array
    }

module BundleType =
    let SearchSet = "searchset"
