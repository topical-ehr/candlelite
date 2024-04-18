module CandleLite.BundleTests

open System
open System.Text.Json.Nodes
open Xunit

open CandleLite.Core
open CandleLite.DotNet.JsonViaJsonNode

let example_transaction =
    """
{
  "resourceType": "Bundle",
  "id": "bundle-transaction",
  "meta": {
    "lastUpdated": "2014-08-18T01:43:30Z"
  },
  "type": "transaction",
  "entry": [
    {
      "fullUrl": "urn:uuid:61ebe359-bfdc-4613-8bf2-c5e300945f0a",
      "resource": {
        "resourceType": "Patient",
        "text": {
          "status": "generated",
          "div": "<div xmlns=\"http://www.w3.org/1999/xhtml\">Some narrative</div>"
        },
        "active": true,
        "name": [
          {
            "use": "official",
            "family": "Chalmers",
            "given": [
              "Peter",
              "James"
            ]
          }
        ],
        "gender": "male",
        "birthDate": "1974-12-25"
      },
      "request": {
        "method": "POST",
        "url": "Patient"
      }
    },
    {
      "fullUrl": "urn:uuid:88f151c0-a954-468a-88bd-5ae15c08e059",
      "resource": {
        "resourceType": "Patient",
        "text": {
          "status": "generated",
          "div": "<div xmlns=\"http://www.w3.org/1999/xhtml\">Some narrative</div>"
        },
        "identifier": [
          {
            "system": "http:/example.org/fhir/ids",
            "value": "234234"
          }
        ],
        "active": true,
        "name": [
          {
            "use": "official",
            "family": "Chalmers",
            "given": [
              "Peter",
              "James"
            ]
          }
        ],
        "gender": "male",
        "birthDate": "1974-12-25"
      },
      "request": {
        "method": "POST",
        "url": "Patient",
        "ifNoneExist": "identifier=http:/example.org/fhir/ids|234234"
      }
    },
    {
      "fullUrl": "http://example.org/fhir/Patient/123",
      "resource": {
        "resourceType": "Patient",
        "id": "123",
        "text": {
          "status": "generated",
          "div": "<div xmlns=\"http://www.w3.org/1999/xhtml\">Some narrative</div>"
        },
        "active": true,
        "name": [
          {
            "use": "official",
            "family": "Chalmers",
            "given": [
              "Peter",
              "James"
            ]
          }
        ],
        "gender": "male",
        "birthDate": "1974-12-25"
      },
      "request": {
        "method": "PUT",
        "url": "Patient/123"
      }
    },
    {
      "fullUrl": "urn:uuid:74891afc-ed52-42a2-bcd7-f13d9b60f096",
      "resource": {
        "resourceType": "Patient",
        "text": {
          "status": "generated",
          "div": "<div xmlns=\"http://www.w3.org/1999/xhtml\">Some narrative</div>"
        },
        "identifier": [
          {
            "system": "http:/example.org/fhir/ids",
            "value": "456456"
          }
        ],
        "active": true,
        "name": [
          {
            "use": "official",
            "family": "Chalmers",
            "given": [
              "Peter",
              "James"
            ]
          }
        ],
        "gender": "male",
        "birthDate": "1974-12-25"
      },
      "request": {
        "method": "PUT",
        "url": "Patient?identifier=http:/example.org/fhir/ids|456456"
      }
    },
    {
      "fullUrl": "http://example.org/fhir/Patient/123a",
      "resource": {
        "resourceType": "Patient",
        "id": "123a",
        "text": {
          "status": "generated",
          "div": "<div xmlns=\"http://www.w3.org/1999/xhtml\">Some narrative</div>"
        },
        "active": true,
        "name": [
          {
            "use": "official",
            "family": "Chalmers",
            "given": [
              "Peter",
              "James"
            ]
          }
        ],
        "gender": "male",
        "birthDate": "1974-12-25"
      },
      "request": {
        "method": "PUT",
        "url": "Patient/123a",
        "ifMatch": "W/\"2\""
      }
    },
    {
      "request": {
        "method": "DELETE",
        "url": "Patient/234"
      }
    },
    {
      "request": {
        "method": "DELETE",
        "url": "Patient?identifier=123456"
      }
    },
    {
      "fullUrl": "urn:uuid:79378cb8-8f58-48e8-a5e8-60ac2755b674",
      "resource": {
        "resourceType": "Parameters",
        "parameter": [
          {
            "name": "coding",
            "valueCoding": {
              "system": "http://loinc.org",
              "code": "1963-8"
            }
          }
        ]
      },
      "request": {
        "method": "POST",
        "url": "ValueSet/$lookup"
      }
    },
    {
      "request": {
        "method": "GET",
        "url": "Patient?name=peter"
      }
    },
    {
      "request": {
        "method": "GET",
        "url": "Patient/12334",
        "ifNoneMatch": "W/\"4\"",
        "ifModifiedSince": "2015-08-31T08:14:33+10:00"
      }
    }
  ]
}
"""

let jsonImpl = DotNetJSON() :> Server.ICandleLiteJSON

let checkBundle (bundle: Bundle.Bundle) =
    Assert.Equal("transaction", bundle.Type)
    Assert.Equal("2015-08-31T08:14:33+10:00", bundle.Entry.Value[9].Request.Value.IfModifiedSince.Value)
    Assert.Equal(true, bundle.Entry.Value[9].Request.Value.IfMatch.IsNone)

[<Fact>]
let ``Parse bundle`` () =
    // check deserialisation from string
    example_transaction |> jsonImpl.ParseJSON |> jsonImpl.ParseBundle |> checkBundle

    // check deserialisation from JsonNode
    let node = JsonNode.Parse example_transaction |> JsonViaJsonNode
    jsonImpl.ParseBundle node |> checkBundle
