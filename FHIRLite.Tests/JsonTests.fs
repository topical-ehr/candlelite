module FHIRLite.JsonTests

open System
open System.Text.Json.Nodes
open Xunit

open FHIRLite.Core.JSON
open FHIRLite.DotNet.JsonViaJsonNode

// example from https://www.hl7.org/fhir/patient-example.json.html
let example_patient =
    """
{
  "resourceType": "Patient",
  "id": "example",
  "text": {
    "status": "generated",
    "div": "<div xmlns=\"http://www.w3.org/1999/xhtml\">\n\t\t\t<table>\n\t\t\t\t<tbody>\n\t\t\t\t\t<tr>\n\t\t\t\t\t\t<td>Name</td>\n\t\t\t\t\t\t<td>Peter James \n              <b>Chalmers</b> (&quot;Jim&quot;)\n            </td>\n\t\t\t\t\t</tr>\n\t\t\t\t\t<tr>\n\t\t\t\t\t\t<td>Address</td>\n\t\t\t\t\t\t<td>534 Erewhon, Pleasantville, Vic, 3999</td>\n\t\t\t\t\t</tr>\n\t\t\t\t\t<tr>\n\t\t\t\t\t\t<td>Contacts</td>\n\t\t\t\t\t\t<td>Home: unknown. Work: (03) 5555 6473</td>\n\t\t\t\t\t</tr>\n\t\t\t\t\t<tr>\n\t\t\t\t\t\t<td>Id</td>\n\t\t\t\t\t\t<td>MRN: 12345 (Acme Healthcare)</td>\n\t\t\t\t\t</tr>\n\t\t\t\t</tbody>\n\t\t\t</table>\n\t\t</div>"
  },
  "identifier": [
    {
      "use": "usual",
      "type": {
        "coding": [
          {
            "system": "http://terminology.hl7.org/CodeSystem/v2-0203",
            "code": "MR"
          }
        ]
      },
      "system": "urn:oid:1.2.36.146.595.217.0.1",
      "value": "12345",
      "period": {
        "start": "2001-05-06"
      },
      "assigner": {
        "display": "Acme Healthcare"
      }
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
    },
    {
      "use": "usual",
      "given": [
        "Jim"
      ]
    },
    {
      "use": "maiden",
      "family": "Windsor",
      "given": [
        "Peter",
        "James"
      ],
      "period": {
        "end": "2002"
      }
    }
  ],
  "telecom": [
    {
      "use": "home"
    },
    {
      "system": "phone",
      "value": "(03) 5555 6473",
      "use": "work",
      "rank": 1
    },
    {
      "system": "phone",
      "value": "(03) 3410 5613",
      "use": "mobile",
      "rank": 2
    },
    {
      "system": "phone",
      "value": "(03) 5555 8834",
      "use": "old",
      "period": {
        "end": "2014"
      }
    }
  ],
  "gender": "male",
  "birthDate": "1974-12-25",
  "_birthDate": {
    "extension": [
      {
        "url": "http://hl7.org/fhir/StructureDefinition/patient-birthTime",
        "valueDateTime": "1974-12-25T14:35:45-05:00"
      }
    ]
  },
  "deceasedBoolean": false,
  "address": [
    {
      "use": "home",
      "type": "both",
      "text": "534 Erewhon St PeasantVille, Rainbow, Vic  3999",
      "line": [
        "534 Erewhon St"
      ],
      "city": "PleasantVille",
      "district": "Rainbow",
      "state": "Vic",
      "postalCode": "3999",
      "period": {
        "start": "1974-12-25"
      }
    }
  ],
  "contact": [
    {
      "relationship": [
        {
          "coding": [
            {
              "system": "http://terminology.hl7.org/CodeSystem/v2-0131",
              "code": "N"
            }
          ]
        }
      ],
      "name": {
        "family": "du Marché",
        "_family": {
          "extension": [
            {
              "url": "http://hl7.org/fhir/StructureDefinition/humanname-own-prefix",
              "valueString": "VV"
            }
          ]
        },
        "given": [
          "Bénédicte"
        ]
      },
      "telecom": [
        {
          "system": "phone",
          "value": "+33 (237) 998327"
        }
      ],
      "address": {
        "use": "home",
        "type": "both",
        "line": [
          "534 Erewhon St"
        ],
        "city": "PleasantVille",
        "district": "Rainbow",
        "state": "Vic",
        "postalCode": "3999",
        "period": {
          "start": "1974-12-25"
        }
      },
      "gender": "female",
      "period": {
        "start": "2012"
      }
    }
  ],
  "managingOrganization": {
    "reference": "Organization/1"
  }
}
"""

let example_patient_trimmed = JsonNode.Parse(example_patient).ToJsonString()

let assertArgumentException func path =
    Assert.Throws<ArgumentException>(Action(fun () -> func path |> ignore)) |> ignore

let equalStringLists (a: string list) (b: string list) =
    Assert.Equal<string seq>(a, b)

[<Fact>]
let ``Test GetString`` () =
    let patient = JsonViaJsonNode.Parse example_patient :> IJsonElement

    Assert.Equal(patient.GetString [ "id" ], "example")
    Assert.Equal(patient.GetString [ "active" ], "true")
    Assert.Equal(patient.GetString [ "doesnt_exist" ], "")

    Assert.Equal(
        patient.GetString [ "doesnt_exist"
                            "a" ],
        ""
    )

    Assert.Equal(patient.GetString [ "text"; "status" ], "generated")

    assertArgumentException patient.GetString [ "identifier"; "use" ]
    assertArgumentException patient.GetString [ "name"; "use" ]
    assertArgumentException patient.GetString [ "contact" ]
    assertArgumentException patient.GetString [ "contact"; "relationship" ]

[<Fact>]
let ``Test GetStrings`` () =
    let patient = JsonViaJsonNode.Parse example_patient :> IJsonElement

    equalStringLists (patient.GetStrings [ "name"; "use" ]) [ "official"; "usual"; "maiden" ]
    equalStringLists (patient.GetStrings [ "address"; "line" ]) [ "534 Erewhon St" ]

    equalStringLists
        (patient.GetStrings [ "address"
                              "district" ])
        [ "Rainbow" ]

    equalStringLists
        (patient.GetStrings [ "contact"
                              "address"
                              "district" ])
        [ "Rainbow" ]

    equalStringLists
        (patient.GetStrings [ "contact"
                              "address"
                              "line" ])
        [ "534 Erewhon St" ]

    equalStringLists
        (patient.GetStrings [ "contact"
                              "address"
                              "404" ])
        []

    equalStringLists
        (patient.GetStrings [ "contact"
                              "404"
                              "404" ])
        []

    equalStringLists
        (patient.GetStrings [ "404"
                              "404"
                              "404" ])
        []

    equalStringLists (patient.GetStrings [ "404" ]) []


[<Fact>]
let ``Test SetString`` () =
    let patient = JsonViaJsonNode.Parse example_patient :> IJsonElement

    patient.SetString([ "id" ], "new-id")
    let expected = example_patient_trimmed.Replace("example", "new-id")
    Assert.Equal(patient.ToString(), expected)

    patient.SetString([ "text"; "foo" ], "boo")

    let expected = expected.Replace("},\"identifier\"", ",\"foo\":\"boo\"},\"identifier\"")

    Assert.Equal(patient.ToString(), expected)

    patient.SetString([ "meta"; "versionId" ], "123")

    let expected = expected.Substring(0, expected.Length - 1) + ""","meta":{"versionId":"123"}}"""

    Assert.Equal(patient.ToString(), expected)

[<Fact>]
let ``Test walk and modify`` () =
    let patient = JsonViaJsonNode.Parse example_patient :> IJsonElement

    patient.WalkAndModify(fun prop value -> None)
    let expected = example_patient_trimmed
    Assert.Equal(patient.ToString(), expected)

    patient.WalkAndModify(fun prop value ->
        if prop = "reference" then
            Some <| value + "23"
        else
            None
    )

    let expected = example_patient_trimmed.Replace("Organization/1", "Organization/123")
    Assert.Equal(patient.ToString(), expected)
