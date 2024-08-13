module CandleLite.Core.SearchParameters

open CandleLite.Core.Indexes

let defaultParameters =
    [
        "ALL",
        [
            indexId
            identifier
        ]

        "CarePlan",
        [
            "status", indexString [ "status" ]
            "intent", indexString ["intent"]
            "category", codeableConcept "category"

            reference "encounter"
            reference "subject"
            reference "subject"

            referenceWithPath "performer" ["activity"; "detail"; "performer"]
        ]

        "CareTeam",
        [
            "status", indexString [ "status" ]
            "category", codeableConcept "category"
            "name", indexString [ "name" ]

            reference "encounter"
            reference "subject"

            referenceWithPath "participant" ["participant"; "member"]
        ]

        "Patient",
        [
            "active", indexBool [ "active" ]
            "birthdate", indexString [ "birthDate" ]
            "gender", indexString [ "gender" ]
            "death-date", indexDateTime [ "deceasedDateTime" ]
            "deceased", indexTrueOrDateExists "deceased"

            "email", contactPoints [ "telecom" ] (Some "email")
            "phone", contactPoints [ "telecom" ] (Some "phone")
            "telecom", contactPoints [ "telecom" ] None
            "address", indexAddress [ "address" ]

            "given", indexStrings [ "name"; "given" ]
            "family", indexStrings [ "name"; "family" ]
            "name", humanName [ "name" ]
        ]

        "Practitioner",
        [
            "active", indexBool [ "active" ]
            "birthdate", indexString [ "birthDate" ]
            "gender", indexString [ "gender" ]

            "email", contactPoints [ "telecom" ] (Some "email")
            "phone", contactPoints [ "telecom" ] (Some "phone")
            "telecom", contactPoints [ "telecom" ] None
            "address", indexAddress [ "address" ]

            "given", indexStrings [ "name"; "given" ]
            "family", indexStrings [ "name"; "family" ]
            "name", humanName [ "name" ]
        ]

        "PractitionerRole",
        [
            "active", indexBool [ "active" ]

            reference "practitioner"
            reference "organization"

            "role", codeableConcept "code"
            "specialty", codeableConcept "specialty"
            "characteristic", codeableConcept "characteristic"
        ]

        "Composition",
        [
            "type", codeableConcept "type"
            "category", codeableConcept "category"
            "status", indexString ["status"]

            reference "encounter"
            reference "subject"
        ]

        "Condition",
        [
            "code", codeableConcept "code"
            "category", codeableConcept "category"
            "verification-status", codeableConcept "verificationStatus"
            "clinical-status", codeableConcept "clinicalStatus"

            reference "encounter"
            reference "subject"
        ]

        "DiagnosticReport",
        [
            "code", codeableConcept "code"
            "category", codeableConcept "category"
            "status", indexString [ "status" ]

            reference "encounter"
            reference "subject"
        ]

        "Encounter",
        [
            "status", indexString [ "status" ]
            "class", indexString [ "class" ]

            "type", codeableConcept "type"
            "serviceType", codeableConcept "serviceType"
            "priority", codeableConcept "priority"

            reference "subject"
        ]

        "List",
        [
            "code", codeableConcept "code"
            "category", codeableConcept "category"

            reference "encounter"
            reference "source"
            reference "subject"
        ]

        "MedicationAdministration",
        [
            "category", codeableConcept "category"
            "status", indexString ["status"]

            reference "encounter"
            reference "subject"
        ]

        "MedicationRequest",
        [
            "category", codeableConcept "category"
            "status", indexString ["status"]

            reference "encounter"
            reference "subject"
        ]

        "Observation",
        [
            "code", codeableConcept "code"
            reference "encounter"
            reference "subject"
        ]

        "Organization",
        [
            "active", indexBool [ "active" ]
        ]


        "ServiceRequest",
        [
            "code", codeableConcept "code"
            "category", codeableConcept "category"
            reference "encounter"
            reference "subject"
        ]

        "Task",
        [
            "code", codeableConcept "code"
            reference "encounter"
            referenceWithPath "subject" ["for"]
        ]
    ]


let fromCSV (text: string) : ParametersMap =

    let toParameter fields =
        let resource = List.head fields
        
        let param =
            match List.tail fields with
            | ["id"] -> indexId
            | ["identifier"] -> identifier
            | name :: "address" :: path -> name, indexAddress path
            | name :: "bool" :: [] -> name, indexBool [name]
            | name :: "bool" :: path -> name, indexBool path
            | name :: "codeableConcept" :: [] -> name, codeableConcept name
            | name :: "codeableConcept" :: path :: [] -> name, codeableConcept path
            | name :: "contactPoints" :: "any" :: path -> name, contactPoints path None
            | name :: "contactPoints" :: systemFilter :: path -> name, contactPoints path (Some systemFilter)
            | name :: "datetime" :: path -> name, indexDateTime path
            | name :: "humanName" :: [] -> name, humanName [name]
            | name :: "string" :: [] -> name, indexString [name]
            | name :: "string" :: path -> name, indexString path
            | name :: "strings" :: path -> name, indexStrings path
            | name :: "reference" :: [] -> reference name
            | name :: "reference" :: path -> referenceWithPath name path
            | name :: "trueOrDateExists" :: path :: [] -> name, indexTrueOrDateExists path
            | _ -> failwithf "unable to parse search parameter spec: %A" fields

        resource, param

    text.Split('\n', System.StringSplitOptions.RemoveEmptyEntries)
    |> Seq.ofArray
    |> Seq.map (fun line -> line.Trim())

    // skip CSV header and comments
    |> Seq.filter (fun line -> not <| (line.StartsWith "#" || line.StartsWith "Resource" || line.Length = 0))

    // parse row
    |> Seq.map (fun line -> line.Replace(" ", "").Split(',') |> List.ofArray)
    |> Seq.map toParameter

    // group by ResourceType
    |> Seq.groupBy fst
    |> Seq.map (fun (k, v) -> (k, v |> Seq.map snd |> List.ofSeq ))
    |> Map.ofSeq
    |> ParametersMap


let defaultParametersMap: ParametersMap = defaultParameters |> Map.ofList |> ParametersMap