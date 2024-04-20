module CandleLite.Core.SearchParameters

open CandleLite.Core.Indexes

let parameters =
    [
        "ALL", [ indexId ]

        "Patient",
        [
            identifier

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
            identifier

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

        "PractitionerRole",
        [
            identifier
            "active", indexBool [ "active" ]

            reference "practitioner"
            reference "organization"

            "role", codeableConcept "code"
            "specialty", codeableConcept "specialty"
            "characteristic", codeableConcept "characteristic"
        ]

        "Composition",
        [
            identifier

            "type", codeableConcept "type"
            "category", codeableConcept "category"
            "status", indexString ["status"]

            reference "encounter"
            reference "subject"
        ]

        "Condition",
        [
            identifier

            "code", codeableConcept "code"
            "category", codeableConcept "category"
            "verification-status", codeableConcept "verificationStatus"
            "clinical-status", codeableConcept "clinicalStatus"

            reference "encounter"
            reference "subject"
        ]

        "DiagnosticReport",
        [
            identifier

            "code", codeableConcept "code"
            "category", codeableConcept "category"
            "status", indexString [ "status" ]

            reference "encounter"
            reference "subject"
        ]

        "Encounter",
        [
            identifier

            "status", indexString [ "status" ]
            "class", indexString [ "class" ]

            "type", codeableConcept "type"
            "serviceType", codeableConcept "serviceType"
            "priority", codeableConcept "priority"

            reference "subject"
        ]

        "List",
        [
            identifier

            "code", codeableConcept "code"
            "category", codeableConcept "category"

            reference "encounter"
            reference "source"
            reference "subject"
        ]

        "MedicationAdministration",
        [
            identifier

            "category", codeableConcept "category"
            "status", indexString ["status"]

            reference "encounter"
            reference "subject"
        ]

        "MedicationRequest",
        [
            identifier

            "category", codeableConcept "category"
            "status", indexString ["status"]

            reference "encounter"
            reference "subject"
        ]

        "Observation",
        [
            identifier
            "code", codeableConcept "code"
            reference "encounter"
            reference "subject"
        ]

        "Organization",
        [
            identifier

            "active", indexBool [ "active" ]
        ]


        "ServiceRequest",
        [
            identifier
            "code", codeableConcept "code"
            "category", codeableConcept "category"
            reference "encounter"
            reference "subject"
        ]

        "Task",
        [
            identifier
            "code", codeableConcept "code"
            reference "encounter"
            referenceWithNameAndProperty "subject" "for"
        ]
    ]

let defaultParametersMap: ParametersMap = parameters |> Map.ofList
