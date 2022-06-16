module FHIRLite.Core.SearchParameters

open FHIRLite.Core.Indexes

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
    ]

let defaultParametersMap: ParametersMap = parameters |> Map.ofList
