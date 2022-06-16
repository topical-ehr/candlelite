module Benchmark

open System
open System.IO
open System.Threading
open System.Diagnostics
open System.Net.Http
open Hl7.Fhir.Model
open Hl7.Fhir.Rest

open Utils
open Bogus.DataSets

let jsonSerialiser = Hl7.Fhir.Serialization.FhirJsonSerializer()


module TestData =
    let faker = Bogus.Faker()

    let Patients count =
        Seq.init count id
        |> Seq.map (fun i ->
            let sample = Bogus.Person()
            let random = Bogus.Randomizer()

            let medicareNo = random.ReplaceNumbers("##########") |> Some
            let medicareLineNo = random.ReplaceNumbers("#") |> Some

            let gender = faker.PickRandom<Name.Gender>()
            let prefix = faker.Name.Prefix(N gender) |> Some
            let firstName = faker.Name.FirstName(N gender)
            let lastName = faker.Name.LastName(N gender)
            let middleName = faker.Name.FirstName(N gender) |> Some

            let patientId = i
            let practiceSoftwareId = Identifier(
                                        Value = patientId.ToString(),
                                        System = "https://example.com/fhir/patient-id",
                                        Type = CodeableConcept("https://hl7.org/fhir/v2/0203", "MR", "Test Fixture Patient ID"))

            // Medicare - see http://fhir.hl7.org.au/smart-on-fhir/profiles/profile-medicare/
            let medicareString =
                //  10 digits or 11 digits if with IRN digit
                [medicareNo; medicareLineNo |> Option.map (fun i -> i.ToString())]
                |> List.map (fun so -> so |> Option.defaultValue "")
                |> String.concat ""
    
            let medicarePeriod = Period(EndElement=FhirDateTime(2018, 2)) |> Some

            let medicareId = 
                if medicareString.Length > 1 then
                    Identifier(
                        System = "http://ns.electronichealth.net.au/id/hi/mc",
                        Value = medicareString,
                        Period = (medicarePeriod |> Option.toObj),
                        Type = CodeableConcept(
                                Coding=L [Coding("http://hl7.org/fhir/v2/0203", "MC", "Patient's Medicare Number")],
                                Text = "Medicare Number" )
                        ) |> Some
                else
                    None
    
            let getFullTextName (components:List<string option>) =
                let strings =
                    components
                    |> List.collect Option.toList
                    |> List.filter (fun s -> s.Length > 0)
                String.Join(" ", strings)

            let nameOfficial =
                HumanName(
                    Use = (N HumanName.NameUse.Official),
                    Prefix = (prefix |> Option.toList),
                    Given =  ([Some firstName; middleName] |> List.map Option.toList |> List.collect id),
                    Family = lastName,
                    Text = (getFullTextName [Some firstName; middleName; Some lastName]) )

            let nameUsual =
                HumanName(
                    Use = (N HumanName.NameUse.Usual),
                    Text = firstName)

            let toFhirContactPoint (value:string option) (useType:ContactPoint.ContactPointUse option) (system:ContactPoint.ContactPointSystem) (preferred:bool) =
                match value with
                | Some value ->
                    let c = ContactPoint(System = N system, Value = value)
                    Option.iter (fun x -> c.Use <- N x) useType
                    if preferred then c.Rank <- N 1
                    Some c
                | None -> None
            let contactPoints = [toFhirContactPoint (Some sample.Phone) (Some ContactPoint.ContactPointUse.Home) ContactPoint.ContactPointSystem.Phone true;
                                 toFhirContactPoint (Some sample.Phone) (Some ContactPoint.ContactPointUse.Mobile) ContactPoint.ContactPointSystem.Phone false;
                                 toFhirContactPoint (Some sample.Phone) (Some ContactPoint.ContactPointUse.Work) ContactPoint.ContactPointSystem.Phone false;
                                 toFhirContactPoint (Some sample.Email) None ContactPoint.ContactPointSystem.Email true
                                 ContactPoint(System= N ContactPoint.ContactPointSystem.Other) |> Some
                                ]

            let address =
                Hl7.Fhir.Model.Address(
                        Type = N Address.AddressType.Postal,
                        Line = [sample.Address.Street],
                        City = sample.Address.City,
                        PostalCode = sample.Address.ZipCode
                    )

            let addressNoCity =
                Hl7.Fhir.Model.Address(
                        Type = N Address.AddressType.Postal,
                        Line = [sample.Address.Street]
                    )

            let addressNoLine =
                Hl7.Fhir.Model.Address(
                        Type = N Address.AddressType.Postal,
                        City = sample.Address.City,
                        PostalCode = sample.Address.ZipCode
                    )
            let patient =
                Hl7.Fhir.Model.Patient(
                    Identifier = ( [medicareId; Some practiceSoftwareId] |> List.map Option.toList |> List.collect id |> L ),
                    Name = L [nameOfficial; nameUsual],
                    BirthDate = Hl7.Fhir.Support.DateExtensions.ToFhirDate(sample.DateOfBirth),
                    Gender = N (match gender with | Name.Gender.Male -> AdministrativeGender.Male | Name.Gender.Female -> AdministrativeGender.Female | _ -> failwithf "invalid gender value %A" gender),
                    Telecom = L (contactPoints |> List.map Option.toList |> List.collect id),
                    Address = L [address; addressNoCity; addressNoLine],
                    Active = N true,
                    Link = L[
                        Patient.LinkComponent(
                            Other=ResourceReference(Identifier=practiceSoftwareId, Display="sample placeholder for link to identify patient in the source practice software"),
                            Type=N Patient.LinkType.Seealso)
                    ]
                )
            patient

        )


module Benchmarks =
    let QuestionnaireResponsesWithInProgressEncounters (fhir: FhirClient) =
        
        let patients = TestData.Patients 50 |> Seq.map fhir.Create

        ()  
