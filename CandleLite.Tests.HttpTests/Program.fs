open System
open Argu
open Expecto
open ServerRunners

open Hopac
open Logary
open Logary.Configuration
open Logary.Adapters.Facade
open Logary.Targets

type RemoteFhirArgs =
    | [<Mandatory>] Url of url: string
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Url _ -> "FHIR base url to connect to"

type GoFhirMongoRunnerArgs =
    | Verbose of verbose: bool
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Verbose _ -> "verbose mode"

type Args =
    | Expecto_Args of expectoArgs: string
    | [<CliPrefix(CliPrefix.None)>] Connect_To of ParseResults<RemoteFhirArgs>
    | [<CliPrefix(CliPrefix.None)>] Start_CandleLite of ParseResults<GoFhirMongoRunnerArgs>
    | [<CliPrefix(CliPrefix.None)>] Start_GoFHIR of ParseResults<GoFhirMongoRunnerArgs>
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Connect_To _ -> "test a running FHIR server"
            | Start_CandleLite _ -> "start an instance of CandleLite"
            | Start_GoFHIR _ -> "start an instance of GoFHIR"
            | Expecto_Args _ -> "argument to pass to the Expecto test runner"

[<EntryPoint>]
let main argv =

    System.Net.HttpWebRequest.DefaultWebProxy <- null
    Bogus.Randomizer.Seed <- System.Random(7777777)

    // as suggested at https://github.com/haf/expecto
    let logary =
      Config.create "CandleLite.Tests.Http" "localhost"
      |> Config.targets [ LiterateConsole.create LiterateConsole.empty "console" ]
      |> Config.processing (Events.events |> Events.sink ["console";])
      |> Config.build
      |> run
    LogaryFacadeAdapter.initialise<Expecto.Logging.Logger> logary
    
    let parser = ArgumentParser.Create<Args>(programName = "fhir-http-tests.exe")
    let args = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)

    let runners = Seq.toList <| seq {
        for result in args.GetAllResults() do
            match result with
            | Connect_To arg ->
                let connectTo = arg.GetResult(Url)
                yield new RemoteFhirRunner(connectTo) :> IFhirRunner
            | Start_CandleLite _ ->
                yield new CandleLiteFhirRunner() :> IFhirRunner
            | Start_GoFHIR _ ->
                yield new GoFhirMongoRunner() :> IFhirRunner
            | Expecto_Args _ -> ()
    } 

    match runners with
    | [ runner ] ->
        FhirServer.Current <- runner
    | _ ->
        let usage = parser.PrintUsage()
        printfn "%s" usage
        Environment.Exit(1)

    let expectoArgs = args.GetResult(Expecto_Args, defaultValue = "").Split(" ") |> Array.filter (fun s -> s.Trim().Length > 0)
    let expectoConfig = {
        defaultConfig with
            ``parallel`` = false
            verbosity = Expecto.Logging.Debug
            noSpinner = true
        }

    let r = Tests.runTestsInAssembly expectoConfig expectoArgs
    FhirServer.Current.Dispose()
    printfn "done"
    r