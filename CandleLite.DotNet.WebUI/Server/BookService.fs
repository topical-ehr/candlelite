namespace CandleLite.DotNet.WebUI.Server

open System
open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Logging;
open Bolero
open Bolero.Remoting
open Bolero.Remoting.Server
open CandleLite.DotNet.WebUI

open CandleLite.Core;
open CandleLite.DotNet;

type CandleLiteConfig() =
    interface Server.ICandleLiteConfig with
        member _.CurrentDateTime: DateTime = 
            DateTime.Now

        member _.SearchParameters: Indexes.ParametersMap = 
            CandleLite.Core.SearchParameters.defaultParametersMap;

type BookService(
    ctx: IRemoteContext,
    env: IWebHostEnvironment,
    logger: ILogger<BookService>
    ) =
    inherit RemoteHandler<Client.Main.BookService>()


    // Initialise CandleLite
    let dbImpl = SQLite.DotNetSQLiteImpl.UseFile("CandleLite.sqlite.db")
    let jsonImpl = JsonViaJsonNode.DotNetJSON(indent=true)
    let fhirServer  = Server.CandleLiteServer (CandleLiteConfig(), dbImpl, jsonImpl) :> Server.ICandleLiteServer


    override this.Handler =
        {
            sendRequest = fun req -> async {
                logger.LogInformation(sprintf "FHIR request: %A" req)

                let respHeaders = dict([])
                let resp = fhirServer.HandleRequest(
                    req.method,
                    "/fhir/" + req.path,
                    "/fhir",
                    req.body,
                    (fun header -> ""),
                    fun header value -> respHeaders.[header] <- value
                )

                return [
                    {
                        serverName = "localhost"
                        status = resp.Status
                        headers = []
                        body = resp.BodyString
                    }
                ]
            }

        }
