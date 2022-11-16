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
        member this.CurrentDateTime: DateTime = 
            DateTime.Now

        member this.SearchParameters: Indexes.ParametersMap = 
            CandleLite.Core.SearchParameters.defaultParametersMap;

type BookService(
    
    ctx: IRemoteContext,
    env: IWebHostEnvironment,
    logger: ILogger<BookService>
    ) =
    inherit RemoteHandler<Client.Main.BookService>()

    let books =
        let json = Path.Combine(env.ContentRootPath, "data/books.json") |> File.ReadAllText
        JsonSerializer.Deserialize<Client.Main.Book[]>(json)
        |> ResizeArray

    // Initialise CandleLite
    let dbImpl = SQLite.DotNetSQLiteImpl.UseFile("CandleLite.sqlite.db");
    let jsonImpl = JsonViaJsonNode.DotNetJSON();
    let fhirServer  = Server.CandleLiteServer (CandleLiteConfig(), dbImpl, jsonImpl);


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

            getBooks = fun () -> async {
                logger.LogInformation("sending books list")
                return books.ToArray()
            }

            addBook = ctx.Authorize <| fun book -> async {
                books.Add(book)
            }

            removeBookByIsbn = ctx.Authorize <| fun isbn -> async {
                books.RemoveAll(fun b -> b.isbn = isbn) |> ignore
            }

            signIn = fun (username, password) -> async {
                if password = "password" then
                    // doesnt work in server mode blazor
                    // do! ctx.HttpContext.AsyncSignIn(username, TimeSpan.FromDays(365.))
                    return Some username
                else
                    return None
            }

            signOut = fun () -> async {
                return! ctx.HttpContext.AsyncSignOut()
            }

            getUsername = ctx.Authorize <| fun () -> async {
                return ctx.HttpContext.User.Identity.Name
            }
        }
