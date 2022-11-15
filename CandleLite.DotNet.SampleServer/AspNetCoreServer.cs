using Microsoft.AspNetCore.Http.Extensions;
using Microsoft.FSharp.Collections;

using CandleLite.Core;
using CandleLite.DotNet;

// Initialise FHIRLight
var dbImpl = SQLite.DotNetSQLiteImpl.UseFile("CandleLite.sqlite.db");
Server.ICandleLiteJSON jsonImpl = new JsonViaJsonNode.DotNetJSON();
var fhirServer = new Server.CandleLiteServer(new Config(), dbImpl, jsonImpl);

// Start HTTP server using the .NET 6 "Minimal API" (https://docs.microsoft.com/en-us/aspnet/core/fundamentals/minimal-apis?view=aspnetcore-6.0)
var builder = WebApplication.CreateBuilder(args);
var app = builder.Build();

app.UseDeveloperExceptionPage();
app.UseStatusCodePages();

app.MapGet("/", () => $"Hello World! Currently at {Environment.CurrentDirectory}");

CandleLite.DotNet.SampleServer.BrowseInHtml.AddRoutes(app, fhirServer);

app.MapMethods(
    "/fhir/{*path}",
    new[] { "GET", "POST", "PUT", "DELETE" },
    async (HttpRequest req, HttpResponse res) =>
    {
        string bodyString = await new StreamReader(req.Body).ReadToEndAsync();

        var response = fhirServer.HandleRequest(
            req.Method.ToString(),
            req.GetEncodedPathAndQuery(),
            "/fhir",
            bodyString,
            header => req.Headers[header].ToString(),
            (header, value) => res.Headers[header] = value
        );

        if (response.Status == 204)
        {
            return Results.NoContent();
        }
        else
        {
            res.StatusCode = response.Status;
            return Results.Text(response.BodyString, "application/fhir+json");
        }

    }
);

app.Run();

class Config : Server.ICandleLiteConfig
{
    // TODO: make more C#-friendly
    public FSharpMap<string, FSharpList<Tuple<string, Indexes.SearchParameter>>> SearchParameters => CandleLite.Core.SearchParameters.defaultParametersMap;

    public DateTime CurrentDateTime => DateTime.Now;
}
