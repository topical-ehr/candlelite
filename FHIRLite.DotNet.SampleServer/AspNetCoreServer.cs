using Microsoft.AspNetCore.Http.Extensions;
using Microsoft.FSharp.Collections;

using FHIRLite.Core;
using FHIRLite.DotNet;

// Initialise FHIRLight
var dbImpl = SQLite.DotNetSQLiteImpl.UseFile("fhirlite.sqlite.db");
var jsonImpl = new JsonViaJsonNode.DotNetJSON();
var fhirServer = new Server.FHIRLiteServer(new Config(), dbImpl, jsonImpl);

// Start HTTP server using the .NET 6 "Minimal API" (https://docs.microsoft.com/en-us/aspnet/core/fundamentals/minimal-apis?view=aspnetcore-6.0)
var builder = WebApplication.CreateBuilder(args);
var app = builder.Build();
app.MapGet("/", () => "Hello World!");
app.MapMethods(
    "/fhir/{*path}",
    new[] { "GET", "POST", "PUT", "DELETE" },
    async (HttpRequest req, HttpResponse res) =>
    {
        var url = req.GetEncodedPathAndQuery();

        var bodyString = await new StreamReader(req.Body).ReadToEndAsync();

        var response = fhirServer.HandleRequest(
            req.Method.ToString(),
            url,
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

class Config : Server.IFHIRLiteConfig
{
    // TODO: make more C#-friendly
    public FSharpMap<string, FSharpList<Tuple<string, Indexes.SearchParameter>>> SearchParameters => FHIRLite.Core.SearchParameters.defaultParametersMap;

    public DateTime CurrentDateTime => DateTime.Now;

    public string BasePath => "/fhir";
}