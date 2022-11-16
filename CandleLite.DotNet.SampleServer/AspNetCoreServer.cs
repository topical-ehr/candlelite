using Microsoft.AspNetCore.Http.Extensions;

using CandleLite.Core;
using CandleLite.DotNet;

// Initialise CandleLite
var dbImpl = SQLite.DotNetSQLiteImpl.UseFile("CandleLite.sqlite.db");
Server.ICandleLiteJSON jsonImpl = new JsonViaJsonNode.DotNetJSON();
Server.CandleLiteServer fhirServer = new(new CandleLiteConfig(), dbImpl, jsonImpl);

// Start HTTP server using the .NET 6 "Minimal API" (https://docs.microsoft.com/en-us/aspnet/core/fundamentals/minimal-apis?view=aspnetcore-6.0)
var builder = WebApplication.CreateBuilder(args);
var app = builder.Build();

app.UseDeveloperExceptionPage();
app.UseStatusCodePages();

app.MapGet("/", () => $"Hello!\nRunning from {Environment.CurrentDirectory}");

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

BrowseInHtml.AddRoutes(app, fhirServer);

app.Run();
