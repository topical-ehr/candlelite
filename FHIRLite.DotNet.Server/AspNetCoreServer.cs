using Microsoft.AspNetCore.Http.Extensions;

// Initialise FHIRLight
var deps = new FHIRLite.DotNet.Sqlite.SqliteImpl();
var fhirServer = new FHIRLite.Core.Server.FHIRLiteServer(deps);

// Start HTTP server using the .NET 6 "Minimal API" (https://docs.microsoft.com/en-us/aspnet/core/fundamentals/minimal-apis?view=aspnetcore-6.0)
var builder = WebApplication.CreateBuilder(args);
var app = builder.Build();
app.MapGet("/", () => "Hello World!");
app.MapMethods(
    "/fhir/{*path}",
    new[] { "GET", "POST", "PUT" },
    async (HttpRequest req, HttpResponse res) =>
    {
        var url = req.GetEncodedPathAndQuery();
        var prefix = "/fhir/";

        var bodyString = await new StreamReader(req.Body).ReadToEndAsync();

        var response = fhirServer.DoHTTP(
            req.Method.ToString(),
            url[prefix.Length..],
            bodyString,
            header => req.Headers[header].ToString(),
            (header, value) => res.Headers[header] = value
        );

        res.StatusCode = response.Status;

        return Results.Text(response.Body, "application/json");
    }
);

app.Run();
