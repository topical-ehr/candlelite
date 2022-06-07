using System.Text.Json.Nodes;
using Microsoft.AspNetCore.Http.Extensions;

var deps = new FHIRLite.DotNet.Sqlite.SqliteImpl();
var fhirServer = new FHIRLite.Core.Server.FHIRLiteServer(deps);


// server.PUT(new FHIRLite.Core.Server.Request()


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
        var bodyNode = JsonNode.Parse(bodyString);
        var body = new FHIRLite.DotNet.JsonViaJsonNode.JsonViaJsonNode(bodyNode);

        var response = fhirServer.HTTP(
            req.Method.ToString(),
            url[prefix.Length..],
            body,
            header => req.Headers[header].ToString(),
            (header, value) => res.Headers[header] = value
        );

        res.StatusCode = response.Status;

        return Results.Text(response.Body, "application/json");

    }
);

app.Run();
