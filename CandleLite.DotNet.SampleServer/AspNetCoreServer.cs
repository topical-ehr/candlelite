namespace CandleLite.DotNet.SampleServer;

using Microsoft.AspNetCore.Http.Extensions;
using Microsoft.Extensions.Logging;

using LotusLogger;

using CandleLite.Core;
using CandleLite.DotNet;

public class AspNetCoreServer
{
    // Initialise CandleLite
    static readonly SQLite.DotNetSQLiteImpl dbImpl = SQLite.DotNetSQLiteImpl.UseFile("CandleLite.sqlite.db");
    static readonly Server.ICandleLiteJSON jsonImpl = new JsonViaJsonNode.DotNetJSON(indent: true);
    static readonly Server.ICandleLiteServer fhirServer = new Server.CandleLiteServer(new CandleLiteConfig(), dbImpl, jsonImpl);

    public static void Main(string[] args)
    {
        Run(args, 5454, CancellationToken.None).Wait();
    }
    public static Task Run(string[] args, int port, CancellationToken ct)
    {
        // Start HTTP server using the .NET 6 "Minimal API" (https://docs.microsoft.com/en-us/aspnet/core/fundamentals/minimal-apis?view=aspnetcore-6.0)
        var builder = WebApplication.CreateBuilder(args);
        builder.Services.AddHttpClient();
        var app = builder.Build();


        // initialise logging
        Logger.Sink = new LotusLogger.Sinks.HttpSink(
            new Uri("http://localhost:10000/log/demo"),
            app.Services.GetRequiredService<IHttpClientFactory>(),
            TimeSpan.FromSeconds(1),
            app.Services.GetRequiredService<ILoggerFactory>()
        );
        Logger.Component = "CandleLite";
        Logger.InstanceId = Guid.NewGuid().ToString();

        app.Urls.Add($"http://*:{port}");

        app.UseDeveloperExceptionPage();
        app.UseStatusCodePages();

        app.MapGet("/", () => $"Hello!\nCandleLite sample FHIR server is running from {Environment.CurrentDirectory}");

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

        return app.RunAsync(ct);
    }
}