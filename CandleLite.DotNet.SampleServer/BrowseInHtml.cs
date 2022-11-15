using System.Text.Json.Nodes;
using System.Text.Json;

using Microsoft.AspNetCore.Http.Extensions;
using Microsoft.FSharp.Core;

using CandleLite.Core;

namespace CandleLite.DotNet.SampleServer;

public class BrowseInHtml
{
    public static void AddRoutes(WebApplication app, CandleLite.Core.Server.CandleLiteServer fhirServer)
    {
        app.MapMethods(
            "/browse/{*path}",
            new[] { "GET", "POST", "PUT", "DELETE" },
            async (HttpRequest req, HttpResponse res) =>
            {
                string bodyString = await new StreamReader(req.Body).ReadToEndAsync();
                string GetJson()
                {
                    var response = fhirServer.HandleRequest(
                        req.Method.ToString(),
                        req.GetEncodedPathAndQuery(),
                        "/browse",
                        bodyString,
                        header => "",
                        (header, value) => { }
                    );

                    if (response.Status == 204)
                    {
                        return "HTTP 204 - No Content";
                    }
                    else
                    {
                        return response.BodyString;
                    }
                }

                string FormatJson(string json)
                {
                    var referencesToLinks = (string property, string value) =>
                    {
                        if (property == "reference")
                        {
                            return FSharpOption<string>.Some($"<a href='/browse/{value}'>{value}</a>");
                        }
                        else
                        {
                            return FSharpOption<string>.None;
                        }
                    };

                    var jsonNode = JsonNode.Parse(json);
                    JSON.IJsonElement elt = new JsonViaJsonNode.JsonViaJsonNode(jsonNode);
                    elt.WalkAndModify(FuncConvert.FromFunc(referencesToLinks));
                    return jsonNode!.ToJsonString(new JsonSerializerOptions
                    {
                        WriteIndented = true,

                        // unsafe but using Content-Security-Policy for XSS protection
                        Encoder = System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping,
                    });
                }

                string json = GetJson();
                json = FormatJson(json);

                string browseHtml = File.ReadAllText("wwwroot/browse.html");
                browseHtml = browseHtml.Replace("{{JSON}}", json);

                res.Headers["Content-Security-Policy"] = "script-src 'self'";
                return Results.Text(browseHtml, "text/html");
            }
        );
        app.UseStaticFiles();
    }
}
