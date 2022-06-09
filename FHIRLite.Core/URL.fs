module FHIRLite.Core.URL

type FhirParameter =
    {
        Name: string
        Modifier: string
        Value: string
    }

type FhirURL =
    {
        PathSegments: string array
        Parameters: FhirParameter array
    }

let parseParameters (qs: string) =
    let ps = qs.Split("&")

    ps
    |> Array.map (fun p ->
        let nv = p.Split('=', 1)

        {
            Name = nv.[0]
            Modifier = ""
            Value = nv.[1]
        })

let parse (relativeUrl: string) =
    let pathQS = relativeUrl.Split("?", 1)

    let segments = pathQS.[0].Split('/')

    let parameters =
        if pathQS.Length = 2 then
            parseParameters pathQS.[1]
        else
            [||]

    {
        PathSegments = segments
        Parameters = parameters
    }