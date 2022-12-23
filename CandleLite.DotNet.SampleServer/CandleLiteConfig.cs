namespace CandleLite.DotNet.SampleServer;

using System;
using Microsoft.FSharp.Collections;

using CandleLite.Core;

class CandleLiteConfig : Server.ICandleLiteConfig
{
    // TODO: make more C#-friendly
    public FSharpMap<string, FSharpList<Tuple<string, Indexes.SearchParameter>>> SearchParameters => CandleLite.Core.SearchParameters.defaultParametersMap;

    public DateTime CurrentDateTime => DateTime.Now;
}
