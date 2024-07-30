namespace CandleLite.DotNet.SampleServer;

using System;

using CandleLite.Core;

class CandleLiteConfig : Server.ICandleLiteConfig
{
    public Indexes.ParametersMap SearchParameters => Core.SearchParameters.defaultParametersMap;

    public DateTime CurrentDateTime => DateTime.Now;
}
