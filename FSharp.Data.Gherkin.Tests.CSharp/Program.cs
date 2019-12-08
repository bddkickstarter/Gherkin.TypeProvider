using System;
using FSharp.Data.Gherkin.Features;

namespace FSharp.Data.Gherkin.Tests.CSharp
{
    class Program
    {
        static void Main(string[] args)
        {
            var feature = CSharpTestFeature.CreateFeature();
            Console.WriteLine("Feature is " + feature.Scenario_1_name.Name);
        }
    }
}
