using System;
using Gherkin.Features;

namespace FSharp.Data.Gherkin.Tests.CSharp
{
    class Program
    {
        static void Main(string[] args)
        {
            var feature = CSharpTestFeature.CreateFeature();
            var featureFile = feature.Tags.featureTag1.Name + "," + feature.Tags.featureTag1.Name + "\r\n";
            featureFile += "Feature:" + feature.Name + "\r\n";

            featureFile += "\r\n";
            featureFile += "    Background:\r\n" + GetBackgroundSteps(feature.Background);

            featureFile += "\r\n";
            featureFile += "    " + feature.Scenarios.Scenario_1_name.Tags.scenario1Tag1.Name + "," + feature.Scenarios.Scenario_1_name.Tags.scenario1Tag2.Name + "\r\n";
            featureFile += "    Scenario:" + feature.Scenarios.Scenario_1_name.Name + "\r\n       " + feature.Scenarios.Scenario_1_name.Description + "\r\n" + GetScenario1Steps(feature.Scenarios.Scenario_1_name);

            featureFile += "\r\n";
            featureFile += "    " + feature.Scenarios.Scenario_2_name.Tags.scenario2Tag1.Name + "," + feature.Scenarios.Scenario_2_name.Tags.scenario2Tag2.Name + "\r\n";
            featureFile += "    Scenario:" + feature.Scenarios.Scenario_2_name.Name + "\r\n       " + feature.Scenarios.Scenario_2_name.Description  + "\r\n" + GetScenario2Steps(feature.Scenarios.Scenario_2_name);

            featureFile += "\r\n";
            featureFile += "    " + feature.Scenarios.Scenario_outline_name.Tags.scenarioOutlineTag1.Name + "," + feature.Scenarios.Scenario_outline_name.Tags.scenarioOutlineTag2.Name + "\r\n";
            featureFile += "    Scenario Outline:" + feature.Scenarios.Scenario_outline_name.Name + "\r\n     " + feature.Scenarios.Scenario_outline_name.Description  + "\r\n" + GetScenarioOutlineSteps(feature.Scenarios.Scenario_outline_name);

            Console.WriteLine("Look ma...no Specflow!\r\n" + featureFile);
        }

        private static string GetBackgroundSteps(CSharpTestFeature.CSharpTestFeature_Feature.BackgroundClass background)
        {
            var givenStep= background._0_Given_background_given_step;
            var whenStep= background._1_When_background_when_step;
            var thenStep = background._2_Then_background_then_step;

            var steps = "               " + givenStep.Order + " " + givenStep.Keyword + " " + givenStep.Text + "\r\n";
            steps += "                      " + givenStep.Argument.Content  + "\r\n";
            steps += "               " + whenStep.Order + " " + whenStep.Keyword + " " + whenStep.Text + "\r\n";
            steps += "               |" + whenStep.Argument[0].column1.Header + "|" + whenStep.Argument[0].column2.Header + "|\r\n";

            foreach(var row in whenStep.Argument)
            {
               steps += "               |" + row.column1.Value + "|" + row.column2.Value + "|\r\n";
            }

            steps += "               " + thenStep.Order + " " + thenStep.Keyword + " " + thenStep.Text + "\r\n";

            return steps;
                
        }

        private static string GetScenario1Steps(CSharpTestFeature.CSharpTestFeature_Feature.ScenarioContainer.Scenario_1_nameClass scenario)
        {
            var givenStep= scenario._0_Given_scenario_1_given_step;
            var whenStep= scenario._1_When_scenario_1_when_step;
            var thenStep = scenario._2_Then_scenario_1_then_step;

            var steps = "               " + givenStep.Order + " " + givenStep.Keyword + " " + givenStep.Text + "\r\n";
            steps += "                      " + givenStep.Argument.Content  + "\r\n";
            steps += "               " + whenStep.Order + " " + whenStep.Keyword + " " + whenStep.Text + "\r\n";
            steps += "               |" + whenStep.Argument[0].column1.Header + "|" + whenStep.Argument[0].column2.Header + "|\r\n";

            foreach(var row in whenStep.Argument)
            {
               steps += "               |" + row.column1.Value + "|" + row.column2.Value + "|\r\n";
            }
            steps += "               " + thenStep.Order + " " + thenStep.Keyword + " " + thenStep.Text + "\r\n";

            return steps;
                
        }

        private static string GetScenario2Steps(CSharpTestFeature.CSharpTestFeature_Feature.ScenarioContainer.Scenario_2_nameClass scenario)
        {
            var givenStep= scenario._0_Given_scenario_2_given_step;
            var whenStep= scenario._1_When_scenario_2_when_step;
            var thenStep = scenario._2_Then_scenario_2_then_step;

            var steps = "               " + givenStep.Order + " " + givenStep.Keyword + " " + givenStep.Text + "\r\n";
            steps += "                      " + givenStep.Argument.Content + "\r\n";
            steps += "               " + whenStep.Order + " " + whenStep.Keyword + " " + whenStep.Text + "\r\n";
            steps += "               |" + whenStep.Argument[0].column1.Header + "|" + whenStep.Argument[0].column2.Header + "|\r\n";

            foreach(var row in whenStep.Argument)
            {
               steps += "               |" + row.column1.Value + "|" + row.column2.Value + "|\r\n";
            }

            steps += "               " + thenStep.Order + " " + thenStep.Keyword + " " + thenStep.Text + "\r\n";

            return steps;
                
        }

        private static string GetScenarioOutlineSteps(CSharpTestFeature.CSharpTestFeature_Feature.ScenarioContainer.Scenario_outline_nameClass scenarioOutline)
        {
            var givenStep= scenarioOutline._0_Given_scenario_outline_given_step__Example_Column_1_;
            var whenStep= scenarioOutline._1_When_scenario_outline_when_step__Example_Column_2_;
            var thenStep = scenarioOutline._2_Then_scenario_outline_then_step__Example_Column_3_;

            var steps = "               " + givenStep.Order + " " + givenStep.Keyword + " " + givenStep.Text + "\r\n";
            steps += "                      " + givenStep.Argument.Content + "\r\n";
            steps += "               " + whenStep.Order + " " + whenStep.Keyword + " " + whenStep.Text + "\r\n";
            steps += "               |" + whenStep.Argument[0].column1.Header + "|" + whenStep.Argument[0].column2.Header + "|\r\n";

            foreach(var row in whenStep.Argument)
            {
               steps += "               |" + row.column1.Value + "|" + row.column2.Value + "|\r\n";
            }
            steps += "               " + thenStep.Order + " " + thenStep.Keyword + " " + thenStep.Text + "\r\n\r\n";

            steps += "              Examples:\r\n";
            steps += "               |" 
                        + scenarioOutline.Examples[0].Example_Column_1.Header + "|" 
                        + scenarioOutline.Examples[0].Example_Column_2.Header + "|" 
                        + scenarioOutline.Examples[0].Example_Column_3.Header + "|" + "\r\n";

            foreach(var example in scenarioOutline.Examples)
            {
                steps += "               |" 
                        + example.Example_Column_1.Value + "|" 
                        + example.Example_Column_2.Value + "|" 
                        + example.Example_Column_3.Value + "|" + "\r\n";
            }

            return steps;
                
        }
    }
}
