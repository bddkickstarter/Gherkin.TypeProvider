Feature: Outlines

Scenario Outline: some group of examples
Given some setup
When uses <uses the data>
Then result will be <checks the result>

Examples:
|uses the data|checks the result|
|    data 1   |     true!       |
|    data 2   |     false!      |
|    data 3   |     maybe?      |