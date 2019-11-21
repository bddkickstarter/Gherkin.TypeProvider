Feature: this is a feature

Background: this is a background
Given this is a background given
When this is a background when
Then this is a background then


Scenario: this is a scenario
Given this is a scenario given1
    |Data|
    |some given 1 data|
And this is a scenario given2
    |Data|
    |some given 2 data|
When this is a scenario when1
And this is a scenario when2

Then this is a scenario then1
And this is a scenario then2


Scenario: this is another scenario
Given this is another scenario given1
    |Data|
    |some given 1 data|
And this is a scenario given2
When this is a scenario when1
And this is a scenario when2

Then this is a scenario then1

Scenario Outline: this is a scenario outline
When foo <column>
Then bar

Examples: first set
|column|
|data|
|data2|

Examples: second set
|column|
|data3|
|data4|