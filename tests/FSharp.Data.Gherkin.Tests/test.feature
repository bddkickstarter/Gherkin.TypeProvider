@featureTag1 @featureTag2
Feature: Feature name

Multi-line
Feature Description

Background: Background name

Multi-line
Background Description

  Given background given step
"""
multi line
background
argument
"""

   When background when step
|column1|column2|
| data1 | data2 |
| data3 | data4 |
   Then background then step

@scenario1Tag1 @scenario1Tag2
Scenario: Scenario 1 name

Multi-line
Scenario 1 Description

  Given scenario 1 given step
"""
multi line
scenario 1
argument
"""

   When scenario 1 when step
|column1|column2|
| data1 | data2 |
| data3 | data4 |
   
   Then scenario 1 then step

@scenario2Tag1 @scenario2Tag2
Scenario: Scenario 2 name

Multi-line
Scenario 2 Description

  Given scenario 2 given step
"""
multi line
scenario 2
argument
"""

   When scenario 2 when step
|column1|column2|
| data1 | data2 |
| data3 | data4 |

   Then scenario 2 then step

@scenarioOutlineTag1 @scenarioOutlineTag2
Scenario Outline: Scenario outline name

Multi-line
Scenario Outline Description

  Given scenario outline given step (Example Column 1)
  """
multi line
scenario outline
argument
"""
   
   When scenario outline when step <Example Column 2> 
|column1|column2|
| data1 | data2 |
| data3 | data4 |

   Then scenario outline then step <Example Column 3>

   Examples: Set 1
|Example Column 1|Example Column 2|Example Column 3|
|  Set 1 Data 1  |  Set 1 Data 2  | Set 1 Data 3|

   Examples: Set 2
|Example Column 1|Example Column 2|Example Column 3|
|  Set 2 Data 1  |  Set 2 Data 2  |Set 2 Data 3|

   Examples: Set 3
|Example Column 1|Example Column 2|Example Column 3|
|  Set 3 Data 1  |  Set 3 Data 2  |Set 3 Data 3|




