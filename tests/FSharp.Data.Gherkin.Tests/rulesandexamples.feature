Feature: Rules and Examples

    Scenario: lkj
        Given sdfsdf

    Rule: This is the first Rule

        Example: First example title
            Given example 1 given
            """
                hello world
            """
            When example 1 when
            |hello|world|
            |foo|bar|
            Then example 1 then

    Rule: This is the second Rule

        Example: Second example title
            Given example 2 given
            """
                hello world again
            """
            When example 2 when
            |hello2|world2|
            |foo1|bar2|
            Then example 1 then

