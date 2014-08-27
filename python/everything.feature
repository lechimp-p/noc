Feature: NoC API functionality
    Tests of the functionality of the NoC-API.
    I guess it will be good to split this file
    into parts sooner or later.


Scenario: Admin can log in
    Given User admin exists
    When I am admin
    Then I am logged in


Scenario Outline: Creation of things
    Given User <user> exists
    When I am <user>
    Then I <can> create a <thing> "<name>"  

Examples: User Creation
    | user  | can       | thing     | name      |
    | admin | can       | user      | user_a    |
    | user  | can not   | user      | user_u    |
    | admin | can       | channel   | channel_a |
    | user  | can       | channel   | channel_u |


Scenario Outline: Permissions on setting of properties of user 
    Given User <user> exists
    And User <other> exists
    When I am <user>
    Then I <can> set <property> of user "<other>"

Examples: User Property Permission
    | user      | other     | can     | property    |
    | admin     | user_1    | can     | login       |
    | admin     | user_1    | can     | password    |
    | admin     | user_1    | can     | name        |
    | admin     | user_1    | can     | desc        |
    | admin     | user_1    | can     | icon        |
    | user_2    | admin     | can not | login       |
    | user_2    | admin     | can not | password    |
    | user_2    | admin     | can not | name        |
    | user_2    | admin     | can not | desc        |
    | user_2    | admin     | can not | icon        |
    | user_2    | user_3    | can not | login       |
    | user_2    | user_3    | can not | password    |
    | user_2    | user_3    | can not | name        |
    | user_2    | user_3    | can not | desc        |
    | user_2    | user_3    | can not | icon        |
    | user_2    | user_2    | can not | login       |
    | user_2    | user_2    | can not | password    |
    | user_2    | user_2    | can not | name        |
    | user_2    | user_2    | can not | desc        |
    | user_2    | user_2    | can not | icon        |
