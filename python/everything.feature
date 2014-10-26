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
    | admin     | user_1    | can     | description |
    | admin     | user_1    | can     | icon        |
    | user_2    | admin     | can not | login       |
    | user_2    | admin     | can not | password    |
    | user_2    | admin     | can not | name        |
    | user_2    | admin     | can not | description |
    | user_2    | admin     | can not | icon        |
    | user_2    | user_3    | can not | login       |
    | user_2    | user_3    | can not | password    |
    | user_2    | user_3    | can not | name        |
    | user_2    | user_3    | can not | description |
    | user_2    | user_3    | can not | icon        |
    | user_2    | user_2    | can     | login       |
    | user_2    | user_2    | can     | password    |
    | user_2    | user_2    | can     | name        |
    | user_2    | user_2    | can     | description |
    | user_2    | user_2    | can     | icon        |

Scenario: Searching of users by login
    Given User searcher exists
    And User lechimp exists
    And User lechuck exists
    And User guybrush exists
    When I am searcher 
    And I search for user lech
    Then the result will contain "lechimp"
    And the result will contain "lechuck"
    And the result will not contain "guybrush" 
    And the result will not contain "Georg Lukas" 

Scenario: Searching of channels by name
    Given User searcher exists
    And Channel channel 3 exists
    When searcher creates a channel "channel 1" 
    And searcher creates a channel "channel 2" 
    And searcher creates a channel "camel"
    And I am searcher
    And I search for channel chan
    Then the result will contain "channel 1"
    And the result will contain "channel 2"
    And the result will not contain "camel"
    And the result will not contain "cow"
    And the result will not contain "channel 3"

Scenario: Channels for contacts
    Given User user_1 exists
    And User user_2 exists
    When user_1 has contact one to user_2
    And user_2 has contact two to user_1
    Then channel of one equals channel of two 

Scenario: Creation of channels is idempotent
    Given User user_3 exists
    And User user_4 exists
    When user_3 has contact one to user_4
    And user_3 has contact two to user_4
    Then channel of one equals channel of two
