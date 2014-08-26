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
