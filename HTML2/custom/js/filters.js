angular.module("NoC.filters", []).
filter("username", [ "$rootScope", function($rootScope) { return function(input) {
    "use strict";

    if (typeof input.id == "undefined") {
        console.log("Expected input to have an id..."); 
        return input;
    }

    if (input.id == $rootScope.user.id ) {
        return "Du";
    }
    else {
        return input.name;
    }
};}]);
