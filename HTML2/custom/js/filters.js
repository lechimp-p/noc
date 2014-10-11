angular.module("NoC.filters", [])
.filter("username", [ "$rootScope", function($rootScope) { return function(input) {
    "use strict";

    if (typeof input.id == "undefined") {
        console.log("filter.username: Expected input to have an id..."); 
        return input;
    }

    if (input.id == $rootScope.user.id ) {
        return "Du";
    }
    else {
        return input.name;
    }
};}])
.filter("rel_timestamp", [ "$rootScope", function($rootScope) { return function(input) {
    "use strict";

    var utc_pattern = /(\d\d\d\d)-(\d\d)-(\d\d) (\d\d):(\d\d):(\d\d)\.(\d+) UTC/;  

    if (typeof input != "string") {
        console.log("filter.rel_timestamp: expected string.");
        return input;
    }

    var match = input.match(utc_pattern);
    if (match === null) {
        console.log("filter.rel_timestamp: string \"" + input + "\" does not match UTC pattern.");
        return input;
    }

    var now = new Date();
    var ts = new Date(parseInt(match[1]), parseInt(match[2]) - 1, parseInt(match[3]), // year month day
                      parseInt(match[4]) + $rootScope.user.UTC_offset, // hours
                      parseInt(match[5]), parseInt(match[6]), parseInt(match[7]) / 1000); // minutes seconds milliseconds

    // in seconds
    var diff = (now - ts) / 1000;
    if (diff < 60) {
        return "vor " + Math.floor(diff) + "s";
    } 
    
    // in minutes
    diff = diff / 60;
    if (diff < 60) {
        return "vor " + Math.floor(diff) + "m";
    }

    // in hours
    diff = diff / 60;
    if (diff < 24) {
        return "vor " + Math.floor(diff) + "h";
    }

    // in days
    diff = diff / 24;
    if (diff < 7) {
        return "vor " + Math.floor(diff) + "d";
    } 

    // in weeks
    diff = diff / 7;
    return "vor " + Math.floor(diff) + "w";
};}])
;
