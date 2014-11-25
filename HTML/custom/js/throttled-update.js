angular.module("throttled-update", [])
.directive("throttled-update", [ function() {
    "use strict";

    var link = function(scope, element, attrs) {
        alert("here");
    };

    return  { restrict : "A"
            , link : link
            };
}]);
