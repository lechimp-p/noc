angular.module("imgInput", [])
.directive("imgInput", [ "$compile", function($compile) {
    "use strict";

    var link = function(scope, elem, attrs) {
        var model = attrs.ngModel;
        console.log(elem);

        if (typeof model == "undefined") {
            return;
        }

        var input = angular.element("<input type='file' ng-model='filename'/>");
        elem.append(input);
        $compile(input)(scope);
    };

    return { restrict : "E"
           , link : link 
           , scope : {
                filename : "=ngModel"
            } 
           };
}])
;
