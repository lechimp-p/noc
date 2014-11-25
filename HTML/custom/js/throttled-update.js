/* throttle-update - attribute direcitve

This directive notices changes on a model property defined via ng-model
attribute. It then sets the updating property in its scope and adds the
update-class on the element. After a delay it call update(model, value)
on the scope.

Expects the scope to have a property update or named as defined in 
attribute update-handler whicht takes the ng-model property and 
performs a (server?) update on it. The update-handler must return a 
promise.

*/

angular.module("throttled-update", [])
.directive("throttledUpdate", [ "$timeout", function($timeout) {
    "use strict";

    var throttleDelay = 1000; // ms

    var link = function(scope, element, attrs) {
        var updateClass = _.defaultUndef(attrs.updateClass, "updating"); 
        var errorClass = _.defaultUndef(attrs.errorClass, "faulty"); 
        var model = attrs.ngModel;
        var updateHandler = _.defaultUndef(attrs.updateHandler, "update");

        if (typeof model == "undefined") {
            return;
        }

        var state = { timeout : null
                    };

        scope.updating = false;
        scope.error = false;

        scope.$watch(model, function(nval, oval) {
            if (_.isEqual(nval, oval) || typeof nval == "undefined" || typeof oval == "undefined") {
                return;
            }

            scope.updating = true;
            element.addClass(updateClass);
            scope.error = false;
            element.removeClass(errorClass);

            if (state.timeout !== null) {
                $timeout.cancel(state.timeout);
                state.timeout = null;
            }

            state.timeout = $timeout(function() {
               scope.$eval(updateHandler)(model, nval)
                    .success(function() {
                        scope.updating = false;
                        element.removeClass(updateClass);
                    })
                    .error(function() {
                        scope.updating = false;
                        element.removeClass(updateClass);
                        scope.error = true;
                        element.addClass(errorClass);
                    })
                    ;
               state.timeout = null;
            }
            , throttleDelay);
 
        }); 
    };

    return  { restrict : "A"
            , scope : true // create own scope inherited from parent
            , link : link
            };
}])
;
