angular.module("update", [])
.directive("throttledUpdate", [ "$timeout", function($timeout) {
    "use strict";

    // The time to wait between the last change on the model and 
    // calling $scope.update.
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
               scope.$eval(updateHandler)(model)
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
.directive("confirmedUpdate", [ "$parse", function($parse) {
    "use strict";

    var link = function(scope, element, attrs) {
        var updateClass = _.defaultUndef(attrs.updateClass, "updating"); 
        var confirmationClass = _.defaultUndef(attrs.confirmationClass, "confirmation"); 
        var errorClass = _.defaultUndef(attrs.errorClass, "faulty"); 
        var model = attrs.ngModel;
        var updateHandler = _.defaultUndef(attrs.updateHandler, "update");

        if (typeof model == "undefined") {
            return;
        }

        scope.confirmation = false;
        scope.error = false;

        var state = { old_value : null 
                    , abort : false
                    };

        scope.$watch(model, function(nval, oval) {
            if ( _.isEqual(nval, oval) || typeof nval == "undefined" || typeof oval == "undefined") {
                return;
            }

            // This is to not start confirmation mode again
            // after calling abort.
            if (state.abort) {
                state.abort = false;
                return;
            }

            scope.confirmation = true;
            element.addClass(confirmationClass);
            scope.updating = false;
            element.removeClass(updateClass);
            scope.error = false;
            element.removeClass(errorClass);

            // We maybe already captured the starting value.
            if (state.old_value === null) {
                state.old_value = oval;
            }
        }); 

        scope.confirm = function() {
            // Maybe we don't need a confirmation.
            if (scope.confirmation === false) {
                return;
            }

            scope.confirmation = false;
            element.removeClass(confirmationClass);
            scope.updating = true;
            element.addClass(updateClass);

            state.old_value = null;

            scope.$eval(updateHandler)(model)
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
        };

        scope.abort = function() {
            if (state.old_value === null) {
                return;
            }

            state.abort = true;

            var modelHandle = $parse(model);
            modelHandle.assign(scope, state.old_value);

            scope.confirmation = false;
            element.removeClass(confirmationClass);
        };
    };

    return  { restrict : "A"
            , scope : true // create own scope inherited from parent
            , link : link
            };
}])
;
