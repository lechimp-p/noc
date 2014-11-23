angular.module("NoC.my-profile", []).
controller("my-profile-controller", [ "$scope", "$timeout", "model", "user"
         , function($scope, $timeout, model, user) {
    "use strict";

    var throttleDelay = 1000; // ms

    $scope.updating = {};
    $scope.error = {};

    var throttledUpdate = function(key) {
        var updateState = { timeout : null };       

        $scope.updating[key] = false;
        
        $scope.$watch("user."+key, function(nval, oval) {
            if (_.isEqual(nval, oval)) {
                return;
            }

            $scope.updating[key] = true;
            $scope.error[key] = false;

            if (updateState.timeout !== null) {
                $timeout.cancel(updateState.timeout);
                updateState.timeout = null;
            }

            updateState.timeout = $timeout(function() {
                var upd = {};
                upd[key] = nval;
                model.user($scope.user.id).set(upd)
                    .success(function(_) {
                        $scope.updating[key] = false;
                    })         
                    .errorHandler(function(data, status, headers, config) {
                        $scope.error[key] = true;
                        $scope.updating[key] = false;
                        return true;
                    });
                updateState.timeout = null;
            }, throttleDelay);
        });
    };

    user.onIdAcquired(function(id) {
        $scope.user = { id : id }; 

        model.user($scope.user.id).get().success(function(response) {
            $scope.user = response;

            throttledUpdate("name");
            throttledUpdate("description");
            throttledUpdate("email");
        });
    });
}]);
