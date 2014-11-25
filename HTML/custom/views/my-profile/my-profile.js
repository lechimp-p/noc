angular.module("NoC.my-profile", []).
controller("my-profile-controller", [ "$scope", "$timeout", "model", "user"
         , function($scope, $timeout, model, user) {
    "use strict";

    var throttleDelay = 1000; // ms

    $scope.updating = {};
    $scope.error = {};
    $scope.confirmation = {};
    $scope.beforeChange = {};

    var update = function(key, val) {
        var upd = {};
        upd[key] = val;
        model.user($scope.user.id).set(upd)
             .success(function(_) {
                $scope.updating[key] = false;
             })         
             .errorHandler(function(data, status, headers, config) {
                $scope.error[key] = true;
                $scope.updating[key] = false;
                return true;
             });
    };

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
               update(key, nval);
               updateState.timeout = null;
            }, throttleDelay);
        });
    };

    var confirmedUpdate = function(key) {
        // No need for confirmation on startup.
        $scope.confirmation[key] = false;        

        $scope.$watch("user."+key, function(nval, oval) {
            if (_.isEqual(nval, oval) || $scope.confirmation[key]) {
                return;
            }
             
            // Now we need a confirmation...
            $scope.confirmation[key] = true;
            $scope.beforeChange[key] = oval;
        });
    };

    $scope.confirm = function(key) {
        $scope.confirmation[key] = false;
        $scope.updating[key] = true;
        update(key, $scope.user[key]);    
    };

    $scope.abort = function(key) {
        $scope.confirmation[key] = false;
        $scope.user[key] = $scope.beforeConfirmation;
    };

    user.onIdAcquired(function(id) {
        $scope.user = { id : id }; 

        model.user($scope.user.id).get().success(function(response) {
            $scope.user = response;
            $scope.user.password = "xxxxxxxx";

            throttledUpdate("login");
            throttledUpdate("name");
            throttledUpdate("description");
            throttledUpdate("email");
            throttledUpdate("email");
            confirmedUpdate("password"); 
            // Passwort will no be reset after update.
        });
    });
}]);
