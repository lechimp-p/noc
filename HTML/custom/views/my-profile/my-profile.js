angular.module("NoC.my-profile", []).
controller("my-profile-controller", [ "$scope", "$timeout", "model", "user"
         , function($scope, $timeout, model, user) {
    "use strict";

    var throttleDelay = 1000; // ms

    $scope.error = {};
    $scope.confirmation = {};
    $scope.beforeChange = {};

    $scope.update = function(key, val) {
        var spl = key.split(".");
        if (spl[0] != "user") {
            console.log("my-profile-controller: can only update user.");
            return;
        }

        var upd = {};
        upd[spl.slice(1).join(".")] = val;
        return model.user($scope.user.id).set(upd)
                .errorHandler(function(data, status, headers, config) {
                    // Errors will be handled by the form.
                    return true;
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

            confirmedUpdate("password"); 
            // Passwort will no be reset after update.
        });
    });
}]);
