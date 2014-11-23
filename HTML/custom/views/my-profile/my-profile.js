angular.module("NoC.my-profile", []).
controller("my-profile-controller", [ "$scope", "$timeout", "model", "user"
         , function($scope, $timeout, model, user) {
    "use strict";

    var throttleDelay = 800; // ms

    var throttledUpdate = function(key) {
        var updateState = { timeout : null };       
        
        $scope.$watch("user."+key, function(nval, oval) {
            if (_.isEqual(nval, oval)) {
                return;
            }

            if (updateState.timeout !== null) {
                $timeout.cancel(updateState.timeout);
                updateState.timeout = null;
            }

            updateState.timeout = $timeout(function() {
                var upd = {};
                upd[key] = nval;
                model.user($scope.user.id).set(upd);         
                updateState.timeout = null;
            }, throttleDelay);
        });
    };

    user.onIdAcquired(function(id) {
        $scope.user = { id : id }; 

        model.user($scope.user.id).get().success(function(response) {
            $scope.user = response;
            //$scope.user.password = "xxxxxxxx";
            // TODO: This works around bad formed output of server.
            //       Should be fixed there...
            if ($scope.user.email.length === 0) {
                $scope.user.email = "";
            }
            else {
                $scope.user.email = $scope.user.email[0];
            }

            throttledUpdate("name");
            throttledUpdate("description");
            throttledUpdate("email");
        });
    });
}]);
