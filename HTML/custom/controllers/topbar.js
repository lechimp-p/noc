angular.module("NoC.topbar", []).
controller("topbar-controller", [ "$scope", "model", "user"
         , function($scope, model, user) {
    "use strict";

    $scope.user = null; 

    user.onIdAcquired(function(id) {
        model.user(id).onUpdate(function(response) {
            $scope.user = response;
        });

        model.user(id).update();
    });

    $scope.logout = function() {
        user.logout();
    };
}]);
