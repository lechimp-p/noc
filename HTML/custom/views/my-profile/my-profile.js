angular.module("NoC.my-profile", []).
controller("my-profile-controller", [ "$scope", "model", "user"
         , function($scope, model, user) {
    "use strict";

    user.onIdAcquired(function(id) {
        $scope.user = { id : id }; 

        model.user($scope.user.id).onUpdate(function(response) {
            $scope.user = response;
        });

        model.user($scope.user.id).update();
    });
}]);
