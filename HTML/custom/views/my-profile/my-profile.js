angular.module("NoC.my-profile", []).
controller("my-profile-controller", [ "$scope", "model", "user"
         , function($scope, model, user) {
    "use strict";

    user.onIdAcquired(function(id) {
        $scope.user = { id : id }; 

        model.user($scope.user.id).onUpdate(function(response) {
            $scope.user = response;
            // TODO: This works around bad formed output of server.
            //       Should be fixed there...
            if ($scope.user.email.length === 0) {
                $scope.user.email = "";
            }
            else {
                $scope.user.email = $scope.user.email[0];
            }
        });

        model.user($scope.user.id).update();
    });
}]);
