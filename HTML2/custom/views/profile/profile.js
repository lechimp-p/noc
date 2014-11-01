angular.module("NoC.profile", []).
controller("profile-controller", [ "$scope", "$routeParams", "model"
         , function($scope, $routeParams, model) {
    "use strict";

    $scope.user = { id : $routeParams.userId };

    $scope.addToContacts = function() {
    };

    $scope.removeFromContacts = function() {
    };

    model.user($scope.user.id).onUpdate(function(response) {
        $scope.user = response;
    });

    model.user($scope.user.id).update();
}]);
