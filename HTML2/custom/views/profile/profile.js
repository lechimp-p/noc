angular.module("NoC.profile", []).
controller("profile-controller", [ "$scope", "$routeParams", "user", "model"
         , function($scope, $routeParams, user, model) {
    "use strict";

    $scope.user = { id : $routeParams.userId };

    $scope.addToContacts = function() {
        model.user(user.getId()).contacts.set([$sope.user.id], []);
        model.user($scope.user.id).update();
    };

    $scope.removeFromContacts = function() {
        model.user(user.getId()).contacts.set([], [$sope.user.id]);
    };

    model.user($scope.user.id).onUpdate(function(response) {
        $scope.user = response;
    });

    model.user($scope.user.id).update();
}]);
