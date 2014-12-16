angular.module("NoC.profile", []).
controller("profile-controller", [ "$scope", "$routeParams", "user", "model"
         , function($scope, $routeParams, user, model) {
    "use strict";

    $scope.user = { id : $routeParams.userId };
    $scope.adding = false;
    $scope.removing = false;
    $scope.addRemoveError = false;

    $scope.addToContacts = function() {
        $scope.adding = true;
        $scope.removing = false;
        $scope.addRemoveError = false;
        model.user(user.getId()).contacts.set([$scope.user.id], [])
            .success(function() {
                model.user($scope.user.id).update();
                $scope.adding = false;
            })
            .error(function() {
                $scope.adding = false;
                $scope.addRemoveError = true;
            })
            ;
    };

    $scope.removeFromContacts = function() {
        $scope.adding = false;
        $scope.removing = true;
        $scope.addRemoveError = false;
        model.user(user.getId()).contacts.set([], [$scope.user.id])
            .success(function() {
                $scope.removing = false;
                model.user($scope.user.id).update();
            })
            .error(function() {
                $scope.removing = false;
                $scope.addRemoveError = true;
            })
            ;
    };

    model.user($scope.user.id).onUpdate(function(response) {
        $scope.user = response;
    });

    model.user($scope.user.id).update();
}]);
