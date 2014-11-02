angular.module("NoC.topbar", []).
controller("topbar-controller", [ "$scope", "model", "user"
         , function($scope, model, user) {
    "use strict";

    $scope.user = null; 
    $scope.notifications = [];

    user.onIdAcquired(function(id) {
        model.user(id).onUpdate(function(response) {
            $scope.user = response;
        });
        model.user(id).notifications.onUpdate(function(response) {
            $scope.notifications = response.notifications;
        });

        model.user(id).update();
        model.user(id).notifications.update();
    });

    $scope.logout = function() {
        user.logout();
    };
}]);
