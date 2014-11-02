angular.module("NoC.topbar", []).
controller("topbar-controller", [ "$scope", "model", "user"
         , function($scope, model, user) {
    "use strict";

    $scope.user = null; 
    $scope.notifications = [];
    var updateHandlers = [];

    var releaseUpdateHandlers = function() {
        _.map(updateHandlers, function(release) {
            release();
        }); 
    };

    user.onIdAcquired(function(id) {
        releaseUpdateHandlers();

        updateHandlers.push(
            model.user(id).onUpdate(function(response) {
                $scope.user = response;
            })
        );

        updateHandlers.push(
            model.user(id).notifications.onUpdate(function(response) {
                $scope.notifications = response.notifications;
            })
        );

        model.user(id).update();
        model.user(id).notifications.update();
    });

    $scope.logout = function() {
        user.logout();
    };

    user.onLogout(function() {
        $scope.user = null;
        $scope.notifications = [];
        releaseUpdateHandlers();
    });
}]);
