angular.module("NoC.notifications", []).
controller("notifications-controller", [ "$scope", "model", "user"
         , function($scope, model, user) {
    "use strict";

    $scope.notifications = [];

    var updateHandler = { deregister : null };

    user.onLogout(function() {
        $scope.notifications = [];
        if (updateHandler.deregister !== null) {
            updateHandler.deregister();
            updateHandler.deregister = null;
        }
    });

    user.onIdAcquired(function(id) {
        updateHandler.deregister =
        model.user(id).notifications.onUpdate(function(response) {
            $scope.notifications = response.notifications;
        });
        model.user(id).notifications.update();
    });
}]);
