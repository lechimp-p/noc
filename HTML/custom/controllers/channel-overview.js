angular.module("NoC.channel-overview", []).
controller("channel-overview-controller", [ "$scope", "model", "user"
         , function($scope, model, user) {
    "use strict";
    
    user.onIdAcquired(function(id) {
        model.user(id).subscriptions.onUpdate(function(data) {
            $scope.channels = data.subscriptions;
        });

        model.user(id).subscriptions.update();
    });
}]);
