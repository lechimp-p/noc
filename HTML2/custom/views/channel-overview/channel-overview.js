angular.module("NoC.channel-overview", []).
controller("channel-overview-controller", [ "$scope", "model", "user"
         , function($scope, model, user) {
    "use strict";
    
    user.onIdAcquired(function(id) {
        model.user(id).subscriptions.onChange(function(data) {
            $scope.channels = data.subscriptions;
        });

        model.user(user.getId()).subscriptions.update();
    });
}]);
