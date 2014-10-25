angular.module("NoC.channel-overview", []).
controller("channel-overview-controller", [ "$rootScope", "$scope", "model", "user", "user-events"
         , function($rootScope, $scope, model, user, userEvents) {
    "use strict";
    
    $rootScope.$on(userEvents.idAcquired, function() {
        model.user(user.getId()).subscriptions.onChange(function(data) {
            console.log(data);
            $scope.channels = data.subscriptions;
        });

        model.user(user.getId()).subscriptions.update();
    });
}]);
