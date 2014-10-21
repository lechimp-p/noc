angular.module("NoC.channel-overview", []).
controller("channel-overview-controller", [ "$rootScope", "$scope", "model", "user", "user-events"
         , function($rootScope, $scope, model, user, userEvents) {
    "use strict";

    $scope.update = function() {
        model.user(user.getId())
            .subscriptions.get()
                .success(function(response) {
                    $scope.channels = response.subscriptions;
                }); 
    };
    
    $rootScope.$on(userEvents.idAcquired, function() {
        $scope.update();
    });
}]);
