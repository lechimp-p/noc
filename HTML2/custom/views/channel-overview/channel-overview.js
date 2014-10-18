angular.module("NoC.channel-overview", []).
controller("channel-overview-controller", [ "$rootScope", "$scope", "API", "user", "user-events"
         , function($rootScope, $scope, API, user, userEvents) {
    "use strict";

    $scope.update = function() {
        API.getSubscriptions(user.getId())
            .success(function(response) {
                $scope.channels = response.subscriptions;
            }); 
    };
    
    $rootScope.$on(userEvents.idAcquired, function() {
        $scope.update();
    });
}]);
