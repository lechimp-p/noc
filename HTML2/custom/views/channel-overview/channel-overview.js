angular.module("NoC.channel-overview", []).
controller("channel-overview-controller", [ "$rootScope", "$scope", "model", "user", "user-events"
         , function($rootScope, $scope, model, user, userEvents) {
    "use strict";

    model.user(user.getId()).onChange(function(data) {
        console.log(data);
        $scope.channels = data;
    });
    
    $rootScope.$on(userEvents.idAcquired, function() {
        model.user(user.getId()).update();
    });
}]);
