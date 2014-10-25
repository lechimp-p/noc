angular.module("NoC.contact-overview", []).
controller("contact-overview-controller", [ "$scope", "model", "user",
         function($scope, model, user) {
    "use strict";
    
    user.onIdAcquired(function(id) {
        model.user(id).contacts.onChange(function(response) {
            $scope.contacts = response.contacts;
        });

        model.user(id).contacts.update();
    });
}]);
