angular.module("NoC.login", []).

controller("login-controller", function ($scope, $rootScope, API) {
    "use strict";

    $scope.username = "";
    $scope.password = "";

    $scope.login = function(username, password) {
        API.login(username, password)
            .success(function(_) {
                $rootScope.$broadcast("event:login-successfull");
            })
            .error(function(_, status, _, _) {
            })
            ;        
    };
})

;



