angular.module("NoC.login", []).

controller("login-controller", function ($scope, $rootScope, API) {
    "use strict";

    $scope.username = "";
    $scope.password = "";

    $scope.login = function(username, password) {
        API.login(username, password)
            .success(function(result) {
                $rootScope.user.id = result.id;
                $rootScope.$broadcast("event:login-successfull");
            })
            .error(function(_1, status, _2, _3) {
                // TODO: do proper error handling...
                alert("Could not login...");
            })
            ;        
    };
})

;



