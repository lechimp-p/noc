angular.module("NoC.login", []).
controller("login-controller", ["$scope", "user", function ($scope, user) {
    "use strict";

    $scope.username = "";
    $scope.password = "";

    $scope.login = function(username, password) {
        user.login(username, password);
    };
}])
;
