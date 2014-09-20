angular.module("NoC", 
    [ "ngRoute"
    , "NoC.controllers"
    , "NoC.services"
    , "mobile-angular-ui"
    ]).
config(
    [ '$routeProvider', function($routeProvider) {
        $routeProvider
            .when("/login", { templateUrl: "partials/login.html"
                            , controller : "login-controller"
                            })
            ;
        }
    ]);
