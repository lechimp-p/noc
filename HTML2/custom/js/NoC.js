angular.module("NoC", 
    [ "ngRoute"
    , "NoC.channel"
    , "NoC.login"
    , "NoC.services"
    , "NoC.filters"
    , "mobile-angular-ui"
    ])

.config([ '$routeProvider', function($routeProvider) {
        $routeProvider
            .when("/login", 
                { templateUrl: "partials/login.html"
                , controller : "login-controller"
                })
            .when("/channel/:chanId", 
                { templateUrl: "partials/channel.html"
                , controller : "channel-controller"
                })
            .otherwise(
                { redirectTo : "/login"
                })
            ;
}])

// Let user login when login is required for some action.
// Intercept 401 (unauthorized) responses from server, send a
// login required event and resend the requests after successfull
// login.
.factory('unauthInterceptor', ['$q', '$rootScope', function($q, $rootScope) {
    var STATUS_UNAUTHORIZED = 401;
    var unauthInterceptor =
        { 'responseError' : function(rejection) {
                console.log(rejection);
                if (rejection.status === STATUS_UNAUTHORIZED) {
//                    var deferred = $q.defer();
//                    var request = { config : rejection.config
//                                  , deferred : deferred
//                                  };

//                    $rootScope.deferred401.push(request);
                    $rootScope.$broadcast("event:login-required");
//                    return deferred.promise;
                }
//                else {
                    return $q.reject(rejection);
//                }
            }
        };
    return unauthInterceptor;
}])

.config([ '$httpProvider', function($httpProvider) {
    $httpProvider.interceptors.push( 'unauthInterceptor');
}])

.run(['$rootScope', '$http', '$location', 'API', function($rootScope, $http, $location, API) {
    //$rootScope.deferred401 = [];
    $rootScope.deferredRoute = "";
    $rootScope.user = { id : null 
                      , UTC_offset : 2
                      };

    API.logininfo().success(function(response) {
        $rootScope.user.id = response.id;
    }); 
    
    $rootScope.$on("event:login-successfull", function() {
        $location.path($rootScope.deferredRoute);
        /*$http(req.config).then(function(response) {
            req.deferred.resolve(reponse);
        });*/ 
    });

    $rootScope.$on("event:login-required", function() {
        $rootScope.deferredRoute = $location.path();
        $location.path("/login");
    });
}])

;
