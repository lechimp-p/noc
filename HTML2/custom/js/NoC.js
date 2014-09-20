angular.module("NoC", 
    [ "ngRoute"
    , "NoC.channel"
    , "NoC.login"
    , "NoC.services"
    , "mobile-angular-ui"
    ])

.config([ '$routeProvider', function($routeProvider) {
        $routeProvider
            .when("/login", { templateUrl: "partials/login.html"
                            , controller : "login-controller"
                            })
            ;
}])

// Let user login when login is required for some action.
// Intercept 401 (unauthorized) responses from server, send a
// login required event and resend the requests after successfull
// login.
.factory('unauthInterceptor', ['$q', '$rootScope', function($q, $rootScope) {
    var unauthInterceptor =
        { 'responseError' : function(rejection) {
                console.log(rejection);
                if (rejection.status === 401) {
                    var deferred = $q.defer();
                    var request = { config : rejection.config
                                  , deferred : deferred
                                  };

                    $rootScope.deferred401.push(request);
                    $rootScope.$broadcast("event:login-required");
                    return deferred.promise;
                }
                else {
                    return $q.reject(rejection);
                }
            }
        };
    return unauthInterceptor;
}])

.config([ '$httpProvider', function($httpProvider) {
    $httpProvider.interceptors.push( 'unauthInterceptor');
}])

.run(['$rootScope', '$http', 'API', function($rootScope, $http, API) {
    $rootScope.deferred401 = [];
    
    $rootScope.$on("event:login-confirmed", function() {
        $http(req.config).then(function(response) {
            req.deferred.resolve(reponse);
        }); 
    });

    $rootScope.$on("event:login-required", function() {
        alert("login in....");
        API.login("admin", "admin").success( function(response) {
            $rootScope.$broadcast("event:login-confirmed");
        }); 
    });
}])

;
