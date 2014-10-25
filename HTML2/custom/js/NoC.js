angular.module("NoC", 
    [ "ngRoute"
    , "NoC.channel"
    , "NoC.channel-overview"
    , "NoC.chat"
    , "NoC.contact-overview"
    , "NoC.profile"
    , "NoC.my-profile"
    , "NoC.login"
    , "NoC.filters"
    , "NoC.model"
    , "NoC.user" 
    , "API.utilities"
    , "sticky"
    , "autoresize"
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
            .when("/chat/:chanId", 
                { templateUrl: "partials/chat.html"
                , controller : "channel-controller"
                })
            .when("/user/:userId",
                { templateUrl : "partials/profile.html"
                , controller : "profile-controller"
                })
            .when("/profile",
                { templateUrl : "partials/my-profile.html"
                , controller : "my-profile-controller"
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
                    $rootScope.$broadcast("user:login-required");
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

.run(['$rootScope', '$http', '$location', "user-events", 
       function($rootScope, $http, $location, userEvents) {
    //$rootScope.deferred401 = [];
    $rootScope.deferredRoute = "";

    $rootScope.$on(userEvents.loginSuccessfull, function() {
        $location.path($rootScope.deferredRoute);
        /*$http(req.config).then(function(response) {
            req.deferred.resolve(reponse);
        });*/ 
    });

    $rootScope.$on(userEvents.loginRequired, function() {
        $rootScope.deferredRoute = $location.path();
        $location.path("/login");
    });
}])

;
