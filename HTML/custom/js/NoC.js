angular.module("NoC", 
    [ "ngRoute"
    , "NoC.channel"
    , "NoC.channel-overview"
    , "NoC.contact-overview"
    , "NoC.profile"
    , "NoC.my-profile"
    , "NoC.error"
    , "NoC.topbar"
    , "NoC.login"
    , "NoC.notifications"
    , "NoC.filters"
    , "NoC.model"
    , "NoC.user" 
    , "API.utilities"
    , "sticky"
    , "autoresize"
    , "update"
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
            .when("/error",
                { templateUrl : "partials/error.html"
                , controller : "error-controller"
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
.factory('unauthInterceptor', ['$q', '$rootScope', '$location', function($q, $rootScope, $location) {
    var STATUS_UNAUTHORIZED = 401;
    var unauthInterceptor =
        { 'responseError' : function(rejection) {
                if (rejection.status === STATUS_UNAUTHORIZED) {
                    var deferred = $q.defer();
                    var request = { config : rejection.config
                                  , deferred : deferred
                                  };

                    $rootScope.deferred401.push(request);
                    $rootScope.$broadcast("user-login-required");
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

.run(['$rootScope', '$http', '$location', "user", 
       function($rootScope, $http, $location, user) {
    "use strict";

    $rootScope.deferred401 = [];
    $rootScope.deferredRoute = "";

    user.onLoginSuccessfull(function() {
        $location.path($rootScope.deferredRoute);
        _.map($rootScope.deferred401, function(req) {
            $http(req.config).then(function(response) {
                req.deferred.resolve(response);
            });
        }); 
    });

    user.onLoginRequired(function() {
        $rootScope.deferredRoute = $location.path();
        $location.path("/login");
    });
}])

// redirect to personal profile if path goes to current
// users profile.
.run(["$rootScope", "$location", "user",
        function($rootScope, $location, user) {
    "use strict";

    $rootScope.$on("$routeChangeStart", function(event, _, __) {
        if($location.path() == "/user/"+user.getId()) {
            event.preventDefault();
            $location.path("/profile");
        }
    });

    user.onIdAcquired(function(id) {
        if($location.path() == "/user/"+id) {
            $location.path("/profile");
        }
    });
}])

// redirect to login, if page is personal profile and
// user is not logged in.
.run(["$rootScope", "$location", "user",
        function($rootScope, $location, user) {
    "use strict";

    $rootScope.$on("$routeChangeStart", function(event, _, __) {
        if($location.path() == "/profile" && user.getId() === null) {
            $rootScope.deferredRoute = "/profile";
            event.preventDefault();
            $location.path("/login");
        }
    });
}])

// redirect to personal profile if user is logged in and wants
// to go to login
.run(["$rootScope", "$location", "user",
        function($rootScope, $location, user) {
    "use strict";

    $rootScope.$on("$routeChangeStart", function(event, _, __) {
        if($location.path() == "/login" && user.getId() !== null) {
            event.preventDefault();
            $location.path("/profile");
        }
    });

    user.onIdAcquired(function(id) {
        if($location.path() == "/login") {
            $location.path("/profile");
        }
    });
}])

.run(["$window", "user", function($window, user) {
    user.onLogout(function() {
        $window.location.reload();
    });
}])

;
