angular.module("NoC.user", [])
.factory("user", ["$rootScope", "model", "makeAPICall",
                  function($rootScope, model, makeAPICall) {
    "use strict";

    var loginRequired = "user-login-required";
    var loginSuccessfull = "user-login-successfull";
    var logoutSuccessfull = "user-logout";
    var idAcquired = "user-id-acquired";

    var user_data = { id : null
                    , UTC_offset : 1 
                    };

    var user = {};

    user.onLoginRequired = function(fun) {
        return $rootScope.$on(loginRequired, function(event, id) {
            fun(id);
        });
    };

    user.loginRequired = function() {
        $rootScope.$broadcast(loginRequired);
    };

    user.onLoginSuccessfull = function(fun) {
        return $rootScope.$on(loginSuccessfull, function(event, id) {
            fun(id);
        });
    };

    user.onLogout = function(fun) {
        return $rootScope.$on(logoutSuccessfull, fun);
    };

    user.onIdAcquired = function(fun) {
        if (user_data.id !== null) {
            fun(user_data.id);
        }

        return $rootScope.$on(idAcquired, function(event, id) {
            fun(id);
        });
    };

    makeAPICall("logininfo", "GET", "logininfo", {})
        .success(function(response) {
            user_data.id = response.id;
            if (response.id !== null) {
                $rootScope.$emit(idAcquired, response.id);
            }
        });
    
    user.login = function(login, password) {
        return makeAPICall("login", "POST", "login"
                          , { login : login, password : password })
            .success(function(response) {
                user_data.id = response.id;
                $rootScope.$emit(idAcquired, response.id);
                $rootScope.$emit(loginSuccessfull, response.id);
            });    
    };

    user.logout = function() {
        return makeAPICall("logout", "POST", "logout", {})
                .success(function() {
                    model.flushCache();
                    user_data.id = null;
                    user_data.UTC_offset = 2;
                    $rootScope.$emit(logoutSuccessfull);
                });
    }; 

    user.subscribe = function(cid) {
        return model.user(user_data.id)
                .subscriptions.set([cid], []);
    };

    user.unsubscribe = function(cid) {
        return model.user(user_data.id)
                .subscriptions.set([], [cid]);
    };


    user.getId = function() {
        return user_data.id;
    };

    user.getUTC_offset = function() {
        return user_data.UTC_offset;
    };

    return user;
}])
;
