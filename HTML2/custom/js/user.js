angular.module("NoC.user", [])
.factory("user", ["$rootScope", "model", "makeAPICall",
                  function($rootScope, model, makeAPICall) {
    "use strict";

    var loginRequired = "user-login-required";
    var loginSuccessfull = "user-login-successfull";
    var logoutSuccessfull = "user-logout";
    var idAcquired = "user-id-acquired";

    var user_data = { id : null
                    , UTC_offset : 2
                    };

    var user = {};

    user.onLoginRequired = function(fun) {
        $rootScope.$on(loginRequired, function(event, id) {
            fun(id);
        });
    };

    user.onLoginSuccessfull = function(fun) {
        $rootScope.$on(loginSuccessfull, function(event, id) {
            fun(id);
        });
    };

    user.onLogout = function() {
        $rootScope.$on(logoutSuccessfull, function() {
            fun();
        });
    };

    user.onIdAcquired = function(fun) {
        $rootScope.$on(idAcquired, function(event, id) {
            fun(id);
        });
    };

    makeAPICall("logininfo", "GET", "logininfo", {})
        .success(function(response) {
            user_data.id = response.id;
            $rootScope.$emit(idAcquired, response.id);
        });
    
    user.login = function(login, password) {
        return makeAPICall("login", "POST", "login"
                          , { login : login, password : password })
            .success(function(response) {
                user_data.id = response.id;
                $rootScope.$emit(idAcquired, reponse.id);
                $rootScope.$emit(loginSuccessfull, response.id);
            });    
    };

    user.logout = function() {
        model.flushCache();
        user_data.id = null;
        user_data.UTC_offset = 2;
        return makeAPICall("logout", "POST", "logout", {});
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