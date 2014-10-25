angular.module("NoC.user", [])
.factory("user", ["$rootScope", "model", "makeAPICall", "user-events", 
                  function($rootScope, model, makeAPICall, userEvents) {
    "use strict";

    var user_data = { id : null
                    , UTC_offset : 2
                    };

    var user = {};

    makeAPICall("logininfo", "GET", "logininfo", {})
        .success(function(response) {
            user_data.id = response.id;
            $rootScope.$broadcast(userEvents.idAcquired);
        });
    
    user.login = function(login, password) {
        return makeAPICall("login", "POST", "login"
                          , { login : login, password : password })
            .success(function(response) {
                user_data.id = response.id;
                $rootScope.$broadcast(userEvents.idAcquired);
                $rootScope.$broadcast(userEvents.loginSuccessfull);
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
.factory("user-events", function() {
    return { loginRequired : "user:login-required"
           , loginSuccessfull : "user:login-successfull"
           , idAcquired : "user:id-acquired"
           };
})
;
