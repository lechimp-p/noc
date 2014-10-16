angular.module("NoC.services", []).
factory("API", function($http) {
    "use strict";

    var API = {};

    var makeAPICall = function(name, method, endpoint, data) {
        var config = 
            { method : method
            , url : "api/" + endpoint
            };
        if (method == "POST") {
            config.data = JSON.stringify(data);
            config.headers = {"Content-Type" : "application/json"};
        }
        else if (method == "GET") {
            config.params = data;
        }
        return $http( config )
                .error( function(data, status, headers, config) {
                    console.log("Error in API." + name + ": " + data);
                });
    };

    API.login = function(login, password) {
        return makeAPICall("login", "POST", "login",
                            { "login" : login
                            , "password" : password  
                            });
    };

    API.logout = function() {
        return makeAPICall("logout", "POST", "logout", {});
    };

    API.logininfo = function() {
        return makeAPICall("logininfo", "GET", "logininfo", {});
    };

    API.messages = function(cid, offset, amount) {
        return makeAPICall("messages", "GET", "channel/" + cid  + "/messages",
                            { "offset" : offset 
                            , "amount" : amount 
                            });
    };

    API.messagesTill = function(cid, ts) {
        return makeAPICall("messagesTill", "GET", "channel/" + cid + "/messages",
                            { "timestamp" : ts
                            });
    };
   
    API.post = function(cid, text) {
        return makeAPICall("post", "POST", "channel/" + cid + "/messages",
                            { "text" : text 
                            });
    };

    API.subscribe = function(uid, cid) {
        return makeAPICall("subscribe", "POST", "user/" + uid + "/subscriptions",
                            { "subscribe" : [cid]
                            });
    };

    API.unsubscribe = function(uid, cid) {
        return makeAPICall("unsubscribe", "POST", "user/" + uid + "/subscriptions",
                            { "unsubscribe" : [cid]
                            });
    };

    API.getChannelInfo = function(cid) {
        return makeAPICall("getChahnelInfo", "GET", "channel/"+cid, {});
    }; 

    return API;
});
