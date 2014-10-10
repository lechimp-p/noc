angular.module("NoC.services", []).
factory("API", function($http) {
    "use strict";

    var API = {};

    API.login = function(login, password) {
        return $http(
                { method : "POST"
                , url : "api/login"
                , data : JSON.stringify(
                            { "login" : login
                            , "password" : password  
                            })
                , headers : {"Content-Type" : "application/json"}
                })
                .error( function(data, status, headers, config) {
                    console.log("Error in API.login: " + data);
                })
                ;
    };

    API.messages = function(cid, offset, amount) {
        return $http(
                { method : "GET"
                , url : "api/channel/"+cid+"/messages"
                , params : 
                    { "offset" : offset 
                    , "amount" : amount 
                    }
                })
                .error( function(data, status, headers, config) {
                    console.log("Error in API.messages: " + data);
                })
                ;
    };

    API.messagesTill = function(cid, ts) {
        return $http(
                { method : "GET"
                , url : "api/channel/"+cid+"/messages"
                , params :
                    { "timestamp" : ts
                    }
                })
                .error( function(data, status, headers, config) {
                    console.log("Error in API.messagesTill: " + data);
                })
                ;
    };
   
    API.post = function(cid, text) {
        console.log(cid + " " + text);
        return $http(
                { method : "POST"
                , url : "api/channel/"+cid+"/messages"
                , data : JSON.stringify(
                            { "text" : text 
                            })
                , headers : {"Content-Type" : "application/json"}
                })
                .error( function(data, status, headers, config) {
                    console.log("Error in API.post: " + data);
                })
                ;
    };

    API.getChannelInfo = function(cid) {
        return $http(
                { methof : "GET"
                , url : "api/channel/"+cid
                , params : {}
                })
                .error( function(data, status, headers, config) {
                    console.log("Error in API.getChannelInfo: " + data);
                })
                ;
    }; 

    return API;
});
