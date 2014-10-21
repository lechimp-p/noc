angular.module("NoC.model", [])
.factory("model", ["$q", "$timeout", "makeAPICall", function($q, $timeout, makeAPICall) {
    "use strict";

    ////////////
    // Utilities
    ////////////
    
    var makeCachePromise = function(content) {
        var deferred = $q.defer();

        $timeout(function() {
            deferred.resolve(content);
        });
        
        return deferred.promise;
    };

    var makeCachedAPICall = function(name, method, path, params, cache, prop) {
        return makeAPICall(name, method, path, params) 
                    .success(function(response) {
                        cache[prop] = response;
                    });
    };

    var makeCachePromiseOrAPICall = function(name, method, path, params, cache, prop) {
        if (cache.hasOwnProperty(prop)) {
            return makeCachePromise(cache[prop]);
        }

        return makeCachedAPICall(name, method, path, params, cache, prop);
    };

    var makeAPICallClearCache = function(name, method, path, params, cache, prop) {
        return makeAPICall(pr+".set", "POST", pa, data)
                .success(function(_) {
                    delete cache[prop];
                });
    };

    ////////    
    // Model
    ////////

    var root = {};
    var __ = { users : {}
            , channels : {}
            };
    root.flushCache = function() {
        __.users = {};
        __.channels = {};
    };


    // User

    root.user = function(uid) {
        if (__.users.hasOwnProperty(uid)) {
            return __.users[uid];
        }
        var user = { id : uid };
        var _ = { cache : {} };        
        var pr = "user["+uid+"]";
        var pa = "user/"+uid;

        user.flushCache = function() {
            _.cache = {};
        };        

        user.get = function() {
            return makeCachePromiseOrAPICall( pr+".get", "GET", pa
                                            , {}, _.cache, "get");
        };

        user.set = function(data) {
            return makeAPICallClearCache(pr+".set", "POST", pa
                                        , data, _.cache, "get");
        };

        user.update = function() {
            return makeCachedAPICall(pr+".update", "GET", pa
                                    , {}, _.cache, "get");
        };

        user.contacts = {};
        
        user.contacts.get = function() {
            return makeCachePromiseOrAPICall( pr+".contacts.get", "GET"
                                            , pa+"/contacts"
                                            , {}, _.cache, "contacts");
        };

        user.contacts.set = function(set, remove) {
            return makeAPICallClearCache( pr+".contacts.set", "POST"
                                        , pa+"/contacts"
                                        , { set : set, remove : remove}
                                        , _.cache, "contacts");
        };

        user.contacts.update = function() {
            return makeCachedAPICall( pr+".contacts.update", "GET"
                                    , pa+"/contacts"
                                    , {}, _.cache, "contacts");
        };

        user.subscriptions = {};

        user.subscriptions.get = function() {
            return makeCachePromiseOrAPICall( pr+".subscriptions.get", "GET"
                                            , pa+"/subscriptions"
                                            , {}, _.cache, "subscriptions"); 
        };

        user.subscriptions.set = function(subscribe, unsubscribe) {
            return makeAPICallClearCache( pr+".subscriptions.set", "POST"
                                        , pa+"/subscriptions"
                                        , { subscribe : subscribe, unsubscribe : unsubscribe }
                                        , _.cache, "subscriptions");
        };

        user.subscriptions.update = function() {
            return makeCachedAPICall( pr+".subscriptions.update", "GET"
                                    , pa+"/subscriptions"
                                    , {}, _.cache, "subscriptions"); 
        };

        user.notifications = {};

        user.notifications.get = function() {
            return makeCachePromiseOrAPICall( pr+".notifications.get", "GET"
                                            , pa+"/notifications"
                                            , {}, _.cache, "notifications"); 
        };

        user.notifications.update = function() {
            return makeCachedAPICall( pr+".notifications.get", "GET"
                                    , pa+"/notifications"
                                    , {}, _.cache, "notifications"); 
        };

        __.users[uid] = user;
        return user;
    };

    root.user.search = function(login) {
        return makeAPICall("user.search", "GET", "user", { login : login });
    };


    // Channel

    root.channel = function(cid) {
        if (__.channels.hasOwnProperty(cid)) {
            return __.channels[cid];
        }

        var channel = { id : cid };
        var _ = { cache : {} };
        var pr = "channel["+cid+"]";
        var pa = "channel/"+cid; 

        channel.flushCache = function() {
            _.cache = {};
        };

        channel.get = function() {
            return makeCachePromiseOrAPICall( pr+".get", "GET", pa
                                            , {}, _.cache, "get");
        }; 

        channel.set = function(data) {
            return makeAPICallClearCache( pr+".set", "POST", pa
                                        , data, _.cache, "get");
        };

        channel.update = function() {
            return makeCachedAPICall( pr+".set", "GET", pa
                                    , {}, _.cache, "get");
        };

        channel.messages = function(offset, amount) {
            return makeAPICall( pr+".messages", "GET", pa+"/messages"
                              , { offset : offset, amount : amount });
        };

        channel.messagesTill = function(timestamp) {
            return makeAPICall( pr+".messagesTill", "GET", pa+"/messages"
                              , { timestamp : timestamp });
        };

        channel.post = function(text) {
            return makeAPICall( pr+".post", "POST", pa+"/messages"
                              , { text : text });
        };

        __.channels[cid] = channel;
        return channel;
    };

    root.channel.search = function() {
        return makeAPICall("channel.search", "GET", "channel", {});
    };

    root.channel.create = function(name) {
        return makeAPICall("channel.create", "POST", "channel"
                          , { name : name });
    };

    return root;
}])
;
