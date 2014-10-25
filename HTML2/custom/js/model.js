angular.module("NoC.model", [])
.factory("model", ["$rootScope", "$q", "$timeout", "makeUnwrappedAPICall", 
                    function($rootScope, $q, $timeout, makeUnwrappedAPICall) {
    "use strict";

    ////////////
    // Utilities
    ////////////

    var addHttpInterface = function(promise) {
        promise.success = function(fun) {
        
        return promise;
    }
   
    var makeCachePromise = function(content) {
        var deferred = $q.defer();

        $timeout(function() {
            deferred.resolve(content);
        });

        var promise = deferred.promise;
        promise.success = function(fun) {
            var pr = promise.then(fun);
        };
        
        return deferred.promise;
    };

    var makeCachedAPICall = function(name, method, path, params, cache, prop) {
        return makeUnwrappedAPICall(name, method, path, params) 
                    .then(function(response) {
                        cache[prop] = response;
                        return response;
                    });
    };

    var makeCachePromiseOrAPICall = function(name, method, path, params, cache, prop) {
        if (cache.hasOwnProperty(prop)) {
            return makeCachePromise(cache[prop]);
        }

        return makeCachedAPICall(name, method, path, params, cache, prop);
    };

    var makeUnwrappedAPICallClearCache = function(name, method, path, params, cache, prop) {
        return makeUnwrappedAPICall(name, method, path, params)
                .then(function(response) {
                    delete cache[prop];
                    return response; 
                });
    };


    /////////
    // Events
    /////////

    var channelChangedEvent = "channel-changed";
    var contactsChangedEvent = "contacts-changed";
    var subscriptionsChangedEvent = "subscriptions-changed";
    var notificationsChangedEvent = "notifications-changed";
    var userChangedEvent = "user-changed";

    // Helper function for channel and user to create
    // functions that register eventlisteners. 
    var callOnMatchingId = function(ev, id, fun) {
        $rootScope.$on(ev, function(event, _id, data) {
            if (id === _id) {
                fun(event, data);
            }
        });
    };

    // Helper to emit events.
    var emitIt = function(ev, id, data) {
        return function(_) {
            $rootScope.$emit(ev, id, data);
        };
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

        // Get promise to user information, from cache or server.
        user.get = function() {
            return makeCachePromiseOrAPICall( pr+".get", "GET", pa
                                            , {}, _.cache, "get");
        };

        // Get promise to set user information and clear cache.
        // Afterwards update.
        user.set = function(data) {
            return makeUnwrappedAPICallClearCache(pr+".set", "POST", pa
                                        , data, _.cache, "get")
                    .then(function(response) {
                        user.update()
                        return response;
                    });
        };

        // Get promise to user information from server and update cache.
        // Afterwards emit user.onChange.
        user.update = function() {
            return makeCachedAPICall(pr+".update", "GET", pa
                                    , {}, _.cache, "get")
                    .then(emitIt(userChangedEvent, user.id, _.cache.get));
        };

        // Be informed when user information changes.
        user.onChange = function(fun) {
            return callOnMatchingId(userChangedEvent, user.id, fun);
        };

        user.contacts = {};
        
        user.contacts.get = function() {
            return makeCachePromiseOrAPICall( pr+".contacts.get", "GET"
                                            , pa+"/contacts"
                                            , {}, _.cache, "contacts");
        };

        user.contacts.set = function(set, remove) {
            return makeUnwrappedAPICallClearCache( pr+".contacts.set", "POST"
                                        , pa+"/contacts"
                                        , { set : set, remove : remove}
                                        , _.cache, "contacts")
                    .then(user.contacts.update);
        };

        user.contacts.update = function() {
            return makeCachedAPICall( pr+".contacts.update", "GET"
                                    , pa+"/contacts"
                                    , {}, _.cache, "contacts")
                    .then(emitIt(contactsChangedEvent, user.id));
        };

        user.contacts.onChange = function(fun) {
            return callOnMatchingId(contactsChangedEvent, user.id, fun);
        };

        user.subscriptions = {};

        user.subscriptions.get = function() {
            return makeCachePromiseOrAPICall( pr+".subscriptions.get", "GET"
                                            , pa+"/subscriptions"
                                            , {}, _.cache, "subscriptions"); 
        };

        user.subscriptions.set = function(subscribe, unsubscribe) {
            return makeUnwrappedAPICallClearCache( pr+".subscriptions.set", "POST"
                                        , pa+"/subscriptions"
                                        , { subscribe : subscribe, unsubscribe : unsubscribe }
                                        , _.cache, "subscriptions")
                    .then(user.subscriptions.update);
        };

        user.subscriptions.update = function() {
            return makeCachedAPICall( pr+".subscriptions.update", "GET"
                                    , pa+"/subscriptions"
                                    , {}, _.cache, "subscriptions")
                    .then(emitIt(subscriptionsChangedEvent, user.id, _.cache.subscriptions));
        };

        user.subscriptions.onChange = function(fun) {
            return callOnMatchingId(subscriptionsChangedEvent, user.id, fun);
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
                                    , {}, _.cache, "notifications")
                    .then(emitIt(notificationsChangedEvent, user.id));
        };

        user.notifications.onChange = function(fun) {
            return callOnMatchingId(notificationsChangedEvent, user.id, fun);
        };

        __.users[uid] = user;
        return user;
    };

    root.user.search = function(login) {
        return makeUnwrappedAPICall("user.search", "GET", "user", { login : login });
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
            return makeUnwrappedAPICallClearCache( pr+".set", "POST", pa
                                        , data, _.cache, "get");
        };

        channel.update = function() {
            return makeCachedAPICall( pr+".set", "GET", pa
                                    , {}, _.cache, "get");
        };

        channel.onChange = function(fun) {
            return callOnMatchingId(channelChangedEvent, channel.id, fun);
        };

        channel.messages = function(offset, amount) {
            return makeUnwrappedAPICall( pr+".messages", "GET", pa+"/messages"
                              , { offset : offset, amount : amount });
        };

        channel.messagesTill = function(timestamp) {
            return makeUnwrappedAPICall( pr+".messagesTill", "GET", pa+"/messages"
                              , { timestamp : timestamp });
        };

        channel.post = function(text) {
            return makeUnwrappedAPICall( pr+".post", "POST", pa+"/messages"
                              , { text : text });
        };

        __.channels[cid] = channel;
        return channel;
    };

    root.channel.search = function() {
        return makeUnwrappedAPICall("channel.search", "GET", "channel", {});
    };

    root.channel.create = function(name) {
        return makeUnwrappedAPICall("channel.create", "POST", "channel"
                          , { name : name });
    };

    

    return root;
}])
;
