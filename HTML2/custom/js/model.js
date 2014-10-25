angular.module("NoC.model", [])
.factory("model", ["$rootScope", "$q", "$timeout", "makeAPICall", 
                    function($rootScope, $q, $timeout, makeAPICall) {
    "use strict";

    ////////////
    // Utilities
    ////////////

    var addHttpInterface = function(promise) {
        promise.success = function(fun) {
            var pr = promise.then(fun);
            return addHttpInterface(pr);
        };
        
        return promise;
    };
   
    var makeCachePromise = function(content) {
        var deferred = $q.defer();

        $timeout(function() {
            deferred.resolve(content);
        });

        return addHttpInterface(deferred.promise);
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
        return makeAPICall(name, method, path, params)
                .success(function(_) {
                    delete cache[prop];
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
                fun(data);
            }
        });
    };

    // Helper to emit events.
    var emitIt = function(ev, id) {
        return function(response) {
            console.log(ev+" "+id);
            console.log(response);
            $rootScope.$emit(ev, id, response);
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
            return makeAPICallClearCache(pr+".set", "POST", pa
                                        , data, _.cache, "get")
                    .success(user.update);
        };

        // Get promise to user information from server and update cache.
        // Afterwards emit user.onChange.
        user.update = function() {
            return makeCachedAPICall(pr+".update", "GET", pa
                                    , {}, _.cache, "get")
                    .success(emitIt(userChangedEvent, user.id));
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
            return makeAPICallClearCache( pr+".contacts.set", "POST"
                                        , pa+"/contacts"
                                        , { set : set, remove : remove}
                                        , _.cache, "contacts")
                    .success(user.contacts.update);
        };

        user.contacts.update = function() {
            return makeCachedAPICall( pr+".contacts.update", "GET"
                                    , pa+"/contacts"
                                    , {}, _.cache, "contacts")
                    .success(emitIt(contactsChangedEvent, user.id));
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
            return makeAPICallClearCache( pr+".subscriptions.set", "POST"
                                        , pa+"/subscriptions"
                                        , { subscribe : subscribe, unsubscribe : unsubscribe }
                                        , _.cache, "subscriptions")
                    .success(function(_) {
                        // We need to flush the cache of the targeted
                        // channels as well, since their information
                        // changed.
                        angular.forEach(subscribe.concat(unsubscribe), function(val) {
                            root.channel(val).update();
                        });

                        user.subscriptions.update();
                    });
        };

        user.subscriptions.update = function() {
            return makeCachedAPICall( pr+".subscriptions.update", "GET"
                                    , pa+"/subscriptions"
                                    , {}, _.cache, "subscriptions")
                    .success(emitIt(subscriptionsChangedEvent, user.id));
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
                    .success(emitIt(notificationsChangedEvent, user.id));
        };

        user.notifications.onChange = function(fun) {
            return callOnMatchingId(notificationsChangedEvent, user.id, fun);
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
                                        , data, _.cache, "get")
                    .success(channel.update);

        };

        channel.update = function() {
            return makeCachedAPICall( pr+".set", "GET", pa
                                    , {}, _.cache, "get")
                    .success(emitIt(channelChangedEvent, channel.id));
        };

        channel.onChange = function(fun) {
            return callOnMatchingId(channelChangedEvent, channel.id, fun);
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
