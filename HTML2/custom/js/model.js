angular.module("NoC.model", [])
.factory("model", ["$rootScope", "$q", "$timeout", "$interval", "makeAPICall", 
                    function($rootScope, $q, $timeout, $interval, makeAPICall) {
    "use strict";

    /////////
    // Config
    /////////

    var msgsUpdateIntervalMS = 30000;
    var initMsgsAmount = 10;

    ////////////
    // Utilities
    ////////////

    var addHttpInterface = function(promise) {
        promise.success = function(fun) {
            // we need to pass on the data by
            // ourselves to not destroy the
            // success interface.
            var pr = promise.then( function (val) {
                fun(val);
                return val;
            });

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
    var messagesUpdatedEvent = "channel-messages-changed";
    var contactsChangedEvent = "contacts-changed";
    var subscriptionsChangedEvent = "subscriptions-changed";
    var notificationsChangedEvent = "notifications-changed";
    var userChangedEvent = "user-changed";

    // Helper function for channel and user to create
    // functions that register eventlisteners. 
    var callOnMatchingId = function(ev, id, fun) {
        return $rootScope.$on(ev, function(event, _id, data) {
            if (id === _id) {
                fun(data);
            }
        });
    };

    // Helper to emit events.
    var emitIt = function(ev, id) {
        return function(response) {
            $rootScope.$emit(ev, id, response);
        };
    };

    ////////    
    // Model
    ////////

    var root = {};
    var __c = { users : {}
            , channels : {}
            };
    root.flushCache = function() {
        __c.users = {};
        __c.channels = {};
    };


    // User

    root.user = function(uid) {
        if (__c.users.hasOwnProperty(uid)) {
            return __c.users[uid];
        }
        var user = { id : uid };
        var _c = { cache : {} };        
        var pr = "user["+uid+"]";
        var pa = "user/"+uid;

        user.flushCache = function() {
            _c.cache = {};
        };        

        // Get promise to user information, from cache or server.
        user.get = function() {
            return makeCachePromiseOrAPICall( pr+".get", "GET", pa
                                            , {}, _c.cache, "get");
        };

        // Get promise to set user information and clear cache.
        // Afterwards update.
        user.set = function(data) {
            return makeAPICallClearCache(pr+".set", "POST", pa
                                        , data, _c.cache, "get")
                    .success(user.update);
        };

        // Get promise to user information from server and update cache.
        // Afterwards emit user.onChange.
        user.update = function() {
            return makeCachedAPICall(pr+".update", "GET", pa
                                    , {}, _c.cache, "get")
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
                                            , {}, _c.cache, "contacts");
        };

        user.contacts.set = function(set, remove) {
            return makeAPICallClearCache( pr+".contacts.set", "POST"
                                        , pa+"/contacts"
                                        , { set : set, remove : remove}
                                        , _c.cache, "contacts")
                    .success(user.contacts.update);
        };

        user.contacts.update = function() {
            return makeCachedAPICall( pr+".contacts.update", "GET"
                                    , pa+"/contacts"
                                    , {}, _c.cache, "contacts")
                    .success(emitIt(contactsChangedEvent, user.id));
        };

        user.contacts.onChange = function(fun) {
            return callOnMatchingId(contactsChangedEvent, user.id, fun);
        };

        user.subscriptions = {};

        user.subscriptions.get = function() {
            return makeCachePromiseOrAPICall( pr+".subscriptions.get", "GET"
                                            , pa+"/subscriptions"
                                            , {}, _c.cache, "subscriptions"); 
        };

        user.subscriptions.set = function(subscribe, unsubscribe) {
            return makeAPICallClearCache( pr+".subscriptions.set", "POST"
                                        , pa+"/subscriptions"
                                        , { subscribe : subscribe, unsubscribe : unsubscribe }
                                        , _c.cache, "subscriptions")
                    .success(function(_) {
                        // We need to flush the cache of the targeted
                        // channels as well, since their information
                        // changed.
                        _c.map(subscribe.concat(unsubscribe), function(val) {
                            root.channel(val).update();
                        });

                        user.subscriptions.update();
                    });
        };

        user.subscriptions.update = function() {
            return makeCachedAPICall( pr+".subscriptions.update", "GET"
                                    , pa+"/subscriptions"
                                    , {}, _c.cache, "subscriptions")
                    .success(emitIt(subscriptionsChangedEvent, user.id));
        };

        user.subscriptions.onChange = function(fun) {
            return callOnMatchingId(subscriptionsChangedEvent, user.id, fun);
        };

        user.notifications = {};

        user.notifications.get = function() {
            return makeCachePromiseOrAPICall( pr+".notifications.get", "GET"
                                            , pa+"/notifications"
                                            , {}, _c.cache, "notifications"); 
        };

        user.notifications.update = function() {
            return makeCachedAPICall( pr+".notifications.get", "GET"
                                    , pa+"/notifications"
                                    , {}, _c.cache, "notifications")
                    .success(emitIt(notificationsChangedEvent, user.id));
        };

        user.notifications.onChange = function(fun) {
            return callOnMatchingId(notificationsChangedEvent, user.id, fun);
        };

        __c.users[uid] = user;
        return user;
    };

    root.user.search = function(login) {
        return makeAPICall("user.search", "GET", "user", { login : login });
    };


    // Channel

    root.channel = function(cid) {
        if (__c.channels.hasOwnProperty(cid)) {
            return __c.channels[cid];
        }

        var channel = { id : cid };
        var _c = { cache : { messages : [] } };
        var updateTask = { value : null, counter : 0 };
        var pr = "channel["+cid+"]";
        var pa = "channel/"+cid; 

        channel.flushCache = function() {
            _c.cache = { messages : [] };
        };

        channel.get = function() {
            return makeCachePromiseOrAPICall( pr+".get", "GET", pa
                                            , {}, _c.cache, "get");
        }; 

        channel.set = function(data) {
            return makeAPICallClearCache( pr+".set", "POST", pa
                                        , data, _c.cache, "get")
                    .success(channel.update);

        };

        channel.update = function() {
            return makeCachedAPICall( pr+".update", "GET", pa
                                    , {}, _c.cache, "get")
                    .success(emitIt(channelChangedEvent, channel.id));
        };

        channel.onChange = function(fun) {
            return callOnMatchingId(channelChangedEvent, channel.id, fun);
        };

        channel.messages = {};

        channel.messages.get = function(offset, amount) {
            return makeAPICall( pr+".messages", "GET", pa+"/messages"
                                , { offset : offset, amount : amount }
                              );
        };

        channel.messages.getTill = function(timestamp) {
            return makeAPICall( pr+".messagesTill", "GET", pa+"/messages"
                                , { timestamp : timestamp }
                              );
        };

        channel.messages.onUpdate = function(fun) {
            if (_c.cache.messages.length > 0) {
                fun({ messages : _c.cache.messages });
            }

            return callOnMatchingId(messagesUpdatedEvent, channel.id, fun);
        };

        channel.messages.update = function() {
            if (_c.cache.messages.length === 0) {
                return channel.messages.get(0, initMsgsAmount)
                    .success(function(response) {
                        _c.cache.messages = response.messages;
                    })
                    .success(emitIt(messagesUpdatedEvent, channel.id));
            }
            
            return channel.messages.getTill(_c.cache.messages[0].timestamp)
                .success(function(response) {
                    _c.cache.messages = response.messages.concat(_c.cache.messages);
                })
                .success(emitIt(messagesUpdatedEvent, channel.id));
        };
    
        channel.messages.startUpdateTask = function() {
            updateTask.counter += 1;

            if (updateTask.value !== null) {
                return;
            }
            
            updateTask.value = $interval(channel.messages.update, msgsUpdateIntervalMS);
        };

        channel.messages.stopUpdateTask = function() {
            if (updateTask.value === null) {
                return;
            }

            updateTask.counter -= 1;

            if (updateTask.counter === 0) {
                $interval.cancel(updateTask.value);
            }
        };

        channel.messages.post = function(text) {
            return makeAPICall( pr+".post", "POST", pa+"/messages"
                              , { text : text })
                    .success( channel.messages.update );
        };

        __c.channels[cid] = channel;
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
