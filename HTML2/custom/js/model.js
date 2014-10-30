angular.module("NoC.model", [])
.factory("model", ["$rootScope", "$q", "$timeout", "makeAPICall", 
                    function($rootScope, $q, $timeout, makeAPICall) {
    "use strict";

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
    var messagesChangedEvent = "channel-messages-changed";
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
        var _c = { cache : { msgs : { messages : [] } } };
        var pr = "channel["+cid+"]";
        var pa = "channel/"+cid; 

        channel.flushCache = function() {
            _c.cache = { msgs : { messages : [] } };
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
            if (_c.cache.msgs.messages.length > offset + amount) {
                // seems there are enough messages in
                // the stack to satisfy the request.
                return makeCachePromise( 
                        { messages : _c.cache.msgs.messages.slice(offset, offset + amount) }
                    );
            }

            // semms as we need new messages...
            // But instead of just querying the requested
            // slice we query all messages from start for
            // chaching purpose.
            return  addHttpInterface(
                        makeCachedAPICall( 
                                  pr+".messages", "GET", pa+"/messages"
                                , { offset : offset, amount : amount }
                                , _c.cache, "msgs")
                            // We use then to reduce the passed messages
                            // to the desired slice after caching.
                            .then( function(val) {
                                return { messages : val.data.slice(offset, amount) };
                            })
                    );
        };

        channel.messages.getTill = function(timestamp) {
            // that's the easiest case. we just have no
            // messages atm.
            if (_c.cache.msgs.messages.length === 0) {
                return makeCachedAPICall( 
                              pr+".messagesTill", "GET", pa+"/messages"
                            , { timestamp : timestamp }
                            , _c.cache, "msgs");
            }
            
            // TODO: is it really ok to compare the timestamps like this? 
            // I guess not...
            if ( _c.cache.msgs.messages[0].timestamp > timestamp) {
                // seems we already have newer messages. so we assume
                // the cache is fresh enough...
                return makeCachePromise(
                            { messages : _.takeWhile(_c.cache.msgs.messages, function(msg) {
                                return msg.timestamp > timestamp;
                            }) }
                        );
            }

            // so we need to get more messages. instead of
            // querying for the timestamp we ask for all 
            // messages that are newer then our newest message in the cache.
            return  addHttpInterface(
                        makeAPICall( 
                                  pr+".messagesTill", "GET", pa+"/messages"
                                , { timestamp : _c.cache.msgs.messages[0].timestamp }
                                )
                            .success( function(response) {
                                _c.cache.msgs.messages = response.messages.concat(_c.cache.msgs.messages); 
                            })
                            .success(emitIt(messagesChangedEvent, channel.id))
                            // We use then to reduce the passed messages
                            // to the desired slice after caching.
                            .then( function(val) {
                                return { messages : _.takeWhile(val.data, function (msg) {
                                                            return msg.timestamp > timestamp;
                                                        })
                                       };
                            })
                    );

        };

        channel.messages.update = function() {
            if (_c.cache.msgs.messages.length > 0) {
                return channel.messages.getTill(_c.cache.msgs.messages[0].timestamp);
            }
            return channel.messages.getTill(undefined);
        };

        channel.messages.onChange = function(fun) {
            return callOnMatchingId(messagesChangedEvent, channel.id, fun);
        };

        channel.messages.post = function(text) {
            return makeAPICall( pr+".post", "POST", pa+"/messages"
                              , { text : text });
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
