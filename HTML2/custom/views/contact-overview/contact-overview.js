angular.module("NoC.contact-overview", []).
controller("contact-overview-controller", [ "$scope", "model", "user",
         function($scope, model, user) {
    "use strict";

    var unregister = {};

    $scope.contacts = {}; 

    var setLastMsg = function(userId) {
        return function(msgs) {
            $scope.contacts[userId].channel.lastMsg = msgs.messages[0];
        };
    };

    var registerChanHandler = function(userId, chanId) {
        var chan = model.channel(chanId);
        unregister[chanId] = chan.messages.onUpdate(setLastMsg(userId));
        chan.messages.startUpdateTask();
    };

    var unregisterChanHandler = function(chanId) {
        unregister[chanId]();
        delete unregister[chanId];
        model.channel(chanId).messages.startUpdateTask();
    };
    
    user.onIdAcquired(function(id) {
        model.user(id).contacts.onUpdate(function(response) {
            var next = {};
            var nextIds = [];
            var curIds = _.keys($scope.contacts);

            _.map(response.contacts, function(contact) {
                next[contact.user.id] = contact;
                nextIds.push(contact.user.id);
            });

            var In = _.difference(nextIds, curIds);
            var Out = _.difference(curIds, nextIds);

            _.map(In, function(id) {
                registerChanHandler(id, next[id].channel.id);
            });

            _.map(Out, function(id) {
                unregisterChanHandler($scope.contacts[id].channel.id);
            });

            $scope.contacts = next;
        });

        model.user(id).contacts.update();
    });
}]);
