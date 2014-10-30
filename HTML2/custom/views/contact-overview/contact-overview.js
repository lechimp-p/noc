angular.module("NoC.contact-overview", []).
controller("contact-overview-controller", [ "$scope", "model", "user",
         function($scope, model, user) {
    "use strict";

    var unregister = {};

    $scope.contacts = {}; 

    var setLastMsg = function(msgs) {
        $scope.contacts.lastMsg = msgs[0];
    };

    var registerChanHandler = function(chanId) {
        var chan = model.channel(chanId);
        unregister[chanId] = chan.messages.onChange(setLastMsg);
    };

    var unregisterChanHandler = function(chanId) {
        unregister[chanId]();
        delete unregister[chanId];
    };
    
    user.onIdAcquired(function(id) {
        model.user(id).contacts.onChange(function(response) {
            var next = {};
            var nextIds = [];
            var curIds = _.keys($scope.contacts);

            _.map(response.contacts, function(contact) {
                next[contact.id] = contact;
                nextIds.push(contact.id);
            });

            var In = _.difference(nextIds, curIds);
            var Out = _.difference(curIds, nextIds);

            _.map(In, function(id) {
                registerChanHandler(next[id].channel.id);
            });

            _.map(Out, function(id) {
                unregisterChanHandler($scope.contacts[id].channel.id);
            });

            $scope.contacts = next;
        });

        model.user(id).contacts.update();
    });
}]);
