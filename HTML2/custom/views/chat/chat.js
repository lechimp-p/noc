angular.module("NoC.chat", []).
controller("chat-controller", [ "$rootScope", "$scope", "$interval", "$routeParams", "model"
         , function($rootScope, $scope, $interval, $routeParams, model) {
    "use strict";

    var lastTS = {};
    var updateIntervalMS = 5000;
    var updateTask = { value : null };
 
    var toScope = function(response) {
        if (response.messages.length > 0) {
            lastTS.value = response.messages[0].timestamp;
        }
        if ($scope.msgs) {
            $scope.msgs = $scope.msgs.concat(response.messages.reverse());
        }
        else {
            $scope.msgs = response.messages.reverse();
        }
    };

    $scope.post = function() {
        if ($scope.message.length === 0) {
            return;
        }    

        model.channel($scope.channel.id)
            .post($scope.message)
                .then($scope.updateMessages); 
        $scope.message = "";
    };

    $scope.updateMessages = function() {
        if (typeof lastTS.value === "undefined") {
            return model.channel($scope.channel.id)
                    .messages(0, 10)
                        .then(toScope);
        }
        else {
            return model.channel($scope.channel.id)
                    .messagesTill(lastTS.value)
                        .then(toScope);
        }
    };

    $scope.updateChannelInfo = function() {
        return model.channel($routeParams.chanId).get()
                    .then(function(response) {
                        $scope.channel = response; 
                    });
    };

    $scope.startUpdateTask = function() {
        if (updateTask.value !== null) {
            return;
        }

        $scope.updateMessages();
        updateTask.value = $interval($scope.updateMessages, updateIntervalMS);
    };

    $scope.stopUpdateTask = function() {
        if (updateTask.value === null) {
            return;
        }

        $interval.cancel(updateTask.value);
    };

    $rootScope.$on("$routeChangeStart", function(event, next, current) {
        if (next.pathParams.chanId == $scope.channel.id) {
            $scope.updateChannelInfo();
            $scope.startUpdateTask(); 
        }
        else {
            $scope.stopUpdateTask();
        }
    });

    $scope.updateChannelInfo()
        .then( function (_) { $scope.startUpdateTask(); });

    //$interval($scope.updateMessages, updateIntervalMS);

/*    $scope.msgs =
        [ { author : 
            { icon : "img/profilepic.png"
            , name : "Max Mustermann"
            }
            , timestamp : "Do. 13:37"
            , text : "Man, das ist ja supercrisp hier...."
          }
        , { author :
            { icon : "img/profpic.jpg"
            , name : "Petra Test"
            }
            , timestamp : "Fr. 23:55"
            , text : "Ja, echt spitze!"
          }
        ];

    $scope.channel =
        { image : "img/channel/0.png"
        , name : "Grillen in Kölle"
        , description : "Für alle, die in Köln grillen wollen."
        };
*/
}]);
