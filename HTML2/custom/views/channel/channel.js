angular.module("NoC.channel", []).
controller("channel-controller", [ "$rootScope", "$scope", "$interval", "$routeParams", "model", "user"
         , function($rootScope, $scope, $interval, $routeParams, model, user) {
    "use strict";

    var lastTS = {};
    var updateIntervalMS = 5000;
    var updateTask = { value : null };

    $scope.channel = { id : $routeParams.chanId };
 
    var toScope = function(response) {
        if (response.messages.length > 0) {
            lastTS.value = response.messages[0].timestamp;
        }
        if ($scope.msgs) {
            $scope.msgs = response.messages.concat($scope.msgs);
        }
        else {
            $scope.msgs = response.messages;
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

    $scope.subscribe = function() {
        user.subscribe($scope.channel.id)
                .then(function(response) { 
                    $scope.channel.subscribed = true;
                    $scope.updateChannelInfo();
                });
    };

    $scope.unsubscribe = function() {
        user.unsubscribe($scope.channel.id)
                .then(function(response) { 
                    $scope.channel.subscribed = false;
                    $scope.updateChannelInfo();
                });
    };

    $scope.updateMessages = function() {
        if (typeof lastTS.value === "undefined") {
            return model.channel($scope.channel.id)
                .messages(0, 10).then(toScope);
        }
        else {
            return model.channel($scope.channel.id)
                .messagesTill(lastTS.value).then(toScope);
        }
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

    $scope.updateChannelInfo = function() {
        return model.channel($scope.channel.id)
                .get()
                .then(function(response) {
                    console.log(response);
                    $scope.channel = response; 
                    return response;
                });
    };
    
    $scope.updateChannelInfo()
          .then( $scope.startUpdateTask );

    $rootScope.$on("$routeChangeStart", function(event, next, current) {
        if (next.pathParams.chanId == $scope.channel.id) {
            $scope.updateChannelInfo();
            $scope.startUpdateTask(); 
        }
        else {
            $scope.stopUpdateTask();
        }
    });

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
