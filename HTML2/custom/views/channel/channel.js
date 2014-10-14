angular.module("NoC.channel", []).
controller("channel-controller", [ "$rootScope", "$scope", "$interval", "$routeParams", "API"
         , function($rootScope, $scope, $interval, $routeParams, API) {
    "use strict";

    var lastTS = {};
    var updateIntervalMS = 5000;
    var updateTask = { value : null };
 
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

        API.post($scope.channel.id, $scope.message)
            .success($scope.updateMessages); 
        $scope.message = "";
    };

    $scope.subscribe = function() {
        API.subscribe($scope.channel.id);
    }

    $scope.updateMessages = function() {
        if (typeof lastTS.value === "undefined") {
            return API.messages($scope.channel.id, 0, 10).success(toScope);
        }
        else {
            return API.messagesTill($scope.channel.id, lastTS.value).success(toScope);
        }
    };

    $scope.updateChannelInfo = function() {
        return API.getChannelInfo($routeParams.chanId)
                    .success(function(response) {
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
        .success( function (_) { $scope.startUpdateTask(); });

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
