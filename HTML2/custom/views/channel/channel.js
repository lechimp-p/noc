angular.module("NoC.channel", []).
controller("channel-controller", [ "$rootScope", "$scope", "$routeParams", "model", "user"
         , function($rootScope, $scope, $routeParams, model, user) {
    "use strict";

    $scope.channel = { id : $routeParams.chanId };
    $scope.msgs = [];

    $scope.post = function() {
        if ($scope.message.length === 0) {
            return;
        }    

        model.channel($scope.channel.id)
            .messages
            .post($scope.message);

        $scope.message = "";
    };

    $scope.subscribe = function() {
        user.subscribe($scope.channel.id);
    };

    $scope.unsubscribe = function() {
        user.unsubscribe($scope.channel.id);
    };

    var setChannelInfo = function(data) {
        $scope.channel = data;
    };

    var setMessages = function(msgs) {
        $scope.msgs = msgs.messages;
    };

    var prependMessages = function(msgs) {
        $scope.msgs = msgs.messages.concat($scope.msgs);
    };

    model.channel($scope.channel.id).onChange(setChannelInfo);

    $rootScope.$on("$routeChangeStart", function(event, next, current) {
        if (next.pathParams.chanId == $scope.channel.id) {
            model.channel($scope.channel.id).update();
            model.channel($scope.channel.id).messages.startUpdateTask(); 
        }
        else {
            model.channel($scope.channel.id).messages.stopUpdateTask();
        }
    });

    model.channel($scope.channel.id).update();
    model.channel($scope.channel.id)
        .messages.startUpdateTask();
    model.channel( $scope.channel.id )
        .messages.onUpdate( prependMessages );
    model.channel( $scope.channel.id )
        .messages.update();

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
