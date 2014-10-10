angular.module("NoC.channel", []).
controller("channel-controller", [ "$scope", "$interval", "$routeParams", "API"
         , function($scope, $interval, $routeParams, API) {
    "use strict";

    var lastTS = {};
    var updateIntervalMS = 5000;
 
    var toScope = function(response) {
        console.log("received messages");
        console.log(response);
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

    API.getChannelInfo($routeParams.chanId)
        .success(function(response) {
            $scope.channel = response; 
       
            API.messages(0, 0, 10)
                .success(toScope)
                .success(function(_) {
                    $interval(function() {
                        console.log(lastTS.value);
                        API.messagesTill(0, lastTS.value).success(toScope);
                    }, updateIntervalMS);
                });
        })
        ;

    $scope.post = function(message) {
        API.post(message); 
    };

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
