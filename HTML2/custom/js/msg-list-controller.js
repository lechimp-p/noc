angular.module("NoC.controllers", []).
controller("msg-list-controller", function($scope, $interval, API) {
    "use strict";

    var lastTS = {};
    
    var toScope = function(response) {
        if (response.messages.length > 0) {
            lastTS.value = response.messages[0].timestamp;
        }
        $scope.msgs = response.messages;
    };

    API.login("admin", "admin")
        .success(function(response) {
            console.log("logged in");
            API.messages(0, 0, 10)
                .success(toScope)
                .success(function(_) {
                    $interval(function() {
                        API.messagesTill(0, lastTS.value).success(toScope);
                        //API.messages(0, 0, 10).success(toScope);
                    }, 5000);
                }); 
        })
        ;

    /*$scope.msgs =
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
    */
});
