angular.module("NoC.controllers", []).
controller("msg-list-controller", function($scope, API) {
    "use strict";

    API.login("admin", "admin")
        .success(function(response) {
            API.messages(0, 0, 10)
                .success(function(response) {
                    $scope.msgs = response.messages;   
                })
                .error(function(data, status, headers, config) {
                    console.log("API.messages not reachable?");
                    console.log(data);
                })
                ;
        })
        .error(function(data, status, headers, config) {
            console.log("API.login not reachable?");
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
