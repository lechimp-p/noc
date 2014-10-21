angular.module("API.utilities", [])
.factory("makeAPICall", function($http) {
    return function(name, method, endpoint, data) {
        var config = 
            { method : method
            , url : "api/" + endpoint
            };
        if (method == "POST") {
            config.data = JSON.stringify(data);
            config.headers = {"Content-Type" : "application/json"};
        }
        else if (method == "GET") {
            config.params = data;
        }
        return $http( config )
                .error( function(data, status, headers, config) {
                    console.log("Error in APICall " + name + ": " + data);
                });
    };
})
;
