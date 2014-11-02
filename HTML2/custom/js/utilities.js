angular.module("API.utilities", [])
.factory("makeAPICall", ["$http", "$location", function($http, $location) {
    "use strict";

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
                    $location.path("error"); 
                });
    };
}])
.factory("makeUnwrappedAPICall", ["$q", "makeAPICall", function($q, makeAPICall) {
    "use strict";

    // Like makeAPICall, but forwards the received data to 'then'.
    // This is needed because we like to return promises to cached
    // data, which do not have a success method. Users of models 
    // should therefore use 'then' instead of 'success'/'error'. The standard
    // $http-request gives an object to 'then', which contains information
    // about the request. Model users should not see that.
    return function(name, method, path, params) {
        var pr = makeAPICall(name, method, path, params);
        var deferred = $q.defer();

        // TODO: Error is not forwarded to promise.
        //       I defer the decision if error should
        //       be forwarded until i decided how to
        //       do error handling.
        pr.success(function(response) {
            deferred.resolve(response);
        });

        return deferred.promise;
    };
}])
;
