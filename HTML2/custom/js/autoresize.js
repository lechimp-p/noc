angular.module("autoresize", [])
.directive("autoresize", ["$window", function($window) {
    "use strict";

    var link = function(scope, elem, attrs) {
        var hidden = angular.element("<div></div>");
        var window = angular.element($window);

        var hidden_class = attrs.hiddenClass;
        if (typeof hidden_class == "undefined") {
            hidden_class = "hiddendiv";
        }

        hidden.css({ "visibility" : "hidden"
                   , "position" : "absolute"
                   , "top" : "-99999px"
                   })
              .width(elem.width())
              .addClass(hidden_class)
              .insertAfter(elem);

        elem.on("keyup", function() {
            content = elem.val();
            content = content.replace(/\n/g, '<br />');
            if (content.length === 0) {
                content = "p";
            }
            hidden.html(content + "&nbsp;");
            hidden.width(elem.width());
            elem.height(hidden.height());
        });
    };

    return { link : link };
}])
; 
