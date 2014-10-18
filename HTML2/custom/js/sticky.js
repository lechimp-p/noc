angular.module("sticky", [])
.directive("sticky", ["$window", function($window) {
    "use strict";

    var stick = function(elem, cl, plcl, offset, code) {
        var placeholder = angular.element("<div></div>");
        var window = angular.element($window);

        elem.addClass(cl);
        placeholder.css("height", elem.outerHeight() + "px")
                   .addClass(plcl)
                   .insertBefore(elem);

        window.off("scoll.stick" + code);
        window.on("scroll.stick" + code, function(ev) {
            if(window.scrollTop() + offset <= placeholder.offset().top) {
                unstick(elem, cl, plcl, placeholder , offset, code); 
            }
        });
    };

    var unstick = function(elem, cl, plcl, pl, offset, code) {
        var window = angular.element($window);

        elem.removeClass(cl);
        pl.remove();

        window.off("scroll.stick" + code);
        window.on("scroll.stick"+code, function(ev) {
            if(window.scrollTop() + offset > elem.offset().top) {
                stick(elem, cl, code, offset);
            }
        });
    };

    var link = function(scope, elem, attrs) {
        var window = angular.element($window);
        var code = "" + Math.random();
        var offset = parseInt(attrs.offset);
        if (typeof offset == "undefined") {
            offset = 0;
        }
        var stick_class = attrs.stickClass;
        if (typeof stick_class == "undefined") {
            stick_class = "stick";
        }
        var placeholder_class = attrs.placeholderClass;
        if (typeof placeholder_class == "undefined") {
            placeholder_class = "placeholder";
        }

        window.on("scroll.stick"+code, function(ev) {
            if((window.scrollTop() + offset) > elem.offset().top) {
                stick(elem, stick_class, placeholder_class, offset, code);
            } 
        });
    };

    return { link : link
           };
}])
;
