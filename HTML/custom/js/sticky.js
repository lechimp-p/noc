angular.module("sticky", [])
.directive("sticky", ["$window", function($window) {
    "use strict";

    var shouldStick = function(item) {
        var scroll = (item.useScrollTop ? item.refElement.scrollTop() : 0) + item.offset;
        var pos = item.element.offset().top - item.refOffsetTop;
        return scroll > pos;
    };

    var shouldUnstick = function(item) {
        var scroll = (item.useScrollTop ? item.refElement.scrollTop() : 0) + item.offset;
        var pos = item.placeholder.offset().top - item.refOffsetTop;
        return scroll <= pos; 
    };

    var stick = function(item) {
        if (item.placeholder !== null) {
            alert("Something goes wrong with stick/unstick...");
            return;
        }

        item.element.addClass(item.stickClass);

        item.placeholder = angular.element("<div></div>");
        item.placeholder.css("height", item.element.outerHeight() + "px")
                        .addClass(item.placeholderClass)
                        .insertBefore(item.element);
    };

    var unstick = function(item) {
        if (item.placeholder === null) {
            alert("Something goes wrong with unstick/stick...");
            return;
        }

        item.element.removeClass(item.stickClass);

        item.placeholder.remove();
        item.placeholder = null;
    };

    var bindStickChecker = function(item) {
        item.refElement.on("scroll.stick" + item.code, function(ev) {
            if(shouldStick(item)) {
                unbindChecker(item);
                stick(item); 
                bindUnstickChecker(item);
            }
        });
    };

    var bindUnstickChecker = function(item) {
        item.refElement.on("scroll.stick" + item.code, function(ev) {
            if(shouldUnstick(item)) {
                unbindChecker(item);
                unstick(item); 
                bindStickChecker(item);
            }
        });
    };

    var unbindChecker = function(item) {
        item.refElement.off("scroll.stick" + item.code);
    };

    var link = function(scope, elem, attrs) {
        // offset in pixels to refElement for the
        // start of sticking.
        if (typeof attrs.offset == "undefined") {
            attrs.offset = 0;
        }
        else {
            attrs.offset = parseInt(attrs.offset);
        }

        // class to set to the sticking element
        if (typeof attrs.stickClass == "undefined") {
            attrs.stickClass = "stick";
        }

        // class to set to the placeholder for the 
        // sticking element
        if (typeof attrs.placeholderClass == "undefined") {
            attrs.placeholderClass = "placeholder";
        }

        // offset of the reference element, defaults
        // to 0 (window) 
        var refOffsetTop = 0;
        // determine weather scrollTo should be used (window)
        // or measurement of doc position suffices (element)
        var useScrollTop = true;
        // reference element, inside which the scrolling
        // happens.
        if (typeof attrs.refElement == "undefined") {
            attrs.refElement = angular.element($window);
            
        }
        else {
            attrs.refElement = angular.element("#"+attrs.refElement);
            // offset is measured regarding the client.
            // we therefore need the offset of the refElement.
            refOffsetTop = attrs.refElement.offset().top;
            useScrollTop = false;
        }

        var item = { element : elem
                   , useScrollTop : useScrollTop
                   , offset : attrs.offset
                   , stickClass : attrs.stickClass
                   , placeholderClass : attrs.placeholderClass
                   , placeholder : null
                   , refElement : attrs.refElement
                   , refOffsetTop : refOffsetTop
                   , code : "" + Math.random()
                   };

        bindStickChecker(item);
    };

    return { link : link
           };
}])
;
