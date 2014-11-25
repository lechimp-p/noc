_.mixin({
    takeWhile : function(list, predicate) {
        var ret = [];
        for(var elem in list) {
            if (!predicate(elem)) {
                return ret; 
            }
            ret.push(elem);
        }
        return ret;
    },
    
    defaultUndef : function(item, def) {
        if (typeof item == "undefined") {
            return def;
        }
        return item;
    }
})
;
