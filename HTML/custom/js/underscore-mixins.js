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
    }
});
