var adt = require('adt'), data = adt.data, only = adt.only;

var Layout = data(function () {
    return {
        Layout : {
            offset : only(Number),
            clazz : only(String),
            vtable : adt.any // only(Array) of VTableRow
        }
    };
});

Layout.prototype.copy = function() {
    return Layout.Layout(this.offset, this.clazz, this.vtable.slice(0));
}

var VEntry = data(function() {
    return {
        VEntry : {
            impl : adt.any, // only(Function)
            type : only(String),
            delta : only(Number)
        }
    }
});

module.exports = {Layout: Layout.Layout, VEntry: VEntry.VEntry};
