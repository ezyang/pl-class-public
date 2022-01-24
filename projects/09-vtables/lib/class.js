var adt = require('adt'), data = adt.data, only = adt.only;

// Represents a C++ class, for example:
//
// class Point {
// public:
//      int x, y;
//      Point(int x, int y) { ... }
//      virtual void move(int dx, int dy) { ... }
// }
//
// Generally, you will simultaneously define a class and add
// it to the set of globally defined classes (Metadata.getClasses):
//
//      Class.define({
//          name: "Point",
//          data: ["x", "y"],
//          virtual: [Method("move", impl_Point_move)],
//          nonvirtual: [Method("Point", impl_Point_Point)],
//          inherits: []
//      });
//
// Classes must be defined 'in-order' (so superclasses defined before
// subclasses.)
var Class = data(function () {
    return {
        Class : {
            // The name of the class, e.g. "Point"
            name : only(String),
            // An array of strings for each instance variable
            // in the class, e.g. ["x", "y"]
            data : adt.any,
            // An array of Methods for each virtual function
            // in the class, e.g. [Method("move", impl_move)]
            virtual : adt.any,
            // An array of Methods for each non-virtual function
            // in the class, e.g. [Method("Point", impl_Point)]
            nonvirtual : adt.any,
            // An array of strings for each parent of this
            // class, e.g. [] (or, a more interesting example,
            // for ColoredPoint extends Point it would be ["Point")
            inherits : adt.any,
        }
    };
});

module.exports = Class
