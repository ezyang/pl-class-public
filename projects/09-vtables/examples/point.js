var Class = require('../lib/class')
var Method = require('../lib/method')

var Ptr = require('../lib/pointer')
require('../lib/abi')

// class Point {
// public:
//      int x, y;
//      Point(int x, int y) {
//          this->x = x;
//          this->y = y;
//      }
//      virtual void move(int dx, int dy) {
//          this->x += dx;
//          this->y += dy;
//      }
// }
Class.define({
    name: "Point",
    data: ["x", "y"],
    virtual: [
        Method("move", function impl_Point_move (thiz, dx, dy) {
            thiz.assertType("Point");
            thiz.setMember("x", thiz.getMember("x") + dx);
            thiz.setMember("y", thiz.getMember("y") + dy);
        })
    ],
    nonvirtual: [
        Method("Point", function impl_Point_Point (thiz, x, y) {
            thiz.assertType("Point");
            thiz.setMember("x", x);
            thiz.setMember("y", y);
        })
    ],
    inherits: []
});

Class.define({
    name: "ScaledPoint",
    data: ["s"],
    virtual: [
        Method("move", function impl_ScaledPoint_move(thiz, dx, dy) {
            // A THOUGHT: Why is it a ScaledPoint, and not a Point? :)
            thiz.assertType("ScaledPoint");
            thiz.setMember("x", thiz.getMember("x") + dx * thiz.getMember("s"));
            thiz.setMember("y", thiz.getMember("y") + dy * thiz.getMember("s"));
        }),
        Method("scale", function impl_ScaledPoint_scale(thiz, s) {
            thiz.assertType("ScaledPoint");
            thiz.setMember("s", s);
        })
    ],
    nonvirtual: [
        Method("ScaledPoint", function impl_ScaledPoint_ScaledPoint(thiz, x, y, s) {
            thiz.assertType("ScaledPoint");
            thiz.setMember("x", x);
            thiz.setMember("y", y);
            thiz.setMember("s", s);
        })
    ],
    inherits: ["Point"]
});

function main() {
    var p = Ptr.new_("Point", [2, 3]);
    p.vcall("move", [2, 3]);
    console.log(p);
    var q = Ptr.new_("ScaledPoint", [2, 3, 2]); //.cast("Point");
    q.vcall("move", [2, 3]);
    console.log(q);
}
module.exports = main;
