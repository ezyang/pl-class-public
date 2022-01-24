var Class = require('../lib/class')
var Method = require('../lib/method')

var Ptr = require('../lib/pointer')
require('../lib/abi')

Class.define({
    name: "A",
    data: ["x"],
    virtual: [
        Method("f", function impl_A_f (thiz) {
            thiz.assertType("A");
            console.log("A::f()");
            console.log(thiz.getMember("x"));
        })
    ],
    nonvirtual: [],
    inherits: []
});

Class.define({
    name: "B",
    data: ["y"],
    virtual: [
        Method("g", function impl_B_g (thiz) {
            thiz.assertType("B");
            console.log("B::g()");
            console.log(thiz.getMember("y"));
        }),
        Method("f", function impl_B_f (thiz) {
            thiz.assertType("B");
            console.log("B::f()");
            console.log(thiz.getMember("y"));
        })
    ],
    nonvirtual: [],
    inherits: []
});

Class.define({
    name: "C",
    data: ["z"],
    virtual: [
        Method("f", function impl_C_f (thiz) {
            thiz.assertType("C");
            console.log("C::f()");
            console.log(thiz.getMember("x"));
            console.log(thiz.getMember("y"));
            console.log(thiz.getMember("z"));
        })
    ],
    nonvirtual: [
        Method("C", function impl_C_C (thiz, x, y, z) {
            thiz.setMember("x", x);
            thiz.setMember("y", y);
            thiz.setMember("z", z);
        })
    ],
    inherits: ["A", "B"]
});

function main() {
    var c = Ptr.new_("C", [0,1,2]);
    c.vcall("f", []);
    c.vcall("g", []);
    var a = c.cast("A");
    a.vcall("f", []);
    var b = c.cast("B");
    b.vcall("f", []);
    b.vcall("g", []);
}

module.exports = main;
