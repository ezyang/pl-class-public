var Class = require('../lib/class')
var Method = require('../lib/method')

var Ptr = require('../lib/pointer')
require('../lib/abi')

Class.define({
    name: "A",
    data: ["x"],
    virtual: [],
    nonvirtual: [
        Method("A", function impl_A_A(thiz, x) {
            thiz.setMember("x", x);
        })
    ],
    inherits: []
});

Class.define({
    name: "B",
    data: ["y"],
    virtual: [],
    nonvirtual: [
        Method("B", function impl_B_B(thiz, x, y) {
            thiz.setMember("x", x);
            thiz.setMember("y", y);
        })
    ],
    inherits: ["A"]
});

Class.define({
    name: "C",
    data: ["z"],
    virtual: [],
    nonvirtual: [
        Method("C", function impl_C_C(thiz, x, z) {
            thiz.setMember("x", x);
            thiz.setMember("z", z);
        })
    ],
    inherits: ["A"]
});

Class.define({
    name: "D",
    data: [],
    virtual: [],
    nonvirtual: [
        Method("D", function impl_D_D(thiz, x, y, z) {
            thiz.call("B", [x, y]);
            thiz.call("C", [x, z]);
        })
    ],
    inherits: ["B", "C"]
});

function main() {
    var d = Ptr.new_("D", [1,2,3]);
    var b = d.cast("B");
    var c = d.cast("C");
    b.setMember("x", 9);
    console.log(c.getMember("x"));
}
module.exports = main;
