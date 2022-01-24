var Class = require('../lib/class')
var Method = require('../lib/method')

var Ptr = require('../lib/pointer')
require('../lib/abi')

Class.define({
    name: "A",
    data: [],
    virtual: [],
    nonvirtual: [
        Method("A", function impl_A_A (thiz) {
            console.log("A constructor running");
        }),
    ],
    inherits: []
});

Class.define({
    name: "B",
    data: [],
    virtual: [],
    nonvirtual: [
        Method("B", function impl_B_B (thiz) {
            console.log("B constructor running");
        }),
    ],
    inherits: ["A"]
});

Class.define({
    name: "B2",
    data: [],
    virtual: [],
    nonvirtual: [
        Method("B2", function impl_B_B (thiz) {
            console.log("B2 constructor running");
        }),
    ],
    inherits: ["A"]
});

Class.define({
    name: "C",
    data: [],
    virtual: [],
    nonvirtual: [
        Method("C", function impl_C_C (thiz) {
            console.log("C constructor running");
        }),
    ],
    inherits: ["B", "B2"]
});

function main() {
    Ptr.new_("C", []);
};
module.exports = main;
