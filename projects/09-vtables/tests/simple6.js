var Class = require('../lib/class')
var Method = require('../lib/method')

var Ptr = require('../lib/pointer')
require('../lib/abi')

// class A {
// public:
//      int a;
//      A() {
//          this->a = 0;
//      }
// }
// class B {
// public:
//      B() {
//          this->a = 10;
//      }
// }

Class.define({
    name: "A",
    data: ["a"],
    virtual: [],
    nonvirtual: [
        Method("A", function impl_A_A (thiz) {
            thiz.setMember("a", 0);
        })
    ],
    inherits: []
});

Class.define({
    name: "B",
    data: [],
    virtual: [],
    nonvirtual: [
        Method("B", function impl_B_B (thiz) {
            thiz.setMember("a", 10);
        })
    ],
    inherits: ["A"]
});

/* Expected Output:
10
*/

function main() {
    var a = Ptr.new_("B", []);

    console.log(a.getMember("a"));    // 10
};
module.exports = main;
