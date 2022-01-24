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
//      virtual void set(int ab) {
//          this->a = ab;
//      }
// }
Class.define({
    name: "A",
    data: ["a"],
    virtual: [
        Method("set", function impl_A_set (thiz, ab) {
            thiz.setMember("a", ab);
        })
    ],
    nonvirtual: [
        Method("A", function impl_A_A (thiz) {
            thiz.setMember("a", 0);
        })
    ],
    inherits: []
});

/* Expected Output:
0
30
*/

function main() {
    var a = Ptr.new_("A", []);

    console.log(a.getMember("a"));    // 0
    a.vcall("set", [30]);
    console.log(a.getMember("a"));    // 30
};
module.exports = main;
