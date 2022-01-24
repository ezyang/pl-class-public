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
//      void set(int ab) {
//          this->a = ab;
//      }
// }
Class.define({
    name: "A",
    data: ["a"],
    virtual: [
    ],
    nonvirtual: [
        Method("A", function impl_A_A (thiz) {
            thiz.setMember("a", 0);
        }),
        Method("set", function impl_A_set (thiz, ab) {
            thiz.setMember("a", ab);
        })
    ],
    inherits: []
});

/* Expected Output:
0
20
*/

function main() {
    var a = Ptr.new_("A", []);

    console.log(a.getMember("a"));    // 0
    a.call("set", [20]);
    console.log(a.getMember("a"));    // 20
};
module.exports = main;
