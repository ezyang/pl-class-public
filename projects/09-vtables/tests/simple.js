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
Class.define({
    name: "A",
    data: ["a"],
    virtual: [
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
*/

function main() {
    var a = Ptr.new_("A", []);

    console.log(a.getMember("a"));    // 0
};
module.exports = main;
