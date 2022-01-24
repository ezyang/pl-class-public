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
//      void set() {
//          this->a = 10;
//      }
//      
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
        Method("set", function impl_A_set (thiz) {
            thiz.setMember("a", 10);
        })
    ],
    inherits: []
});

/* Expected Output:
0
10
*/

function main() {
    var a = Ptr.new_("A", []);

    console.log(a.getMember("a"));    // 0
    a.call("set", []);
    console.log(a.getMember("a"));    // 10
};
module.exports = main;
