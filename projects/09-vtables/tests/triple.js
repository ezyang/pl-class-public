var Class = require('../lib/class')
var Method = require('../lib/method')

var Ptr = require('../lib/pointer')
require('../lib/abi')

// class A1 {
// public:
//      int a1;
//      A1() {
//          this->a1 = 0;
//      }
//      void set1(int set) {
//          this->a1 = set;
//      }
//      virtual void set1v(int set) {
//          this->a1 = set;
//      }
// }
// class A2 {
// public:
//      int a2;
//      A2() {
//          this->a2 = 0;
//      }
//      void set2(int set) {
//          this->a2 = set;
//      }
//      virtual void set2v(int set) {
//          this->a2 = set;
//      }
// }
// class A3 {
// public:
//      int a3;
//      A3() {
//          this->a3 = 0;
//      }
//      void set3(int set) {
//          this->a3 = set;
//      }
//      virtual void set3v(int set) {
//          this->a3 = set;
//      }
// }
// class B:A1,A2,A3 {
// public:
//      int b
//      B() {
//          this->b = 0;
//      }
//      void setB(int set) {
//          this->b = set;
//      }
//      virtual void set1v(int set) {
//          this->b = set;
//      }
// }


Class.define({
    name: "A1",
    data: ["a1"],
    virtual: [
        Method("set1v", function impl_A1_set1v (thiz, set) {
            thiz.setMember("a1", set);
        })
    ],
    nonvirtual: [
        Method("A1", function impl_A1_A1 (thiz) {
            thiz.setMember("a1", 0);
        }),
        Method("set1", function impl_A1_set1 (thiz, set) {
            thiz.setMember("a1", set);
        })
    ],
    inherits: []
});

Class.define({
    name: "A2",
    data: ["a2"],
    virtual: [
        Method("set2v", function impl_A2_set2v (thiz, set) {
            thiz.setMember("a2", set);
        })
    ],
    nonvirtual: [
        Method("A2", function impl_A2_A2 (thiz) {
            thiz.setMember("a2", 0);
        }),
        Method("set2", function impl_A2_set2 (thiz, set) {
            thiz.setMember("a2", set);
        })
    ],
    inherits: []
});

Class.define({
    name: "A3",
    data: ["a3"],
    virtual: [
        Method("set3v", function impl_A3_set3v (thiz, set) {
            thiz.setMember("a3", set);
        })
    ],
    nonvirtual: [
        Method("A3", function impl_A3_A3 (thiz) {
            thiz.setMember("a3", 0);
        }),
        Method("set3", function impl_A3_set3 (thiz, set) {
            thiz.setMember("a3", set);
        })
    ],
    inherits: []
});

Class.define({
    name: "B",
    data: ["b"],
    virtual: [
        Method("set1v", function impl_B_set1v (thiz, set) {
            thiz.setMember("b", set);
        })
    ],
    nonvirtual: [
        Method("B", function impl_B_B (thiz) {
            thiz.setMember("b", 0);
        }),
        Method("setB", function impl_B_setB (thiz, set) {
            thiz.setMember("b", set);
        })
    ],
    inherits: ["A1", "A2", "A3"]
});



/* Expected Output:
1
2
3
4
1
6
7
5
*/

function main() {
    var b = Ptr.new_("B", []);

    b.call("set1", [1]);
    b.call("set2", [2]);
    b.call("set3", [3]);
    b.call("setB", [4]);

    console.log(b.getMember("a1"));    // 1
    console.log(b.getMember("a2"));    // 2
    console.log(b.getMember("a3"));    // 3
    console.log(b.getMember("b"));     // 4

    b.vcall("set1v", [5]);
    b.vcall("set2v", [6]);
    b.vcall("set3v", [7]);

    console.log(b.getMember("a1"));    // 1
    console.log(b.getMember("a2"));    // 6
    console.log(b.getMember("a3"));    // 7
    console.log(b.getMember("b"));     // 5
};
module.exports = main;
