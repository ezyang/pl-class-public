var Class = require('../lib/class')
var Method = require('../lib/method')

var Ptr = require('../lib/pointer')
require('../lib/abi')

//     A
//    / \
//   B   \
//  / \   \
// C   D   E
//  \ /   /
//   |   /
//   |  /
//   | /
//   |/
//   F
//
// class A {
// public:
//      int a;
//      A() {
//          this->a = 0;
//      }
//      String speak() {
//          return "A";
//      }
//      String speakV() {
//          return "AV";
//      }
// }
// class B:A {
// public:
//      int b;
//      B() {
//          this->b = 0;
//      }
//      String speak() {
//          return "B";
//      }
//      String speakV() {
//          return "BV";
//      }
// }
// class E:A {
// public:
//      int e;
//      E() {
//          this->e = 0;
//      }
//      String speak() {
//          return "E";
//      }
//      String speakV() {
//          return "EV";
//      }
// }
// class C:B {
// public:
//      int c;
//      C() {
//          this->c = 0;
//      }
//      String speak() {
//          return "C";
//      }
//      String speakV() {
//          return "CV";
//      }
// }
// class D:B {
// public:
//      int d;
//      D() {
//          this->d = 0;
//      }
//      String speak() {
//          return "D";
//      }
//      String speakV() {
//          return "DV";
//      }
// }
// class F:C, D, E {
// public:
//      int f;
//      D() {
//          this->f = 0;
//      }
//      String speak() {
//          return "F";
//      }
//      String speakV() {
//          return "FV";
//      }
// }


Class.define({
    name: "A",
    data: ["a"],
    virtual: [
        Method("speakV", function impl_A_speakV (thiz) {
            return "AV";
        })
    ],
    nonvirtual: [
        Method("A", function impl_A_A (thiz) {
            thiz.setMember("a", 0);
        }),
        Method("speak", function impl_A_speak (thiz) {
            return "A";
        })
    ],
    inherits: []
});

Class.define({
    name: "B",
    data: ["b"],
    virtual: [
        Method("speakV", function impl_B_speakV (thiz) {
            return "BV";
        })
    ],
    nonvirtual: [
        Method("B", function impl_B_B (thiz) {
            thiz.setMember("b", 0);
        }),
        Method("speak", function impl_B_speak (thiz) {
            return "B";
        })
    ],
    inherits: ["A"]
});

Class.define({
    name: "E",
    data: ["e"],
    virtual: [
        Method("speakV", function impl_E_speakV (thiz) {
            return "EV";
        })
    ],
    nonvirtual: [
        Method("E", function impl_E_E (thiz) {
            thiz.setMember("e", 0);
        }),
        Method("speak", function impl_E_speak (thiz) {
            return "E";
        })
    ],
    inherits: ["A"]
});

Class.define({
    name: "C",
    data: ["c"],
    virtual: [
        Method("speakV", function impl_C_speakV (thiz) {
            return "CV";
        })
    ],
    nonvirtual: [
        Method("C", function impl_C_C (thiz) {
            thiz.setMember("c", 0);
        }),
        Method("speak", function impl_C_speak (thiz) {
            return "C";
        })
    ],
    inherits: ["B"]
});

Class.define({
    name: "D",
    data: ["d"],
    virtual: [
        Method("speakV", function impl_D_speakV (thiz) {
            return "DV";
        })
    ],
    nonvirtual: [
        Method("D", function impl_D_D (thiz) {
            thiz.setMember("d", 0);
        }),
        Method("speak", function impl_D_speak (thiz) {
            return "D";
        })
    ],
    inherits: ["B"]
});

Class.define({
    name: "F",
    data: ["f"],
    virtual: [
        Method("speakV", function impl_F_speakV (thiz) {
            return "FV";
        })
    ],
    nonvirtual: [
        Method("F", function impl_F_F (thiz) {
            thiz.setMember("f", 0);
        }),
        Method("speak", function impl_F_speak (thiz) {
            return "F";
        })
    ],
    inherits: ["C", "D", "E"]
});

/* Expected Output:
A
B
C
D
E
F
FV
FV
FV
FV
FV
FV
*/

function main() {
    var f = Ptr.new_("F", []);

    console.log(f.cast("A").call("speak", []));
    console.log(f.cast("B").call("speak", []));
    console.log(f.cast("C").call("speak", []));
    console.log(f.cast("D").call("speak", []));
    console.log(f.cast("E").call("speak", []));
    console.log(f.call("speak", []));

    console.log(f.cast("A").vcall("speakV", []));
    console.log(f.cast("B").vcall("speakV", []));
    console.log(f.cast("C").vcall("speakV", []));
    console.log(f.cast("D").vcall("speakV", []));
    console.log(f.cast("E").vcall("speakV", []));
    console.log(f.vcall("speakV", []));
};
module.exports = main;
