var Class = require('../lib/class')
var Method = require('../lib/method')

var Ptr = require('../lib/pointer')
require('../lib/abi')

//    A
//   / \
//  B   C
//   \ /
//    D
//
// class A {
// public:
//      int aa, ab, ac, ad, ava, avb, avc, avd;
//      A() {
//          this->aa = 0;
//          this->ab = 0;
//          this->ac = 0;
//          this->ad = 0;
//          this->ava = 0;
//          this->avb = 0;
//          this->avc = 0;
//          this->avd = 0;
//      }
//
//      void set(int set) {
//          this->aa = set;
//      }
//
//      virtual void setV(int set) {
//          this->ava = set;
//      }
//
//      virtual String vCast() {
//          return "A_vCast";
//      }
// }
//
// class B : A {
// public:
//      B() {
//          super();
//      }
//
//      void set(int set) {
//          this->ab = set;
//      }
//
//      String ambi() {
//          return "B_ambi";
//      }
//
//      virtual void setV(int set) {
//          this->avb = set;
//      }
// }
//
// class C : A {
// public:
//      C() {
//          super();
//      }
//
//      void set(int set) {
//          this->ac = set;
//      }
//
//      String ambi() {
//          return "C_ambi";
//      }
//
//      virtual void setV(int set) {
//          this->avc = set;
//      }
// }
//
// class D : B, C {
// public:
//      D() {
//          super();
//      }
//
//      void set(int set) {
//          this->ad = set;
//      }
//
//      virtual void setV(int set) {
//          this->avd = set;
//      }
//
//      virtual String vCast() {
//          return "D_vCast";
//      }
// }

Class.define({
    name: "A",
    data: ["aa", "ab", "ac", "ad", "ava", "avb", "avc", "avd"],
    virtual: [
        Method("setV", function impl_A_setV (thiz, set) {
            thiz.setMember("ava", set);
        }),
        Method("vCast", function impl_A_vCast (thiz) {
            return "A_vCast";
        })
    ],
    nonvirtual: [
        Method("A", function impl_A_A (thiz) {
            thiz.setMember("aa", 0); thiz.setMember("ab", 0);
            thiz.setMember("ac", 0); thiz.setMember("ad", 0);
            thiz.setMember("ava", 0); thiz.setMember("avb", 0);
            thiz.setMember("avc", 0); thiz.setMember("avd", 0);
        }),
        Method("set", function impl_A_set (thiz, set) {
            thiz.setMember("aa", set);
        })
    ],
    inherits: []
});

Class.define({
    name: "B",
    data: [],
    virtual: [
        Method("setV", function impl_B_setV (thiz, set) {
            thiz.setMember("avb", set);
        })
    ],
    nonvirtual: [
        Method("B", function impl_B_B (thiz) {
            thiz.call("A", []);
        }),
        Method("set", function impl_B_set (thiz, set) {
            thiz.setMember("ab", set);
        }),
        Method("ambi", function impl_B_ambi (thiz) {
            return "B_ambi";
        })
    ],
    inherits: ["A"]
});

Class.define({
    name: "C",
    data: [],
    virtual: [
        Method("setV", function impl_C_setV (thiz, set) {
            thiz.setMember("avc", set);
        })
    ],
    nonvirtual: [
        Method("C", function impl_C_C (thiz) {
            thiz.call("A", []);
        }),
        Method("set", function impl_C_set (thiz, set) {
            thiz.setMember("ac", set);
        }),
        Method("ambi", function impl_C_ambi (thiz) {
            return "C_ambi";
        })
    ],
    inherits: ["A"]
});

Class.define({
    name: "D",
    data: [],
    virtual: [
        Method("setV", function impl_D_setV (thiz, set) {
            thiz.setMember("avd", set);
        }),
        Method("vCast", function impl_D_vCast (thiz) {
            return "D_vCast";
        })
    ],
    nonvirtual: [
        Method("D", function impl_D_D (thiz) {
            thiz.call("B", []);
            thiz.call("C", []);
        }),
        Method("set", function impl_D_set (thiz, set) {
            thiz.setMember("ad", set);
        })
    ],
    inherits: ["B", "C"]
});

/* Expected Output:
1
1
1
0
2
2
2
0
0
0
0
3
0
4
0
1
5
4
0
1
0
0
0
6
6
6
0
B_ambi
*/

function main() {
    var d = Ptr.new_("D", []);

    // Due to object layout issues, C's ad is not changed
    d.call("set", [1]);
    console.log(d.getMember("ad"));           // 1
    console.log(d.cast("A").getMember("ad")); // 1
    console.log(d.cast("B").getMember("ad")); // 1
    console.log(d.cast("C").getMember("ad")); // 0

    d.vcall("setV", [2]);
    console.log(d.getMember("avd"));          // 2
    console.log(d.cast("A").getMember("avd")); // 2
    console.log(d.cast("B").getMember("avd")); // 2
    console.log(d.cast("C").getMember("avd")); // 0

    d.cast("C").call("set", [3]);
    console.log(d.getMember("ac"));           // 0
    console.log(d.cast("A").getMember("ac")); // 0
    console.log(d.cast("B").getMember("ac")); // 0
    console.log(d.cast("C").getMember("ac")); // 3

    d.cast("B").call("set", [4]);
    console.log(d.getMember("aa")); // 0
    console.log(d.getMember("ab")); // 4
    console.log(d.getMember("ac")); // 0
    console.log(d.getMember("ad")); // 1

    d.cast("A").call("set", [5]);
    console.log(d.getMember("aa")); // 5
    console.log(d.getMember("ab")); // 4
    console.log(d.getMember("ac")); // 0
    console.log(d.getMember("ad")); // 1

    d.cast("A").vcall("setV", [6]);
    console.log(d.getMember("ava")); // 0
    console.log(d.getMember("avb")); // 0
    console.log(d.getMember("avc")); // 0
    console.log(d.getMember("avd")); // 6
    console.log(d.cast("A").getMember("avd")); // 6
    console.log(d.cast("B").getMember("avd")); // 6
    console.log(d.cast("C").getMember("avd")); // 0

    console.log(d.call("ambi", [])); // B_ambi
    console.log(d.cast("B").vcall("vCast", [])); // D_vCast
    console.log(d.cast("C").vcall("vCast", [])); // D_vCast
};
module.exports = main;
