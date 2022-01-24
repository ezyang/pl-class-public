var Class = require('../lib/class')
var Method = require('../lib/method')

var Ptr = require('../lib/pointer')
require('../lib/abi')

// class Bird {
// public:
//      int featherCount, age;
//      Bird(int featherCount, int age) {
//          this->featureCount = featureCount;
//          this->age = age;
//      }
//      void grow() {
//          this->featureCount += 1;
//          this->age += 1;
//      }
//      String swim() {
//          return "Birds can't do that.";
//      }
//      virtual void hunt() {
//          this->featherCount -= 2;
//          this->grow();
//      }
//      virtual String sense() {
//          return "The bird senses another bird nearby!";
//      }
//      virtual String fly() {
//          return "CAW";
//      }
// }
//
// class Plane {
// public:
//      int weight, age, dirty;
//      Plane(int weight, int age) {
//          this->weight = weight;
//          this->age = age;
//          this->dirty = 0;
//      }
//      void clean() {
//          this->dirty = -1;
//      }
//      String swim() {
//          return "Planes can't do that.";
//      }
//      virtual String sense() {
//          return "The plane senses another plane nearby!";
//      }
//      virtual void upgrade() {
//          this->weight += 10;
//          this->age += 1;
//      }
//      virtual String fly() {
//          this->dirty = 1;
//          return "[PLANE NOISE HERE]";
//      }
// }
// class Superman: Bird, Plane {
// public:
//      int height;
//      Superman(int age, int weight, int height) {
//        this->Bird(0, age);
//        this->Plane(weight, age);
//        this->height = height;
//      }
//      String swim() {
//        return "Superman is swimming!";
//      }
//      virtual String sense() {
//          return "Superman sees!";
//      }
//      virtual void upgrade() {
//        this->weight += 1;
//        this->height += 1;
//        this->age += 1;
//      }
//      virtual String fly() {
//        return "Superman is flying!";
//      }
// }
Class.define({
    name: "Bird",
    data: ["featherCount", "age"],
    virtual: [
        Method("hunt", function impl_Bird_hunt (thiz) {
            thiz.setMember("featherCount", thiz.getMember("featherCount") - 2);
            thiz.call("grow", []);
        }),
        Method("sense", function impl_Bird_sense (thiz) {
          return "The bird senses another bird nearby!";
        }),
        Method("fly", function impl_Bird_fly (thiz) {
          return "CAW";
        })
    ],
    nonvirtual: [
        Method("Bird", function impl_Bird_Bird (thiz, featherCount, age) {
            thiz.setMember("featherCount", featherCount);
            thiz.setMember("age", age);
        }),
        Method("grow", function impl_Bird_grow (thiz) {
            thiz.setMember("featherCount", thiz.getMember("featherCount") + 1);
            thiz.setMember("age", thiz.getMember("age") + 1);
        }),
        Method("swim", function impl_Bird_swim (thiz) {
            return "Birds can't do that.";
        })
    ],
    inherits: []
});

Class.define({
    name: "Plane",
    data: ["weight", "age", "dirty"],
    virtual: [
        Method("sense", function impl_Plane_sense (thiz) {
          return "The plane senses another plane nearby!";
        }),
        Method("upgrade", function impl_Plane_upgrade (thiz) {
            thiz.setMember("weight", thiz.getMember("weight") + 10);
            thiz.setMember("age", thiz.getMember("age") + 1);
        }),
        Method("fly", function impl_Plane_fly (thiz) {
          thiz.setMember("dirty", 1);
          return "[PLANE NOISE HERE]";
        })
    ],
    nonvirtual: [
        Method("Plane", function impl_Plane_Plane (thiz, weight, age) {
            thiz.setMember("weight", weight);
            thiz.setMember("age", age);
            thiz.setMember("dirty", 0);
        }),
        Method("clean", function impl_Plane_clean (thiz) {
            thiz.setMember("dirty", -1);
        }),
        Method("swim", function impl_Plane_swim (thiz) {
            return "Planes can't do that.";
        })
    ],
    inherits: []
});

Class.define({
    name: "Superman",
    data: ["height"],
    virtual: [
        Method("sense", function impl_Superman_sense (thiz) {
          return "Superman sees!";
        }),
        Method("upgrade", function impl_Superman_upgrade (thiz) {
            thiz.setMember("weight", thiz.getMember("weight") + 1);
            thiz.setMember("height", thiz.getMember("height") + 1);
            thiz.setMember("age", thiz.getMember("age") + 1);
        }),
        Method("fly", function impl_Superman_fly (thiz) {
          return "Superman is flying!";
        })
    ],
    nonvirtual: [
        Method("Superman", function impl_Superman_Superman (thiz, age, weight,
            height) {
            thiz.call("Bird", [0, age]);
            thiz.call("Plane", [weight, age]);
            thiz.setMember("height", height);
        }),
        Method("swim", function impl_Superman_swim (thiz) {
            return "Superman is swimming!";
        })
    ],
    inherits: ["Bird", "Plane"]
});

/* Expected Output:
0
40
220
510
0
1
41
-1
0
42
Superman is swimming!
Birds can't do that.
Planes can't do that.
Superman is flying!
Superman is flying!
Superman is flying!
40
43
43
Superman sees!
Superman sees!
Superman sees!
*/

function main() {
    var superman = Ptr.new_("Superman", [40, 220, 510]);

    // Simple stat checks
    console.log(superman.getMember("featherCount")); // 0
    console.log(superman.getMember("age")); // 40
    console.log(superman.getMember("weight")); // 220
    console.log(superman.getMember("height")); // 510
    console.log(superman.getMember("dirty")); // 0

    // Test non-conflicting non-virtual calls
    superman.call("clean", []);
    superman.call("grow", []);

    // Check relavent stats
    console.log(superman.getMember("featherCount")); // 1
    console.log(superman.getMember("age")); // 41
    console.log(superman.getMember("dirty")); // -1

    // Test non-conflicting virtual call
    superman.vcall("hunt", []);
    console.log(superman.getMember("featherCount")); // 0
    console.log(superman.getMember("age")); // 42

    // Test conflicting non-virtual call
    console.log(superman.call("swim", []));
    console.log(superman.cast("Bird").call("swim", []));
    console.log(superman.cast("Plane").call("swim", []));

    // Test conflicting virtual call
    console.log(superman.vcall("fly", []));
    console.log(superman.cast("Bird").vcall("fly", []));
    console.log(superman.cast("Plane").vcall("fly", []));

    // Due to object layout issues, Plane's age is never updated!
    superman.vcall("upgrade", []);
    console.log(superman.cast("Plane").getMember("age")); // 40
    console.log(superman.cast("Bird").getMember("age"));  // 43
    console.log(superman.getMember("age"));               // 43

    console.log(superman.vcall("sense", []));               // Superman sees!
    console.log(superman.cast("Bird").vcall("sense", []));  // Superman sees!
    console.log(superman.cast("Plane").vcall("sense", [])); // Superman sees!
};
module.exports = main;
