var Class = require('../lib/class')
var Method = require('../lib/method')

var Ptr = require('../lib/pointer')
require('../lib/abi')

// class Fireplace {
// public:
//      int numLogs, heat, COEmission;
//      Fireplace(int numLogs, int initialHeat) {
//          this->numLogs = numLogs;
//          this->heat = initialHeat;
//          this->COEmission = 0;
//      }
//      void increaseLogs () {
//          this->numLogs += 1;
//      }
//      void calculateHeat () {
//          this->heat = numLogs * 80;
//      }
//      virtual void calculateCO() {
//          this->calculateHeat();
//          this->COEmission = this->heat / 100.0;
//      }
// }
Class.define({
    name: "Fireplace",
    data: ["numLogs", "heat", "COEmission"],
    virtual: [
        Method("calculateCO", function impl_Fireplace_calculateCO (thiz) {
            thiz.assertType("Fireplace");
            thiz.call("calculateHeat", []);
            thiz.setMember("COEmission", thiz.getMember("heat") / 100.0);
        })
    ],
    nonvirtual: [
        Method("Fireplace", function impl_Fireplace_Fireplace (thiz, numLogs,
            initialHeat) {
            thiz.assertType("Fireplace");
            thiz.setMember("numLogs", numLogs);
            thiz.setMember("heat", initialHeat);
            thiz.setMember("COEmission", 0);
        }),
        Method("increaseLogs", function impl_Fireplace_increaseLogs (thiz) {
            thiz.assertType("Fireplace");
            thiz.setMember("numLogs", thiz.getMember("numLogs") + 1);
        }),
        Method("calculateHeat", function impl_Fireplace_calculateHeat (thiz) {
            thiz.assertType("Fireplace");
            thiz.setMember("heat", thiz.getMember("numLogs") * 80);
        }),
    ],
    inherits: []
});

/* Expected Output:
1
100
0
3
100
240
2.4
3.2
Error found
Error found
Error found
Error found
4
320
3.2
*/

function main() {
    var myFireplace = Ptr.new_("Fireplace", [1, 100]); // Cold start, LOL

    console.log(myFireplace.getMember("numLogs"));    // 1
    console.log(myFireplace.getMember("heat"));       // 100
    console.log(myFireplace.getMember("COEmission")); // 0

    // Simple calls
    myFireplace.call("increaseLogs", []);
    myFireplace.call("increaseLogs", []);
    console.log(myFireplace.getMember("numLogs"));    // 3
    console.log(myFireplace.getMember("heat"));       // 100
    myFireplace.call("calculateHeat", []);
    console.log(myFireplace.getMember("heat"));       // 240
    myFireplace.vcall("calculateCO", []);
    console.log(myFireplace.getMember("COEmission")); // 2.4
    myFireplace.call("increaseLogs", []);
    myFireplace.vcall("calculateCO", []);
    console.log(myFireplace.getMember("COEmission")); // 3.2

    // Error calls

    // Expect error
    errorFound = false;
    try {
      carActuallyFunCar.vcall("increaseLogs", []);
    } catch (t) {
      errorFound = true;
    }
    if (errorFound) {
      console.log ("Error found");
    } else {
      console.log ("Error NOT found");
    }

    // Expect error
    errorFound = false;
    try {
      carActuallyFunCar.vcall("calculateHeat", []);
    } catch (t) {
      errorFound = true;
    }
    if (errorFound) {
      console.log ("Error found");
    } else {
      console.log ("Error NOT found");
    }
 
    // Expect error
    errorFound = false;
    try {
      carActuallyFunCar.call("calculateCO", []);
    } catch (t) {
      errorFound = true;
    }
    if (errorFound) {
      console.log ("Error found");
    } else {
      console.log ("Error NOT found");
    }
 
    // Expect error
    errorFound = false;
    try {
      carActuallyFunCar.call("whatFunction?", []);
    } catch (t) {
      errorFound = true;
    }
    if (errorFound) {
      console.log ("Error found");
    } else {
      console.log ("Error NOT found");
    }

    // Make sure nothing's changed
    console.log(myFireplace.getMember("numLogs"));    // 4
    console.log(myFireplace.getMember("heat"));       // 320
    console.log(myFireplace.getMember("COEmission")); // 3.2
};
module.exports = main;
