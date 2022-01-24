var Class = require('../lib/class')
var Method = require('../lib/method')

var Ptr = require('../lib/pointer')
require('../lib/abi')

// Inheritance and virtual/non-virtual test

// class Vehicle {
// public:
//      int color, wheels, topSpeed, position;
//      Vehicle(int color, int wheels, int topSpeed) {
//          this->color = color;
//          this->wheels = wheels;
//          this->topSpeed = topSpeed;
//          this->position = 0;
//      }
//      void setColor(int newColor) {
//          this->color = newColor;
//      }
//      virtual void move(int time) {
//          this->position += time * topSpeed;
//      }
// }
//
// class SportsCar : Vehicle {
// public:
//      int turboSpeed;
//      SportsCar(int color, int wheels, int topSpeed, int turboSpeed) {
//          this->Vehicle(color, wheels, topSpeed);
//          this->turboSpeed = turboSpeed;
//      }
//      void setTurboSpeed(int newTurboSpeed) {
//          this->turboSpeed = newTurboSpeed;
//      }
//      virtual void move(int time) {
//          this->position += time * turboSpeed;
//      }
// }
// class Truck : Vehicle {
// public:
//      Truck(int color, int wheels, int topSpeed) {
//          this->Vehicle(color, wheels, topSpeed);
//      }
//      void setColor(int newColor) {
//          // Do nothing
//      }
// }

Class.define({
    name: "Vehicle",
    data: ["color", "wheels", "topSpeed", "position"],
    virtual: [
        Method("move", function impl_Vehicle_move (thiz, time) {
            thiz.setMember("position", thiz.getMember("position") + 
                (thiz.getMember("topSpeed") * time));
        })
    ],
    nonvirtual: [
        Method("Vehicle", function impl_Vehicle_Vehicle (thiz, color, wheels,
                topSpeed) {
            thiz.assertType("Vehicle");
            thiz.setMember("color", color);
            thiz.setMember("wheels", wheels);
            thiz.setMember("topSpeed", topSpeed);
            thiz.setMember("position", 0);
        }),
        Method("setColor", function impl_Vehicle_setColor (thiz, newColor) {
            thiz.setMember("color", newColor);
        })
    ],
    inherits: []
});

Class.define({
    name: "SportsCar",
    data: ["turboSpeed"],
    virtual: [
        Method("move", function impl_SportsCar_move (thiz, time) {
            thiz.assertType("SportsCar");
            thiz.setMember("position", thiz.getMember("position") + 
                (thiz.getMember("turboSpeed") * time));
        })
    ],
    nonvirtual: [
        Method("SportsCar", function impl_SportsCar_SportsCar (thiz, color,
                wheels, topSpeed, turboSpeed) {
            thiz.assertType("SportsCar");
            thiz.call("Vehicle", [color, wheels, topSpeed]);
            thiz.setMember("turboSpeed", turboSpeed);
        }),
        /*
        Method("setColor", function impl_SportsCar_setColor (thiz, newColor) {
            thiz.assertType("SportsCar");
            thiz.setMember("color", newColor);
        }),*/
        Method("setTurboSpeed", function impl_SportsCar_setTurboSpeed (thiz,
              newTurboSpeed) {
            thiz.assertType("SportsCar");
            thiz.setMember("turboSpeed", newTurboSpeed);
        })
    ],
    inherits: ["Vehicle"]
});

Class.define({
    name: "Truck",
    data: [],
    virtual: [],
    nonvirtual: [
        Method("Truck", function impl_Truck_Truck (thiz, color, wheels,
            topSpeed) {
            thiz.call("Vehicle", [color, wheels, topSpeed]);
        }),
        Method("setColor", function impl_Truck_setColor (thiz, newColor) {
            thiz.assertType("Truck");
            // Do nothing.
        })
    ],
    inherits: ["Vehicle"]
});

/* Expected Output:
1
1
0
180
2
Error found
540
200
1140
Error found
Error found
*/

function main() {
    var car = Ptr.new_("Vehicle", [0, 4, 120]);
    var funCar = Ptr.new_("SportsCar", [0, 4, 140, 180]);
    var bigCar = Ptr.new_("Truck", [0, 10, 100]);

    // Try setting colors (Truck's color should NOT change)
    car.call("setColor", [1]);
    funCar.call("setColor", [1]);
    bigCar.call("setColor", [1]);

    console.log(car.getMember("color"));    // 1
    console.log(funCar.getMember("color")); // 1
    console.log(bigCar.getMember("color")); // 0

    // Fun with virtual and non-virtual functions (SportCar's move, Truck's
    // setcolor)
    var carActuallyFunCar = funCar.cast("Vehicle");
    var carActuallyBigCar = bigCar.cast("Vehicle");

    carActuallyFunCar.vcall("move", [1]);
    carActuallyBigCar.call("setColor", [2]);

    console.log(funCar.getMember("position")); // 180
    console.log(bigCar.getMember("color"));    // 2

    // Expect error
    errorFound = false;
    try {
      carActuallyFunCar.call("setTurboSpeed", [200]);
    } catch (t) { 
      errorFound = true; 
    }
    if (errorFound) {
      console.log ("Error found");
    } else {
      console.log ("Error NOT found");
    }

    // Virtual funtion test
    carActuallyFunCar.vcall("move", [2]);
    carActuallyBigCar.vcall("move", [2]);

    console.log(funCar.getMember("position")); // 540
    console.log(bigCar.getMember("position")); // 200

    // Non-virtual + virtual interaction
    funCar.call("setTurboSpeed", [200]);
    carActuallyFunCar.vcall("move", [3]);
    console.log(funCar.getMember("position")); // 1140

    // Expect error
    errorFound = false;
    try {
      car.call("setTurboSpeed", [210]);
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
      carActuallyFunCar.call("setTurboSpeed", [210]);
    } catch (t) {
      errorFound = true;
    }
    if (errorFound) {
      console.log ("Error found");
    } else {
      console.log ("Error NOT found");
    }
}
module.exports = main;
