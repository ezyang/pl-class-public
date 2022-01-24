var adt = require('adt'), data = adt.data, only = adt.only;

/**
 * Typed pointers to arrays.  The backing array is mutable, but pointers
 * themselves are pure.  Since JavaScript doesn't have a type, we build
 * in the type of a pointer into the data type itself; casting changes
 * the type (and might change the offset too, in C++).
 */

// Represents a pointer with some static type.  For example:
//
//      p = new C();
//
// Supposing that instances of C take five words of space (and
// the constructor initializes the entire body to zeros).  Then
// you would have the pointer Ptr([0,0,0,0,0], 0, "C"), stating
// you have a pointer to the zeroth element of a backing array of five
// zeroed words, and your pointer's type is C.
var Ptr = data(function () {
    return { Ptr : {
        array : adt.any, // only(Array),
        offset : only(Number),
        type : only(String)
    } };
});

// p.deref() is equivalent to the C code "*p".  For example:
//
//  Ptr([4,8,12], 1, "A").deref() == 8
//
Ptr.prototype.deref = function(d) {
    if (this.array === undefined) {
        throw new Error("Null pointer dereference");
    }
    if (this.offset + d < 0 || this.offset + d >= this.array.length) {
        throw new Error("Out of bounds pointer reference");
    }
    return this.array[this.offset + d];
}

// p.write(d, v) is equivalent to the C code *((int*)p + d) = v.  For example:
//
//  mem = [0,0,0];
//  Ptr(mem, 1, "A").write(1, 8);
//  console.log(mem); // [0,0,8]
Ptr.prototype.write = function(d, x) {
    if (this.array === undefined) {
        throw new Error("Null pointer dereference");
    }
    if (this.offset + d < 0 || this.offset + d >= this.array.length) {
        throw new Error("Out of bounds pointer reference");
    }
    this.array[this.offset + d] = x;
}

// p.add(d, "A") is equivalent to the C code (A*)((int*)p + d)
Ptr.prototype.add = function(d, ty) {
    return Ptr.Ptr(this.array, this.offset + d, ty);
}

// p.assertType("A") asserts that a pointer is statically typed as A.
// Since we don't have a typechecker, we have to run this check at
// runtime.
Ptr.prototype.assertType = function(ty) {
    if (this.type != ty) {
        throw new Error("Could not match expected type " + ty + " with actual type " + this.type);
    }
}

// Ptr.null is an invalid pointer which cannot be dereferenced.
Ptr.null = function(ty) {
    return Ptr.Ptr(undefined, 0, ty);
}

module.exports = Ptr;
