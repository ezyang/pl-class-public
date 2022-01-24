/**
 * The compiler metadata API.  As you compile a class definition,
 * you will make decisions about the data layout and vtable layout
 * of your definitions.  The functions in this module are responsible
 * for storing this information for you, so that the client code can
 * look things up.
 *
 * In a real compiler, most of these definitions would be inlined
 * straight into their use-sites, or simply exist as static compile
 * time data shipped with the binary.
 */

require("util-is");
var util = require("util");
var assert = require("assert");
var Layout = require("./layout");
var Class = require("./class");

var classes = [];
var data_offsets = {};
var all_virtual_offsets = {};
var sizes = {}
var globals = {}
var templates = {}
var all_casts = {}
var all_parents = {}

// ----------------------------------------------------------------- //

//                          Getters

// ----------------------------------------------------------------- //

// Returns an array of all Classes defined in the program.
function getClasses() {
    return classes;
}
exports.getClasses = getClasses;

// Function to define a Class.  See lib/class.js for a usage example.
Class.define = function(info) {
    classes.push(Class.Class.create(info));
}

// hasDataOffset("Point", "x") returns true if "x" is an
// instance variable defined in the class "Point".
// (It is not an error to access an instance variable
// which does not have its data offset defined at some
// class; in this case, we will find a parent which does
// have this data offset defined and cast to it.)
function hasDataOffset(ty, sym) {
    return ty + "::" + sym in data_offsets;
}
exports.hasDataOffset = hasDataOffset;

// getDataOffset("Point", "x") returns the offset from
// the base of a pointer of type Point* at which the instance variable
// named "x" is stored.
function getDataOffset(ty, sym) {
    return data_offsets[ty + "::" + sym];
}
exports.getDataOffset = getDataOffset;

// hasVirtualOffset("Point", "move") returns true if
// "move" is a virtual function defined in the class Point.
// (It is not an error to access an virtual function
// which does not have its offset defined in the
// current class; in this case, we will find a parent which does
// have this virtual function defined and cast to it.)
function hasVirtualOffset(ty, sym) {
    return sym in all_virtual_offsets[ty];
}
exports.hasVirtualOffset = hasVirtualOffset;

// getVirtualOffset("Point", "move") returns the offset
// from the base of the virtual table from an object of
// type Point at which the virtual function is stored.
function getVirtualOffset(ty, sym) {
    return all_virtual_offsets[ty][sym];
}
exports.getVirtualOffset = getVirtualOffset;

// Returns all of the virtual offsets for a type,
// as a dictionary mapping virtual function name to offset.
function getVirtualOffsets(ty) {
    return all_virtual_offsets[ty];
}
exports.getVirtualOffsets = getVirtualOffsets;

// Returns an array of strings of the parents of the class sym.
function getParents(sym) {
    return all_parents[sym];
}
exports.getParents = getParents;

// Returns the delta adjustment (upwards) for casting from ty_from to
// ty_to.  In the absence of multiple inheritance this will always be
// zero.  This function does not know about all transitive casts; so it
// may be the case that getCast('C', 'B') and getCast('B', 'A') are
// defined, but getCast('C', 'A') is not.  The returned integer is
// always non-negative.
function getCast(ty_from, ty_to) {
    return all_casts[ty_from][ty_to];
}
exports.getCast = getCast;

// Returns all immediate casts from ty_from, as a dictionary from
// destination type to delta. (see also getCast)
function getCasts(ty_from) {
    return all_casts[ty_from];
}
exports.getCasts = getCasts;

// Returns the size of objects of type sym.
function sizeof(sym) {
    return sizes[sym];
}
exports.sizeof = sizeof;

// hasGlobal("P", "P") returns true if we have defined a
// non-virtual function P::P.
function hasGlobal(ty, sym) {
    return ty + "::" + sym in globals;
}
exports.hasGlobal = hasGlobal;

// getGlobal("P", "P") returns the non-virtual function P::P.
function getGlobal(ty, sym) {
    return globals[ty + "::" + sym];
}
exports.getGlobal = getGlobal;

// getTemplate("C") returns the template (an array of Layout objects)
// associated with the class C.
function getTemplate(sym) {
    return templates[sym];
}
exports.getTemplate = getTemplate;

// Debugging function which dumps all of the metadata we know about.
function dump() {
    console.log("data_offsets = ", util.inspect(data_offsets, false, 10));
    console.log("all_virtual_offsets = ", util.inspect(all_virtual_offsets, false, 10));
    console.log("sizes = ", util.inspect(sizes, false, 10));
    console.log("globals = ", util.inspect(globals, false, 10));
    console.log("all_casts = ", util.inspect(all_casts, false, 10));
    console.log("all_parents = ", util.inspect(all_parents, false, 10));
    console.log("templates = ", util.inspect(templates, false, 10));
}
exports.dump = dump;

// ----------------------------------------------------------------- //

//                          Setters

// ----------------------------------------------------------------- //

// setParents("A", ["B", "C"]) sets the parent classes of A to
// be B and C.
function setParents(sym, parents) {
    assert(util.isArray(parents));
    parents.forEach(function(x) {assert(util.isString(x))});
    all_parents[sym] = parents;
}
exports.setParents = setParents;

// setDataOffset("A::x", "1") specifies that the instance variable
// x from class A is stored in slot 1.  (Reminder: slot 0 is
// reserved for the vtable pointer.)
function setDataOffset(sym, off) {
    assert(util.isNumber(off));
    data_offsets[sym] = off;
}
exports.setDataOffset = setDataOffset;

// setVirtualOffsets("A", {"f": 1}) specifies that the virtual function
// f in class A is in slot 1 of the virtual table.
function setVirtualOffsets(sym, offs) {
    assert(util.isPureObject(offs));
    for (var name in offs) {
        assert(util.isNumber(offs[name]));
    }
    all_virtual_offsets[sym] = offs;
}
exports.setVirtualOffsets = setVirtualOffsets;

// setSizeOf("A", 2) states that a new object of class A has size 2
// (i.e. has two slots.)
function setSizeof(sym, size) {
    assert(util.isNumber(size));
    sizes[sym] = size;
}
exports.setSizeof = setSizeof;

// setGlobal("A", "f", fun) says that the class A has a non-virtual
// function f which has implementation fun.
function setGlobal(ty, sym, global) {
    assert(util.isFunction(global));
    globals[ty + "::" + sym] = global;
}
exports.setGlobal = setGlobal;

// setTemplate("A", [Layout(0, "Point", vtable)]) says that, for an
// object of type A, the contents of slot zero is a vtable for the
// Point class.
function setTemplate(sym, template) {
    assert(util.isArray(template));
    template.forEach(function (x) {
        assert(x.isLayout);
    })
    templates[sym] = template;
}
exports.setTemplate = setTemplate;

// setCasts("B", {"A": 2}) states that to cast an object of type
// B to A, you need to offset the pointer by two (add two).
function setCasts(sym, casts) {
    assert(util.isPureObject(casts));
    for (var name in casts) {
        assert(util.isNumber(casts[name]));
    }
    all_casts[sym] = casts;
}
exports.setCasts = setCasts;
