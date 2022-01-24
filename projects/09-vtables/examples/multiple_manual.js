var Meta = require('../lib/metadata')
var Layout = require('../lib/layout')

function impl_A_f (thiz) {
    thiz.assertType("A");
    console.log("A::f()");
    console.log(thiz.getMember("x"));
}

function impl_B_g (thiz) {
    thiz.assertType("B");
    console.log("B::g()");
    console.log(thiz.getMember("y"));
}

function impl_B_f (thiz) {
    thiz.assertType("B");
    console.log("B::f()");
    console.log(thiz.getMember("y"));
}

function impl_C_f (thiz) {
    thiz.assertType("C");
    console.log("C::f()");
    console.log(thiz.getMember("x"));
    console.log(thiz.getMember("y"));
    console.log(thiz.getMember("z"));
}

function impl_C_C (thiz, x, y, z) {
    thiz.setMember("x", x);
    thiz.setMember("y", y);
    thiz.setMember("z", z);
}

Meta.setCasts("A", {});
Meta.setParents("A", []);
Meta.setDataOffset("A::x", 1);
Meta.setSizeof("A", 2);
Meta.setVirtualOffsets("A", {"f": 0});
var vtable = [ Layout.VEntry(impl_A_f, "A", 0) ];
Meta.setTemplate("A", [Layout.Layout(0, "A", vtable)]);

Meta.setCasts("B", {});
Meta.setParents("B", []);
Meta.setDataOffset("B::y", 1);
Meta.setSizeof("B", 2);
Meta.setVirtualOffsets("B", {"g": 0, "f": 1});
var vtable = [ Layout.VEntry(impl_B_g, "B", 0), Layout.VEntry(impl_B_f, "B", 0) ];
Meta.setTemplate("B", [Layout.Layout(0, "B", vtable)]);

Meta.setCasts("C", {"A": 0, "B": 2});
Meta.setParents("C", ["A", "B"]);
Meta.setDataOffset("C::z", 4);
Meta.setSizeof("C", 5);
Meta.setGlobal("C", "C", impl_C_C);
Meta.setVirtualOffsets("C", {});
var vtable_as_a = [
    Layout.VEntry(impl_C_f, "C", 0),
];
var vtable_as_b = [
    Layout.VEntry(impl_B_g, "B", 0),
    Layout.VEntry(impl_C_f, "C", -2)
];
Meta.setTemplate("C", [Layout.Layout(0, "C", vtable_as_a), Layout.Layout(2, "B", vtable_as_b)])
