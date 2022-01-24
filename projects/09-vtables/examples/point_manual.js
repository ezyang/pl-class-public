var Meta = require('../lib/metadata')
var Layout = require('../lib/layout')

function impl_Point_Point(thiz, x, y) {
    thiz.assertType("Point");
    thiz.setMember("x", x);
    thiz.setMember("y", y);
}

function impl_Point_move(thiz, dx, dy) {
    thiz.assertType("Point");
    thiz.setMember("x", thiz.getMember("x") + dx);
    thiz.setMember("y", thiz.getMember("y") + dy);
}

function impl_ScaledPoint_ScaledPoint(thiz, x, y, s) {
    thiz.assertType("ScaledPoint");
    thiz.setMember("x", x);
    thiz.setMember("y", y);
    thiz.setMember("s", s);
}

function impl_ScaledPoint_move(thiz, dx, dy) {
    thiz.assertType("ScaledPoint");
    thiz.setMember("x", thiz.getMember("x") + dx * thiz.getMember("s"));
    thiz.setMember("y", thiz.getMember("y") + dy * thiz.getMember("s"));
}

function impl_ScaledPoint_scale(thiz, s) {
    thiz.assertType("ScaledPoint");
    thiz.setMember("s", s);
}

Meta.setCasts("Point", {});
Meta.setParents("Point", []);
Meta.setDataOffset("Point::x", 1);
Meta.setDataOffset("Point::y", 2);
Meta.setSizeof("Point", 3);
Meta.setGlobal("Point", "Point", impl_Point_Point);
Meta.setVirtualOffsets("Point", {"move": 0});
var vtable = [ Layout.VEntry(impl_Point_move, "Point", 0) ];
Meta.setTemplate("Point", [Layout.Layout(0, "Point", vtable)]);

Meta.setCasts("ScaledPoint", {"Point": 0});
Meta.setParents("ScaledPoint", ["Point"]);
Meta.setDataOffset("ScaledPoint::s", 3);
Meta.setSizeof("ScaledPoint", 4);
Meta.setGlobal("ScaledPoint", "ScaledPoint", impl_ScaledPoint_ScaledPoint);
Meta.setVirtualOffsets("ScaledPoint", {"move": 0, "scale": 1});
var vtable = [
    Layout.VEntry(impl_ScaledPoint_move, "ScaledPoint", 0),
    Layout.VEntry(impl_ScaledPoint_scale, "ScaledPoint", 0)
];
Meta.setTemplate("ScaledPoint", [Layout.Layout(0, "ScaledPoint", vtable)]);
