
var Class = require('../lib/class')
var Method = require('../lib/method')

var Ptr = require('../lib/pointer')
require('../lib/abi')

function impl_Widget_outputV (thiz) {
    console.log("Virtual fn: b=" + thiz.getMember("b"));
}
function impl_Widget_output1 (thiz) {
    console.log("Non-virtual fn");
}
function impl_Widget_output2 (thiz) {
    console.log("Non-virtual fn: b=" + thiz.getMember("b"));
}
function impl_Widget_Widget (thiz, b) {
    thiz.setMember("b", b);
}

Class.define({
    name: "Widget",
    data: ["b"],
    virtual: [
        Method("outputV", impl_Widget_outputV)
    ],
    nonvirtual: [
        Method("output1", impl_Widget_output1),
        Method("output2", impl_Widget_output2),
        Method("Widget", impl_Widget_Widget)
    ],
    inherits: []
});

function main() {
    var a = Ptr.new_("Widget", [2]);
    a.vcall("outputV", []);
}

function main_outputV() {
    var a = Ptr.null("Widget");
    a.vcall("outputV", []);
}
function main_output1() {
    var a = Ptr.null("Widget");
    a.call("output1", []);
}
function main_output2() {
    var a = Ptr.null("Widget");
    a.call("output2", []);
}

module.exports = [main, main_outputV, main_output1, main_output2];
