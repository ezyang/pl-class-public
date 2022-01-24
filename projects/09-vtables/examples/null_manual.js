var Meta = require('../lib/metadata')
var Layout = require('../lib/layout')

// Here are implementations of functions you may find useful when
// making calls to the Meta API.
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

// Calls to Meta API here (there should be NINE setXXX() calls)
// BEGIN null-meta
// ANSWER HERE
// END null-meta

// What is the result when we run main, and why?
// BEGIN null-main
// ANSWER HERE
// END null-main

// What is the result when we run main_outputV, and why?
// BEGIN null-main_outputV
// ANSWER HERE
// END null-main_outputV

// What is the result when we run main_output1, and why?
// BEGIN null-main_output1
// ANSWER HERE
// END null-main_output1

// What is the result when we run main_output2, and why?
// BEGIN null-main_output2
// ANSWER HERE
// END null-main_output2
