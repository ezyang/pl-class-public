var Meta = require('./metadata')
var Layout = require('./layout')
var extend = require('util')._extend;

function copyOffsets(m) { return extend({}, m); }
function compile() { Meta.getClasses().forEach(compileClass); }
module.exports = compile;

function compileClass(c) {
// BEGIN compileClass (DO NOT DELETE THIS LINE)
// ANSWER HERE
// END compileClass (DO NOT DELETE THIS LINE)
}
