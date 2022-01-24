#!/usr/bin/env nodejs

require('./lib/abi')

var point_main = require('./examples/point');
var multiple_main = require('./examples/multiple');

var Meta = require('./lib/metadata');

require('./examples/point_manual');
require('./examples/multiple_manual');

Meta.dump()

console.log("-- point -------------------------------------");
point_main();
console.log("-- multiple ----------------------------------");
multiple_main();
