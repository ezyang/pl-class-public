#!/usr/bin/env nodejs

require('./lib/abi')

var main = require('./examples/diamond');
require('./examples/diamond_manual');
main();
