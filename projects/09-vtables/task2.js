#!/usr/bin/env nodejs

require('./lib/abi')

var main = require('./examples/null');
require('./examples/null_manual');

main.forEach(function (f) {
    try {
        console.log(f);
        f();
    } catch (e) {
        console.log(e.stack);
    }
});
