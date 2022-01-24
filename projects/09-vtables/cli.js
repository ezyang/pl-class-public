#!/usr/bin/env nodejs

/**
 * Lab notes:
 *
 *  - JavaScript is not staged, but in a language which supports
 *  staging (Scala is the obvious example), you could actually
 *  use an interpreter in this style to build an actual compiler,
 *  where calls to the metadata API are executed at compile-time, while
 *  calls to the pointer ABI are "generated code".
 *  - A handy optimization is to omit the vtable ptr when there
 *  are no virtual functions.  However, you now need a "vtable ptr
 *  offset", since it means the vtable can now be somewhere random
 *  within the object, not always in the first word. (Question for
 *  you, gentle reader: Why?)
 * - Confusion with degree of freedom whether or not the virtual
 *   offsets should be percolated. Not necessary, and we're
 *   inconsistent about it.  At least make point_manual and
 *   multi_manual consistent
 * - Improve diamond inheritance tests
 * - Too much magic! Metadata API is too big
 *
 * - Pedagogically clearer if you talk about what is optimizations
 *   explicitly
 * - Better to put vtable at end; then you can get rid of it if
 *   there are no methods in it
 *
 * - Don't infinite loop if there aren't enough casts from Meta.getCasts
 *
 * - The tests are not robust:
 *      - Check what happens if student code assumes vcall definitions
 *        are in the correct order
 *      - Check what happens if student code doesn't deep copy vtable
 *        (and they fill things in)
 *      - Check if they assume that every vcall is redefined in child
 *
 * - Bad error message when cannot find global method. Should output the
 *   method that was searched for
 * - Diamond: D to C cast doesn't work
 *
 * - Add metadata validation to make sure that names are fully
 *   qualified, e.g., A::x not x
 *
 */

var util = require("util");

function usage() {
  var s = "\
Usage: \n\
    nodejs cli.js EXAMPLE\n\
    e.g. nodejs cli.js point\n\
         nodejs cli.js multiple\n"
  process.stderr.write(s);
  process.exit(1);
}

if (process.argv.length != 3) {
  usage();
}

var main = require('./examples/' + process.argv[2]);

var compile = require('./lib/compiler');
compile();

// var Meta = require('./lib/metadata');
// Meta.dump();

if (util.isArray(main)) {
    main.forEach(function (f) {
        try {
            f();
        } catch (e) {
            console.log(e.stack);
        }
    });
} else {
    main();
}
