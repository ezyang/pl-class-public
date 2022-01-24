// In this lab, you will use JavaScript's first class functions
// and closures to implement Haskell-style lazy evaluation.
//
// The most convenient way to run this lab is using node.js
// <http://nodejs.org>.  If you work on 'access.cims.nyu.edu',
// this software should be pre-installed.
//
// You can run this file by running the following command:
//
//      node lazy.js
//
// In the beginning, you should get an exception on the enumFrom test.

// ----------------------------------------------------------------- //

//                          Lazy Evaluation

// ----------------------------------------------------------------- //

// There are two basic properties about lazy evaluation.
//
//  1. When an expression is 'delay'ed, it is not evaluated immediately;
//  instead we return a thunk which evaluates the expression when
//  it is 'force'ed (i.e. when we need it.)
//
//  2. Once a delayed expression is forced, it is never evaluated
//  again: the result is cached and returned directly if the thunk
//  forced again.
//
// Here is a simple example of creating a thunk and then forcing it
// twice.  Notice that the expression we pass to delay has to be written
// in a function() { ... } expression, since otherwise JavaScript will
// eagerly evaluate it!

console.log("== TEST: Memoization ==");

var t = delay(function() { console.log("Call me once"); return 2; });
console.log("1st force");
console.log(force(t));
console.log("2nd force");
console.log(force(t));

// A simple implementation which doesn't work is to have 'delay' simply
// return the function it is given, and 'force' call this function.
// This fulfills property (1), but not property (2).  WITHOUT changing
// 'force', fix delay so that the computed expression is only evaluated
// once.  Hint: represent the thunk as a CLOSURE.  If you don't
// remember how closures work, go read:
// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Closures

// BEGIN delay (DO NOT DELETE THIS LINE)
function delay(f) {
    return f;
}
// END delay (DO NOT DELETE THIS LINE)

function force(t) {
    return t();
}

// ----------------------------------------------------------------- //

//                          Streams

// ----------------------------------------------------------------- //

// With delay and force, we can implement infinite streams of
// numbers.  A stream is simply a thunk that, when forced, gives
// you an object containing 'head', the head element of the
// stream, and 'tail', a thunk which can be forced to give
// you the rest of the stream.  Here are two example functions:

// 'repeat' accepts an element n, and creates a stream of that
// element repeated, e.g. [n, n, ...].

function repeat(n) {
    return delay(function() {
        return { head: n,
                 tail: repeat(n) };
    });
}

// 'take' accepts a number n and a stream, and returns a (JavaScript)
// list of the first n elements of the stream.  If you want to see what
// a stream looks like, we suggest you 'take' some elements and print
// them to the console.

function take(n, thunk_xs) {
    var r = [];
    for (var i = 0; i < n; i++) {
        var xs = force(thunk_xs);
        r.push(xs.head);
        thunk_xs = xs.tail;
    }
    return r;
}

// Here's a simple example of these two functions working together.
// You can try varying the number of elements which are taken and
// observe that the list is as long as you want.

console.log("== TEST: repeat ==");
console.log(take(5, repeat(2)));

// ----------------------------------------------------------------- //

//                          Exercises

// ----------------------------------------------------------------- //

// Please implement the following functions.  You should follow the
// naming convention that thunk_xs represents a thunk which must be
// forced before it can be used, whereas xs is an actual object; you
// will probably find it helpful to follow this convention.
// Additionally, the function 'repeat' and others like it returns
// a THUNK (the delayed function, however, should return a real
// object!).

// enumFrom accepts a number n, and creates an stream of the
// numbers [n, n+1, n+2, ...]

// BEGIN enumFrom (DO NOT DELETE THIS LINE)
function enumFrom(n) {
    // your code hre
}
// END enumFrom (DO NOT DELETE THIS LINE)

console.log("== TEST: enumFrom ==");
console.log(take(5, enumFrom(2)));

// map accepts a function f and an stream [x_0, x_1, ....]
// and returns a new stream [f(x_0), f(x_1), ...]

// BEGIN map (DO NOT DELETE THIS LINE)
function map(f, thunk_xs) {
    // your code here
}
// END map (DO NOT DELETE THIS LINE)

console.log("== TEST: map ==");
var xs = map(function(x) {console.log("Multiplying " + x); return x * 2}, enumFrom(4));
console.log("Taking... (this should happen before any Multiplying)");
console.log(take(5, xs));

// zipWith accepts a function f and two streams [x_0, x_1, ...]
// and [y_0, y_1, ...], and returns a new stream
// [f(x_0,y_0), f(x_1,y_1), ...]

// BEGIN zipWith (DO NOT DELETE THIS LINE)
function zipWith(f, thunk_xs, thunk_ys) {
    // your code here
}
// END zipWith (DO NOT DELETE THIS LINE)

console.log("== TEST: zipWith ==");
console.log(take(5, zipWith(function(x,y) {return x > y},
                            enumFrom(8),
                            map(function(x) {return x * x}, enumFrom(1)))));

// ----------------------------------------------------------------- //

//                          Fibonnaci

// ----------------------------------------------------------------- //

// In this section, we'll define enough lazy functions to compute
// the fibonacci sequence using streams.  Here is what we will
// need:
//
// tail
//      Accepts a stream [x_0, x_1, x_2, ...],
//      Returns a new stream [x_1, x_2, ...].
//
//      Equivalent to the Haskell function:
//
//        tail (_:xs) = xs
//
// cons
//      Accepts an element x and a stream [y_0, y_1, ...],
//      Returns new stream [x, y_0, y_1, ... ].
//
//      Equivalent to the Haskell function:
//
//        cons x xs = x:xs
//
// fix
//      Accepts a function f from streams to streams, and
//      feeds the output of f "into" its input.  How can
//      you have the output if you haven't run the function
//      yet?  Well, as long as the access to the output
//      happens in a *thunk*, no problem!
//
//      This function is equivalent to the Haskell function:
//
//        fix f = let r = f r in r
//
//      This function has the curious property that it returns
//      a fixpoint of f: that is, the returned stream 'r'
//      obeys the equality 'f(r) == r'.  (In fact, it computes
//      the "least" fixpoint.  The definition of least fixpoint
//      is somewhat technical, so we will not go into it here.)

// BEGIN fibonnaci (DO NOT DELETE THIS LINE)

// your code here

// END fibonnaci (DO NOT DELETE THIS LINE)

// With these functions, we can define an improved version
// of repeat which is actually a cyclic structure:

function repeatShared(n) {
    return fix(function(thunk_xs) {
        return cons(n, thunk_xs);
    });
}

// And also an efficient definition of the Fibonnaci sequence:

function plus(x,y) { return x + y; }
var thunk_fibs = fix(function(thunk_fibs) {
    return cons(0, cons(1, zipWith(plus, thunk_fibs, tail(thunk_fibs))));
});

console.log("TEST: fibonnaci");
console.log(take(10, repeatShared(0)));
console.log(take(15, thunk_fibs));
