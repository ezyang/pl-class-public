var acorn  = require("acorn");
var util   = require("util");
var assert = require("assert");
var fs     = require("fs");
var successful_tests = 0, failed_tests = 0, skipped_tests = 0;

// In this lab, we will revisit the JavaScript interpreter you
// implemented in a previous lab, adding support for control flow
// constructs:
//
//      BlockStatement ::= "{" Statement ";" ... "}"
//      Statement  ::= ...
//                   | BlockStatement
//                   | "return" [Expression]
//                   | "throw" [Expression]
//                   | "try" BlockStatement
//                     ["catch" CatchClause]
//                     ["finally" BlockStatement]
//      CatchClause ::= "catch" Identifier BlockStatement
//
// We are going to implement these features by converting our
// interpreter to *continuation passing style.* There is a simple rule
// in continuation passing style:
//
//      NEVER EVER EVER USE THE RETURN KEYWORD

// Before we get started, here are some general debugging tips for this lab.
//
// 1. If you have a bunch of failing tests, consider running only
// a single test and debugging what is going on.  You can do this
// by setting only_run to the name of the test you want to run:

// e.g. only_run = "tests/try/S12.14_A7_T2.js"
only_run = null

// This will make it easier to add debug prints and focus on the
// failing case.
//
// 2. The tests are Python code; you can edit them and see what happens.
// We have put in scope a print function which you can use to print
// out the intermediate state inside tests; call it like a normal
// function.
//
// 3. Your stack traces on errors may look long, but this is because
// they are actually a complete history of every step your program
// took from the very beginning.  They are very useful; if a value
// is not what you expect, try walking up the stack.

// ----------------------------------------------------------------- //

//                          Warmups

// ----------------------------------------------------------------- //

// Let's take a simple example from the old interpreter: the case
// for handling a literal expression:
//
// function evalLiteral(exp, env) {
//      return exp.value;
// }
//
// In CPS, we're not allowed to return the value; instead, we take
// a function 'cc' (the "current continuation") and *call it* with
// the value of the literal.  Fill in the implementation of evalLiteral:

// BEGIN evalLiteral (DO NOT DELETE THIS LINE)
function evalLiteral(exp, env, cc) {
    // exp.value is the literal
    // cc is a function that takes a value as an argument, and
    // never returns (continuations are associated with their
    // own environments, so you don't need to pass env to them)
    // YOUR CODE HERE
    throw new Unsupported("Literal"); // DELETE ME
}
// END evalLiteral (DO NOT DELETE THIS LINE)

// Once you implement this, this test should pass:
test("literal", "23");

// Absent any control flow, program execution proceeds along the
// current continuation (cc).  However, with control flow, we may
// choose to call a different continuation.  In our interpreter,
// we will store the alternate continuations that are "in scope"
// in the object 'kenv', which gets passed to all of our evaluation
// functions: thus evalExpression(expr, env, kenv, cc) takes four
// arguments:
//
//  1. The expression to evaluate (same as before),
//  2. The current environment (same as before),
//  3. The continuation environment,
//  4. The current continuation
//
// For example, "return expr;" evaluates "expr" with the current
// continuation set to the return continuation from kenv
// (kenv["return"]), IGNORING the previous current continuation.
// (a bare "return;" just calls the return continuation immediately.)
// Fill in the implementation of return:

// BEGIN evalReturnStatement (DO NOT DELETE THIS LINE)
function evalReturnStatement(stmt, env, kenv, cc) {
    // stmt.argument is either null or an Expression
    // kenv["return"] is the return continuation
    // YOUR CODE HERE
    throw new Unsupported("ReturnStatement"); // DELETE ME
}
// END evalReturnStatement (DO NOT DELETE THIS LINE)

test("return/1", "return true");
test("return/2", "return true; $ERROR('should short-circuit')");
test("return/3", "return; $ERROR('should short-circuit')", undefined);
test("return/4", "if (1) { return true }; $ERROR('should short-circuit')");

// Sometimes, you will need to chain continuations together in order to
// get all of the results you need.  For example, to evaluate e1 + e2,
// you first evaluate e1, then evaluate e2, and then add the values
// together.  When evaluating e1, the current continuation is "evaluate
// e2, and then add the two results together".  Implement
// BinaryExpression.  (As before, evalBinop takes exp.operator and two
// values and returns the result of performing the binary operation.)

// BEGIN evalBinaryExpression (DO NOT DELETE THIS LINE)
function evalBinaryExpression(exp, env, kenv, cc) {
    // exp.left is an Expression
    // exp.right is an Expression
    // exp.operator is an Operator
    // YOUR CODE HERE
    throw new Unsupported("BinaryExpression"); // DELETE ME
}
// END evalBinaryExpression (DO NOT DELETE THIS LINE)

test("binop", "return (2+4==6)");

// ----------------------------------------------------------------- //

//                          Functions

// ----------------------------------------------------------------- //

// When we enter a function call (apply), we need to update the
// continuation environment with a new return continuation that
// forces us to /immediately/ return from the current function
// (but not the parent!)  Fill in new_kenv with an appropriate
// new continuation environment for function application (DO NOT
// MUTATE kenv). (Hint: what is the current continuation?)

// BEGIN apply (DO NOT DELETE THIS LINE)
function apply(closure, args, kenv, cc) {
    if (typeof closure === "function") {
        cc(closure.apply(null, args));
    } else {
        var code = closure.code;
        var env = extendEnvironment(code.params.map(function(p) {return p.name}),
                                    args,
                                    code.body.body,
                                    closure.environment);
        var new_kenv = {} // YOUR CODE HERE
        throw new Unsupported("CallExpression"); // DELETE ME
        evalStatements(code.body.body, 0, env, new_kenv, cc);
    }
}
// END apply (DO NOT DELETE THIS LINE)

test("apply/1", "function f() {return false; $ERROR('unreachable')}; f(); return true");
test("apply/2", "function f() {return 1}; function g() {return f() * 2}; return g() === 2");

// ----------------------------------------------------------------- //

//                          try...catch

// ----------------------------------------------------------------- //

// Suppose we have a try { ... } catch (e) { ... } block.  Here is how
// we will implement exception in CPS style:
//
//      1. When we enter a try block, we need to define a new
//         "catch" continuation, add it to the kenv (don't mutate
//         kenv; create a new record and copy the fields over),
//         and then start evaluating the body of the try block.
//      2. What does the "catch" continuation do? It creates a new
//         environment with 'e' bound to the exception value, and then
//         runs the body of the catch block in the original continuation
//         environment.
//      3. To throw an exception (throw e), we should evaluate the
//         expression e, and then pass on the result to the "catch"
//         continuation to pass on the exception to the handler.
//
// To make exceptions work across function calls, we'll need to modify
// our implementation of 'apply' so that the "catch" continuation
// is passed through to the function call. (Don't forget to pass a "return"
// continuation through a try block.)
//
// Hint: extendEnvironment([stmt.handler.param.name], [exc], null, env);
// will create a new environment frame binding the exception value 'exc'.

// BEGIN evalThrowStatement (DO NOT DELETE THIS LINE)
function evalThrowStatement(stmt, env, kenv, cc) {
    // stmt.argument is the Expression e in 'throw e'.
    // kenv["catch"] is the catch continuation
    // YOUR CODE HERE
    throw new Unsupported("ThrowStatement"); // DELETE ME
}
// END evalThrowStatement (DO NOT DELETE THIS LINE)

// BEGIN evalTryStatementSimple (DO NOT DELETE THIS LINE)
function evalTryStatement(stmt, env, kenv, cc) {
    // stmt.block is a Statement, representing the ... in try { ... }
    // stmt.handler is a CatchClause; specifically
    //  stmt.handler.param.name is the String exception name, e.g. 'e'
    //  stmt.handler.body is a BlockStatement (use evalStatement)
    // You may assume the following invariants hold... (for now!)
    // kenv["return"] is the return continuation
    // kenv["catch"] is the catch continuation
    if (stmt.finalizer !== null) throw new Unsupported("finalize block");
    if (stmt.handler === null)   throw new Unsupported("missing catch block");

    // YOUR CODE HERE
    throw new Unsupported("TryStatement"); // DELETE ME
}
// END evalTryStatementSimple (DO NOT DELETE THIS LINE)

test("try/simple/1", "try {x=true;} catch(e) {$ERROR('bad');}; return x");
test("try/simple/2", "try {throw true; $ERROR('bad')} catch(e) {return e}; $ERROR('bad2')");
test("try/simple/3", "try {return true} catch(e) {$ERROR('bad')}; $ERROR('bad2')");
test("try/simple/4", "try { try {throw false} catch(e) {throw true}; $ERROR('bad') } catch(e) { return e }");
test("try/fn/1",
        "function f() {throw true}"+
        "try{f(); $ERROR('bad')} catch(e) {return e}; $ERROR('bad2')");
test("try/fn/2",
        "function f() {return}"+
        "try{f(); return true} catch(e) {$ERROR('bad')}; $ERROR('bad2')");
test("try/fn/3",
        "function f() {try{throw false} catch(e) {return 0}}"+
        "try{f(); return true} catch(e) {$ERROR('bad')}; $ERROR('bad2')");
test("try/fn/4",
        "function f() {try{throw false} catch(e) {}}"+
        "try{f(); return true} catch(e) {$ERROR('bad')}; $ERROR('bad2')");

// ----------------------------------------------------------------- //

//                          Finally!

// ----------------------------------------------------------------- //

// Now we are going to add support for the 'finally' clause.
// Intuitively, finally is a block of code that always executes when
// you exit a block.  Let's be more precise:
//
//      1. When we enter a try block with a finally clause, along with
//      installing the catch continuation, we also have to wrap the
//      return and current continuation to run the finalizer before
//      continuing with the original continuation.  (This "installs" the
//      finalizer code so it runs when the body calls back on one of the
//      continuations.)
//
//      2. The catch continuation creates a new environment
//      as before, but before executing its body, it TOO has to
//      modify the return, current, AND catch continuation to
//      run the finalizer before running the original continuation.
//      (Note that if there is no handler, i.e., stmt.handler
//      is null, you should just pass on the exception to the
//      enclosing catch continuation.)
//
//      3. The finalizer runs as usual with the original continuation
//      environment.
//
// To get you started, we've defined a helper function 'wrapFinalizer'
// for you.  This function takes a continuation and returns a new
// continuation that will first run the finalizer before running the
// input continuation.

// BEGIN evalTryStatement (DO NOT DELETE THIS LINE)
// Rename this function to evalTryStatement to use it
function evalTryStatementDELETE_THIS_STRING(stmt, env, kenv, cc) {
    // stmt.finalizer is null or a BlockStatement
    // stmt.handler is null or a CatchClause; specifically
    //  stmt.handler.param.name is the String exception name, e.g. 'e'
    //  stmt.handler.body is a BlockStatement
    // kenv["catch"] is the catch continuation
    // kenv["return"] is the return continuation
    function wrapFinalizer(cc) {
        return function (r) {
            if (stmt.finalizer === null) {
                cc(r);
            } else {
                evalStatement(stmt.finalizer, env, kenv, function() { cc(r) });
            }
        }
    }
    // YOUR CODE HERE
    // DON'T FORGET TO RENAME THIS FUNCTION TO evalTryStatement
    throw new Unsupported("TryStatement"); // DELETE ME
}
// END evalTryStatement (DO NOT DELETE THIS LINE)

test("finalize/1", "var i = 0; try{ try {throw 1} finally {i++} } catch(e) {}; return i === 1");

// This concludes the mandatory part of the lab.  There should be
// 18 skipped tests.  Everything beyond here is optional.

// ----------------------------------------------------------------- //

//                  OPTIONAL: while loops

// ----------------------------------------------------------------- //

// Loops feature short-circuiting operators break and continue.  As
// before, we'll implement these by introducing two new continuations
// to our continuation environment: "break" and "continue".

function evalBreakStatement(stmt, env, kenv, cc) {
    kenv["break"]();
}

function evalContinueStatement(stmt, env, kenv, cc) {
    kenv["continue"]();
}

// Implement evalWhileStatement.  Then, if you like, implement
// do...while loops (DoWhileStatement).
//
// Hint: Decompose a while-loop into two mutually recursive
// continuations: one for evaluating the condition, and one for
// evaluating the body.  This will help you define the continue
// continuation, and it will also help you implement do..while.
//
// Hint: break/continue work across try/catch blocks, so you'll need
// to adjust your code there to handle these continuations as well.

// BEGIN evalWhileStatement (DO NOT DELETE ME)
function evalWhileStatement(stmt, env, kenv, cc) {
    // stmt.type is "WhileStatement" or "DoWhileStatement"
    // stmt.body is a BlockStatement
    // stmt.test is an Expression
    throw new Unsupported(stmt.type);
}
// END evalWhileStatement (DO NOT DELETE ME)

test("while/1", "var i = 0; while (i<5) {i = i+1}; return (i==5);");
test("while/2", "var i = 0, odds = 0; while(i < 10) {i=i+1; if(i % 2 == 0) continue; odds=odds+1}; return (odds == 5)");

// ----------------------------------------------------------------- //

//                  OPTIONAL: And beyond!

// ----------------------------------------------------------------- //

// Continuations are powerful enough to support all of JavaScript's
// control flow features.  Try and implement:
//
//      1. Labeled continue/break statements
//      2. Generators <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Iterators_and_Generators>
//      3. call/cc <https://en.wikipedia.org/wiki/Call-with-current-continuation>
//
// Additionally, our interpreter currently will run out of stack space
// on large programs because it never returns.  Modify it so that
// it uses a trampoline.  <http://raganwald.com/2013/03/28/trampolines-in-javascript.html>
//
// (Sorry, no tests for these tasks!)

// ----------------------------------------------------------------- //

//                The rest of the interpreter

// ----------------------------------------------------------------- //

// You don't need to read this section, but there are plenty of
// examples of CPS'ified code, if you need some inspiration.

function evalStatements(stmts, i, env, kenv, cc) {
    if (i < stmts.length) {
        evalStatement(stmts[i], env, kenv, function() {
            evalStatements(stmts, i+1, env, kenv, cc);
        });
    } else {
        cc();
    }
    __unreachable();
}

function evalStatement(stmt, env, kenv, cc) { switch(stmt.type) {
    case "EmptyStatement":
        cc();
        __unreachable();
    case "ExpressionStatement":
        evalExpression(stmt.expression, env, kenv, cc);
        __unreachable();
    case "BlockStatement":
        var new_env = extendEnvironment([],[],null,env);
        evalStatements(stmt.body, 0, new_env, kenv, cc);
        __unreachable();
    case "FunctionDeclaration":
        // These are hoisted, so it's a no-op
        cc();
        __unreachable();
    case "VariableDeclaration":
        function evalVarDecls(decls, i, env, kenv, cc) {
            if (i < decls.length) {
                if (decls[i].init) {
                    evalExpression(decls[i].init, env, kenv, function(val) {
                        setVariableValue(decls[i].id.name, val, env);
                        evalVarDecls(decls, i+1, env, kenv, cc);
                    });
                } else {
                    evalVarDecls(decls, i+1, env, kenv, cc);
                }
            } else {
                cc();
            }
        }
        evalVarDecls(stmt.declarations, 0, env, kenv, cc);
        __unreachable();
    case "ReturnStatement":
        evalReturnStatement(stmt, env, kenv, cc);
        __unreachable();
    case "IfStatement":
        evalExpression(stmt.test, env, kenv, function(r) {
            if (r) {
                evalStatement(stmt.consequent, env, kenv, cc);
            } else {
                if (stmt.alternate) {
                    evalStatement(stmt.alternate, env, kenv, cc);
                } else {
                    cc();
                }
            }
        });
        __unreachable();
    case "ThrowStatement":
        evalThrowStatement(stmt, env, kenv,cc);
        __unreachable();
    case "TryStatement":
        evalTryStatement(stmt, env, kenv, cc);
        __unreachable();
    case "DoWhileStatement":
    case "WhileStatement":
        evalWhileStatement(stmt, env, kenv, cc);
        __unreachable();
    case "BreakStatement":
        evalBreakStatement(stmt, env, kenv, cc);
        __unreachable();
    case "ContinueStatement":
        evalContinueStatement(stmt, env, kenv, cc);
        __unreachable();
    default:
        throw "Unsupported statement type:\n" + util.inspect(stmt);
}}

function evalExpression(exp, env, kenv, cc) {
    switch (exp.type) {
    case "Literal":
        evalLiteral(exp, env, cc);
        __unreachable();
    case "Identifier":
        lookupVariableValue(exp.name, env, kenv, cc);
        __unreachable();
    case "AssignmentExpression":
        evalExpression(exp.right, env, kenv, function(rhs) {
            if (exp.operator === "=") {
                setVariableValue(exp.left.name, rhs, env);
                cc(rhs);
            } else {
                lookupVariableValue(exp.left.name, env, kenv, function(lhs) {
                    var newval;
                    switch (exp.operator) {
                        case "+=": newval = rhs + lhs;
                                   break;
                        case "*=": newval = rhs * lhs;
                                   break;
                    }
                    setVariableValue(exp.left.name, newval, env);
                    cc(newval);
                });
            }
        });
        __unreachable();
    case "UnaryExpression":
        evalExpression(exp.argument, env, kenv, function(e) {
            switch (exp.operator) {
                case "!": cc(!e); break;
                case "+": cc(+e); break;
                case "-": cc(-e); break;
                // This is slightly inaccurate; typeof is not supposed
                // to error if the variable is not bound.
                case "typeof": cc(typeof e); break;
                default:
                    throw "Unsupported unary operator:\n" + util.inspect(exp);
            }
        });
        __unreachable();
    case "BinaryExpression":
        evalBinaryExpression(exp, env, kenv, cc);
        __unreachable();
    case "UpdateExpression":
        assert.strictEqual(exp.argument.type, "Identifier");
        lookupVariableValue(exp.argument.name, env, kenv, function(before) {
            var after;
            switch(exp.operator) {
                case "++":
                    after = before + 1;
                    break;
                case "--":
                    after = before - 1;
                    break;
            }
            setVariableValue(exp.argument.name, after, env);
            cc(exp.prefix ? after : before);
        });
        __unreachable();
    case "LogicalExpression":
        switch(exp.operator) {
            case "&&":
                evalExpression(exp.left, env, kenv, function(l) {
                    if (l) {
                        evalExpression(exp.right, env, kenv, function(r) {
                            cc(r);
                        });
                    } else {
                        cc(l);
                    }
                });
            case "||":
                evalExpression(exp.left, env, kenv, function(l) {
                    if (l) {
                        cc(l);
                    } else {
                        evalExpression(exp.right, env, kenv, function(r) {
                            cc(r);
                        });
                    }
                });
        }
        __unreachable();
    case "FunctionExpression":
        cc({code: exp, environment: env});
        __unreachable();
    case "CallExpression":
        evalExpression(exp.callee, env, kenv, function(closure) {
            var arg_vals = [];
            function evalArguments(args, i, envs, kenv, cc) {
                if (i < args.length) {
                    evalExpression(args[i], envs, kenv, function(r) {
                        arg_vals[i] = r;
                        evalArguments(args, i+1, envs, kenv, cc);
                    });
                } else {
                    cc(arg_vals);
                }
            }
            evalArguments(exp.arguments, 0, env, kenv, function(args) {
                apply(closure, args, kenv, cc);
            });
        });
        __unreachable();
    default:
        throw "Unsupported expression type:\n" + util.inspect(exp);
}}

// ----------------------------------------------------------------- //

//                          Tests

// ----------------------------------------------------------------- //

testDirectory("tests");
console.log("done: " + successful_tests + " tests passed, " + failed_tests + " tests failed (skipped " + skipped_tests + ")");

function testDirectory(dirname) {
    var files = fs.readdirSync(dirname);
    for (var i = 0; i < files.length; i++) {
        (function(){ // need a closure here
            var filename = dirname + "/" + files[i];
            if (fs.lstatSync(filename).isDirectory()) {
                testDirectory(filename);
            } else {
                var data = fs.readFileSync(filename, "utf8");
                if (filename != "" && filename[0] != ".") {
                    test(filename, data);
                }
            }
        })();
    }
}

// ----------------------------------------------------------------- //

//                          Boring nonsense

// ----------------------------------------------------------------- //

// More grotty stuff.

function run(str, ret, fk, cc) {
    var program = acorn.parse(str, {allowReturnOutsideFunction: true, ecmaVersion: 6});
    // Miscellaneous bindings for the test suite
    var env = {"$ERROR" : fk,
               "assert" : assert,
               "isNaN": isNaN,
               "NaN": NaN,
               "undefined": undefined,
               "Infinity": Infinity,
               "print": console.log,
               "Test262Error": function() {}};
    var hoisted_ids = hoist(program.body, env);
    hoisted_ids.forEach(function(pair) {
        env[pair[0]] = pair[1];
    });
    evalStatements(program.body, 0, env, {"return": ret, "catch": fk}, cc);
}

function lookupVariableValue(id, env, kenv, cc) {
    if (id in env) {
        cc(env[id]);
    } else if ("__parent" in env) {
        lookupVariableValue(id, env["__parent"], kenv, cc);
    } else {
        kenv["catch"]("Unbound variable " + id);
    }
}

function extendEnvironment(params, args, body, env) {
    var frame = {};
    var hoisted = body ? hoist(body, env) : [];
    for (var i = 0; i < params.length; i++) {
        frame[params[i]] = args[i];
    }
    hoisted.forEach(function(pair) {
        var id = pair[0];
        var val = pair[1];
        // don't write it if it's just a variable hoist
        if (val == undefined && id in frame) return;
        frame[id] = val;
    });
    frame["__parent"] = env;
    return frame;
}

function setVariableValue(id, value, env) {
    while ("__parent" in env) {
        if (id in env) {
            env[id] = value;
            return value;
        }
        env = env["__parent"];
    }
    env[id] = value;
    return value;
}

function evalBinop(b, x, y) { switch (b) {
    case "instanceof": return x instanceof y;
    case "==": return x == y;
    case "!=": return x != y;
    case "===": return x === y;
    case "!==": return x !== y;
    case "<": return x < y;
    case "<=": return x <= y;
    case ">": return x > y;
    case ">=": return x >= y;
    case "<<": return x << y;
    case ">>": return x >> y;
    case ">>>": return x >>> y;
    case "+": return x + y;
    case "-": return x - y;
    case "*": return x * y;
    case "/": return x / y;
    case "%": return x % y;
    case "|": return x | y;
    case "^": return x ^ y;
    case "&": return x & y;
    default:  throw "Unsupported binary operator " + b
}}

// This is a simple utility function which grovels through all
// expressions looking for variable declarations.  Given a list of
// statements, it returns a list of identifiers that are to be hoisted
// to the initial environment declaration.
function hoist(stmts, env) {
    var r = [];
    function hoistStatement(stmt) { switch (stmt.type) {
        case "EmptyStatement":
        case "ExpressionStatement":
        case "ReturnStatement":
        case "ContinueStatement":
        case "BreakStatement":
        case "ThrowStatement":
            return;
        case "TryStatement":
            hoistStatement(stmt.block);
            // NB: even if the var declaration is shadowed by the
            // pattern, it still gets hoisted!
            if (stmt.handler) hoistStatement(stmt.handler.body);
            if (stmt.finalizer) hoistStatement(stmt.finalizer);
            return;
        case "IfStatement":
            hoistStatement(stmt.consequent);
            if (stmt.alternate) hoistStatement(stmt.alternate);
            return;
        case "FunctionDeclaration":
            r.push([stmt.id.name, {code: stmt, environment: env}]);
            return;
        case "VariableDeclaration":
            return stmt.declarations.forEach(function(decl) { r.push([decl.id.name, undefined]); });
        case "WhileStatement":
        case "DoWhileStatement":
            return hoistStatement(stmt.body);
        case "BlockStatement":
            return stmt.body.forEach(hoistStatement);
        default:
            throw "Unsupported statement type:\n" + util.inspect(stmt);
    }}
    stmts.forEach(hoistStatement);
    return r;
}

function __unreachable() {
    throw new Error("Impossible! continuation returned")
}

function test(name, str) {
    if (only_run !== null && only_run !== name) return;
    var expected_val = arguments.length > 2 ? arguments[2] : true;
    var signal = {};
    try {
        run(str, function(r) {
            if (r !== expected_val) {
                console.log(name + "\n  did not return " + expected_val);
                failed_tests++;
            } else {
                successful_tests++;
            }
            throw signal;
        }, function(e) {
            console.log(name + "\n  threw uncaught exception: " + e);
            failed_tests++;
            throw signal;
        }, function() {
            // Successfully getting to the end == success
            successful_tests++;
            throw signal;
        });
        console.log(name + "\n  didn't call continuation");
        failed_tests++;
    } catch (e) {
        if (e === signal) {
            return;
        } else {
            if (e instanceof Unsupported) {
                skipped_tests++;
                //console.log(name + " skipped, not implemented " + e.type);
            } else {
                console.log(name + "\n  internal error: " + e);
                console.log(e.stack);
                failed_tests++;
            }
        }
    }
}

function Unsupported(type) {
    this.type = type;
    this.message = "Unsupported statement " + type;
    Error.captureStackTrace(this);
}
util.inherits(Unsupported, Error)

// If you're like me, you'll keep typing eval() when you mean
// evalExpression().  This override should help you out.
function eval() { throw "eval(): You probably did not want to call this function"; }
