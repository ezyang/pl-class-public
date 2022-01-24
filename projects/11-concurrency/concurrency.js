// --------------------------------------------------------------------
// Cooperative concurrency in JavaScript
//
// Continuation passing style can be used to build cooperative
// concurrency between multiple user threads, even in the absence of
// true parallelism (workers) or an event loop (setTimeout).  The
// current thread of execution is explicitly represented as the "current
// continuation"; if we wish to switch to executing a different thread,
// we save the continuation to a thread queue, and then run a different
// continuation.
//
// Note: In this lab, we will mix continuation passing code with regular
// code.  If a function is to be written in CPS, it will take a
// continuation as its argument and will be documented to not return.
// Unlike 'metacc', the naming convention will be that 'k' indicates the
// current continuation.

var assert = require('assert');
var HashMap = require('hashmap');
var Queue = require('Queue');

// Debugging tip: we have inserted a number of debugging console logs
// for you.  They are always prefixed by a ">", so you can filter them out
// using:
//
//      node concurrency.js | grep -v ">"

// --------------------------------------------------------------------
// The basics

// To manage our threads, we have a global thread queue, which is a
// queue of threads (continuations) waiting to be run.  It supports
// two operations:
//
//      var k = queue.dequeue(); // dequeue the next thread, or null if
//                               // there are no more
//      queue.enqueue(k); // enqueue a thread to run later
//
queue = new Queue();

// Implement the following functions:

// 'runNextThread' runs the next thread waiting on the thread queue.  If
// the queue is empty, call halt().  This function does NOT return.
// (Hint: why doesn't runNextThread take a continuation as an argument?)
function runNextThread() {
    console.log("> Ready to run " + queue.getLength() + " thread(s)");
// BEGIN runNextThread

// your code here

// END runNextThread
}

// 'yield' suspends the execution of the current thread and runs
// the next thread waiting on the queue.  This function does NOT
// return.
function yield(k) {
// BEGIN yield

// your code here

// END yield
}

// 'spawn' schedules a new thread to execute the function f.  f takes no
// arguments and does not return.  'spawn' does NOT take a continuation
// as its argument (so it DOES return, although it doesn't return
// anything useful.)
function spawn(f) {
    console.log("> Spawning thread");
// BEGIN spawn

// your code here

// END spawn
}

// 'finishThread' is called when a thread is "done" executing (there
// is nothing else for it to do).  It does NOT return.  (Hint: this
// function is very simple.)
function finishThread() {
    console.log("> Finishing thread");
// BEGIN finishThread

// your code here

// END finishThread
}

// yield and spawn are enough to implement cooperative threading.
// Here are two more utility functions for starting and stopping
// executing threads:

// 'halt' halts the execution of the program, meaning that no more
// threads run.  To aid in debugging, we throw an exception to
// ensure that this function doesn't return.
function halt() {
    console.log("> Halting");
    throw "halt";
}

// 'run' executes the first thread on the thread queue.  (It is
// assumed that the threads are responsible for yielding to
// one another.)  You should not use this, except to kick
// off execution of the thread queue.
function run() {
    try {
        runNextThread();
    } catch (e) {
        if (e == "halt") return;
        throw e;
    }
}

console.log("----------------------------------------------");
console.log("EXAMPLE ONE");
queue = new Queue();
spawn(function() {
    console.log("A1");
    yield(function() {
        console.log("A2");
        finishThread();
    });
});
spawn(function() {
    console.log("B1");
    yield(function() {
        console.log("B2");
        finishThread();
    });
});
run();

// Sample output for EXAMPLE ONE (your output may
// be slightly different):
//
// A1
// B1
// A2
// B2

// --------------------------------------------------------------------
// Monitors

// With an explicit representation of threads, we can implement
// inter-thread synchronization.  Let us implement monitors as a queue
// of threads waiting to be waken up.

// 'monitor' allocates a new monitor and returns it.
function monitor() {
    return new Queue();
}

// Implement the following two functions:

// 'wait' blocks a thread until another thread runs 'notify' on
// the monitor.  This function does not return.
function wait(m, k) {
    console.log("> Waiting on monitor");
// BEGIN wait

// your code here

// END wait
}

// 'notify' wakes up a single waiting thread if there is one
// (to be run after the notifying thread finishes running),
// and does nothing otherwise.  This function does not return.  (Hint:
// 'notify' is responsible for resuming the waiting thread.)
function notify(m, k) {
    console.log("> Waking up thread on monitor");
// BEGIN notify

// your code here

// END notify
}

console.log("----------------------------------------------");
console.log("EXAMPLE TWO");
queue = new Queue();
spawn(function() {
    var m = monitor();
    spawn(function() {
        console.log("Starting to wait");
        wait(m, function() {
            console.log("Done waiting");
            finishThread();
        })
    });
    yield(function() {
        notify(m, function() {
            console.log("Notified waiter!");
            finishThread();
        });
    });
});
run();

// Sample output for EXAMPLE TWO (your output may
// be slightly different):
//
// Starting to wait
// Notified waiter!
// Done waiting

// --------------------------------------------------------------------
// STM (OPTIONAL)

// THIS PART OF THE LAB IS OPTIONAL.
//
// In this section, we'll implement a simplified version of STM.  As
// JavaScript is not parallel, we will not getting any scaling benefits
// from our implementation of STM; however, it will allow us to improve
// liveness by performing atomic transactions on TVars *across* yields.

// We start by defining TVars:

var fresh_id = 0;
function TVar(init) {
    // A unique identifier for the variable, for debugging purposes.
    this.id = fresh_id++;
    // The current value of the transaction variable.
    this.val = init;
    // The set of TWait objects which are waiting on this TVar.
    // This is morally a list of TWait objects, but its represented
    // as a hash map so that we can remove them quickly.
    // The HashMap is a map from TWait to TWait (the key and the
    // value are the same).
    // Ignore this for now.
    this.waiting_threads = new HashMap();
}

// Like many other STM implementations, we will use a *transaction
// log* to keep track of an ongoing transaction, and only actually
// commit changes to the TVars at the end of the transaction.

function TLog() {
    // A HashMap mapping from TVar to TEntry (transaction entry)
    // For docs on HashMap, see https://www.npmjs.com/package/hashmap
    this.log = new HashMap();
}

function TEntry(ref, v) {
    assert(ref instanceof TVar);
    // The TVar which this entry is for (the key for the TEntry
    // in the transaction log.)
    this.ref = ref;
    // The value of the TVar when we first read/wrote this TVar;
    // if the TVar is not this value when we commit, we know there
    // is an inconsistency.
    this.old = ref.val;
    // The value of the TVar as seen by the current transaction.
    // If we write to a TVar in a transaction, we don't update
    // the TVar, but we update the 'val' in the appropriate 'TEntry'.
    this.val = v;
}

// To keep things simple, we'll use TEntrys for both writes *and* reads
// (a read is a "write" which doesn't change the value.)  Thus, a read
// or a write will result in an entry be adding to the TLog
// of the current transaction.

// Implement these functions to read and write to TVars inside
// a transaction.  You may find the methods get(key) and
// set(key, value) on HashMap useful.

// txn.write(tvar, v) takes a TVar and a new value, and writes this TVar
// within the transaction.  This function DOES return.
//
// Hint: if the entry already exists in the transaction log, you
// should update 'val', not 'old'.  You'll get a very strange bug
// if you update 'old'.
TLog.prototype.write = function TLog_write(tvar, v) {
    assert(tvar instanceof TVar);
    console.log("> write _" + tvar.id + " = " + v);
// BEGIN TLog_write

// your code here

// END TLog_write
}

// tvar.read(tvar) takes a TVar and returns the value of the TVar within
// the transaction.  This function DOES return.
TLog.prototype.read = function TLog_read(tvar) {
    assert(tvar instanceof TVar);
    console.log("> read _" + tvar.id);
// BEGIN TLog_read

// your code here

// END TLog_read
}

// Next, we need to implement functions to validate and
// commit the transaction log at the end of a transaction.
// You may find the methods txn.forEach(f) and txn.values()
// useful.

// txn.check() checks that a transaction log is consistent with the
// current values stored in the TVars.  This function DOES return.
TLog.prototype.check = function TLog_check() {
    console.log("> Checking transaction");
// BEGIN TLog_check

// your code here

// END TLog_check
}

// txn.commit() takes a transaction log and commits the changes to the
// actual TVars, so that other threads can see the change.  This
// function DOES return.
TLog.prototype.commit = function TLog_commit() {
    console.log("> Committing transaction");
// BEGIN TLog_commit

// your code here

// END TLog_commit
}

// Finally, 'atomically' takes a function f and a current continuation
// k, and runs f within an atomic transaction (allocating a new
// transaction log to serve for the transaction).  f is a function which
// takes two arguments: a transaction log and a continuation
// (eventually, it will also take a third argument: a retry
// continuation), and doesn't return.  atomically does NOT return.
//
// When f finishes executing, we must check if the transaction log
// is consistent.  If it is, we commit it and call the current
// continuation.  Otherwise, we retry the transaction.
//
// WARNING: there's a plain-English "answer this question" below.
// Don't miss it.
function atomically(f, k) {
// BEGIN atomically

// your code here

// END atomically
}

// Don't worry about these for now.
TVar.prototype.wakeupAll = TVar_wakeupAll;
TLog.prototype.suspend = TLog_suspend;

// In this following example, you will find that the message
// "Balance after withdrawal" is printed twice.  Briefly
// explain WHY this occurs.
//
// BEGIN doubleMsgExplain
// Your explanation here
// END doubleMsgExplain

console.log("----------------------------------------------");
console.log("EXAMPLE THREE");

function UnsafeBankAccount() {
    var balance = new TVar(0);
    this.withdraw = function withdraw(txn, n, k) {
        var curr = txn.read(balance);
        // artificially yield to induce a race
        yield(function() {
            txn.write(balance, curr - n);
            k(txn.read(balance));
        });
    }
    this.deposit = function deposit(txn, n, k) {
        var curr = txn.read(balance);
        // artificially yield to induce a race
        yield(function() {
            txn.write(balance, curr + n);
            k(txn.read(balance));
        });
    }
    this.balance = function(txn, k) {
        k(txn.read(balance));
    }
}

queue = new Queue();
var b = new UnsafeBankAccount();
spawn(function() {
    atomically(function(txn, k) {
        b.deposit(txn, 1, function(i) {
            console.log("Balance after deposit:", i);
            k();
        });
    }, finishThread);
});
spawn(function() {
    atomically(function(txn, k) {
        b.withdraw(txn, 2, function(i) {
            console.log("Balance after withdrawal:", i);
            k();
        });
    }, finishThread);
});
run();
spawn(function() {
    atomically(function(txn, k) {
        b.balance(txn, function (i) {
            console.log("Final balance:", i);
            k();
        });
    }, finishThread);
});
run();

// Sample output for EXAMPLE THREE (your output may
// be slightly different):
//
// Balance after deposit: 1
// Balance after withdrawal: -2
// Balance after withdrawal: -1
// Final balance: -1

console.log("----------------------------------------------");
console.log("EXAMPLE FOUR");

queue = new Queue();
var t1 = new TVar(0);
var t2 = new TVar(1);
spawn(function() {
    atomically(function(txn, k) {
        console.log("t1 = 2");
        txn.write(t1, 2);
        yield(k);
    }, finishThread);
});
spawn(function() {
    atomically(function(txn, k) {
        // This write should not conflict, because it
        // doesn't modify the variable.
        console.log("t2 = 1");
        txn.write(t2, 1);
        yield(k);
    }, finishThread);
});
spawn(function() {
    atomically(function(txn, k) {
        console.log("t2 = 3");
        txn.write(t2, 3);
        yield(k);
    }, finishThread);
});
spawn(function() {
    atomically(function(txn, k) {
        // This read SHOULD conflict, if it sees something
        // inconsistent
        console.log("read t2");
        txn.read(t2);
        yield(k);
    }, finishThread);
});
run();

// Sample output for EXAMPLE FOUR (your output may
// be slightly different):
//
// t1 = 2
// t2 = 1
// t2 = 3
// read t2
// read t2

console.log("----------------------------------------------");
console.log("EXAMPLE FIVE");

queue = new Queue();
var t1 = new TVar(0);
var t2 = new TVar(0);
spawn(function() {
    atomically(function(txn, k) {
        txn.write(t1, 1);
        yield(function() {
            var i = txn.read(t2);
            console.log("read t2", i);
            yield(k);
        });
    }, finishThread);
});
spawn(function() {
    atomically(function(txn, k) {
        txn.write(t2, 1);
        yield(function() {
            var i = txn.read(t1);
            console.log("read t1", i);
            yield(k);
        });
    }, finishThread);
});
run();

// Sample output for EXAMPLE FIVE (your output may
// be slightly different):
//
// read t2 0
// read t1 0
// read t1 1

// --------------------------------------------------------------------
// STM Retry

// Another essential building block of STM is 'retry', which allows
// STM applications to implement "blocking" on TVars until some
// condition is fulfilled.  A simple way to implement retry is as
// follows:  at any point, a transaction may retry.  At this point:
//
//      1. We first check if our transaction log is consistent.
//      If it is inconsistent, it's possible that the retry is
//      spurious: that is, it only arose because we saw an
//      inconsistent view of memory.  (In that case, we should
//      rerun the transaction immediately.)
//
//      2. Otherwise, for every TVar we read/wrote, we register
//      our transaction (TWait) as a member of "waiting_threads".
//      Don't forget that we need to call runNextThread() to
//      continue normal execution of the threads in our program.
//
//      3. When a TVar is modified, it wakes up all of its waiting
//      threads.  These waiting threads remove themselves any
//      other TVars which they were waiting on and schedule
//      themselves to run again. (This subsequent rerun may
//      itself retry, in which case we go back to step (1)).
//
// TWait is a data structure which represents transactions which
// are waiting.
function TWait(tvars, k) {
    // A list of TVars which we are waiting on.  When we
    // are woken up, we need to remove ourselves from the
    // waiting_threads queue of each TVar we are waiting on.
    this.waiting_on = tvars;
    // The continuation that should be executed when we
    // wake up.
    this.k = k;
}

// We will need to make adjustments to your implementation of
// commitTransaction (to wakeup when the value changes) and atomically
// (to pass a retry continuation to f), but first, implement
// the following three utility functions:

// txn.suspend(f) is called when a transaction is retried.  It takes the
// continuation to be run when the transaction is woken up, and creates
// a new TWait record and registers the thread as waiting on every TVar
// that was read/written to in the transaction.  (NB: this doesn't
// actually "suspend" the thread; it just makes sure we can wake it
// up earlier.)  This function RETURNS.
function TLog_suspend(f) {
    var wait = new TWait(this.log.keys(), f);
// BEGIN TLog_suspend

// your code here

// END TLog_suspend
}

// waiter.wakeup() is called when a transaction is woken up (because a
// TVar it depends on changed).  It does two things: it removes TWait
// from each of the TVars that it is waiting on (so we don't get woken
// up multiple times), and spawns a new thread to retry the saved
// continuation.
TWait.prototype.wakeup = function TWait_wakeup() {
// BEGIN TWait_wakeup

// your code here

// END TWait_wakeup
}

// tvar.wakeupAll() is called when a TVar has a modification committed
// to it.  It wakes up each transaction in 'waiting_threads', and
// resets the list of 'waiting_threads' to empty.
function TVar_wakeupAll() {
// BEGIN TVar_wakeupAll

// your code here

// END TVar_wakeupAll
}

// Finally, (1) adjust TLog.commit() so that it wakes up all threads
// which were waiting on the TVars it modifies, and (2) adjust
// atomically() so that it passes a third argument to 'f', which is the
// "retry continuation", which should be called when a thread wants to
// retry. (If you need a reminder what "retry" does, look back to the
// introduction to the "STM Retry" section.)
//
// After you implement these changes, this code should work:

console.log("----------------------------------------------");
console.log("EXAMPLE SIX");

function BankAccount() {
    var balance = new TVar(0);
    this.withdraw = function withdraw(txn, n, k, retry_k) {
        var curr = txn.read(balance);
        if (curr - n < 0) {
            // If there's not enough, retry.
            retry_k();
        } else {
            yield(function() {
                txn.write(balance, curr - n);
                k(txn.read(balance));
            });
        }
    }
    this.deposit = function deposit(txn, n, k) {
        var curr = txn.read(balance);
        // artificially yield to induce a race
        yield(function() {
            txn.write(balance, curr + n);
            k(txn.read(balance));
        });
    }
    this.balance = function(txn, k) {
        k(txn.read(balance));
    }
}
queue = new Queue();
var b = new BankAccount();
spawn(function() {
    atomically(function(txn, k) {
        b.deposit(txn, 1, function(i) {
            console.log("Balance after deposit:", i);
            k();
        });
    }, finishThread);
});
spawn(function() {
    atomically(function(txn, k, retry_k) {
        b.withdraw(txn, 3, function(i) {
            console.log("Balance after withdrawal:", i);
            k();
        }, retry_k);
    }, finishThread);
});
run();
spawn(function() {
    atomically(function(txn, k) {
        b.deposit(txn, 3, function() {
            yield(function() {yield(function() {
                b.withdraw(txn, 2, function(i) {
                    console.log("Balance after deposit and withdrawal", i);
                    k();
                });
            })});
        });
    }, finishThread);
});
run();
spawn(function() {
    atomically(function(txn, k) {
        b.deposit(txn, 1, function(i) {
            console.log("Balance after second deposit", i);
            k();
        });
    }, finishThread);
});
run();

// Sample output for EXAMPLE SIX (your output may
// be slightly different):
//
// Balance after deposit: 1
// Balance after deposit and withdrawal 2
// Balance after second deposit 3
// Balance after withdrawal: 0
