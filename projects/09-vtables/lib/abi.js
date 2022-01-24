var Ptr = require("./pointer");
var Meta = require("./metadata");

/**
 * obj.setMember("x", 2) is equivalent to the C++ code "obj.x = 2"
 */
Ptr.prototype.setMember = function(member, x) {
    var ptr = this.autocast(member);
// BEGIN Ptr.setMember (DO NOT DELETE THIS LINE)
// ANSWER HERE
// END Ptr.setMember (DO NOT DELETE THIS LINE)
}

/**
 * obj.getMember("x") is equivalent to the C++ code "obj.x"
 */
Ptr.prototype.getMember = function(member) {
    var ptr = this.autocast(member);
// BEGIN Ptr.getMember (DO NOT DELETE THIS LINE)
// ANSWER HERE
// END Ptr.getMember (DO NOT DELETE THIS LINE)
}

/**
 * obj.vcall("f", [arg1, arg2]) is equivalent to the C++ code
 * "obj->f(arg1, arg2)" where f is a virtual function.
 */
Ptr.prototype.vcall = function(method, args) {
    var ptr = this.autocast(method);
    if (!ptr) {
        throw new Error("Method " + method + " not implemented in class or any of its parents");
    }
    var f, shifted_ptr;
// BEGIN Ptr.vcall (DO NOT DELETE THIS CALL)
// ANSWER HERE (place the function in the variable f
// and the appropriately adjusted pointer in shifted_ptr)
// END Ptr.vcall (DO NOT DELETE THIS CALL)
    args.unshift(shifted_ptr); // push thiz onto arguments list
    return f.apply(undefined, args);
}

/**
 * obj.call("f", [arg1, arg2]) is equivalent to the C++ code
 * "obj->f(arg1, arg2)" where f is a non-virtual function.
 */
Ptr.prototype.call = function(method, args) {
    var ptr = this.autocast(method);
    var f;
// BEGIN Ptr.call (DO NOT DELETE THIS CALL)
// ANSWER HERE (place the function in the variable f)
// END Ptr.call (DO NOT DELETE THIS CALL)
    args.unshift(ptr);
    return f.apply(undefined, args);
}

/**
 * Ptr.new_("A", [arg1, arg2]) is equivalent to the C++ code
 * "new A(arg1, arg2)".
 *
 * NB: Since we haven't implemented overloading, parent class
 * constructor calls always call with an empty argument list.
 * You can "overload" by having the function implementations
 * you pass in test for the types/number of arguments, if you
 * like.
 */
Ptr.new_ = function(name, args) {
    var array = new Array(Meta.sizeof(name));
    var template = Meta.getTemplate(name);
    if (!template) {
        throw new Error("No template for " + name + " available");
    }
    template.forEach(function(layout) {
        array[layout.offset] = layout.vtable;
    });
    var ptr = Ptr.Ptr(array, 0, name);
    args.unshift(ptr);

    // Run the constructors
    function runCtors(ptr) {
        // Parents get run first, in order of parent declaration
        Meta.getParents(ptr.type).forEach(function(p) {
            runCtors(ptr.cast(p));
        });

        // Now run the constructor at this type
        var ctor = Meta.getGlobal(ptr.type, ptr.type);
        if (ctor) {
            if (ptr.type == name) {
                ctor.apply(undefined, args);
            } else {
                // Don't call with arguments, it's WRONG WRONG WRONG
                ctor(ptr);
            }
        }
    }
    runCtors(ptr);

    return ptr;
}

/**
 * obj.cast("B") is equivalent to the C++ code "(B*)obj".
 */
Ptr.prototype.cast = function(ty_to) {
    if (ty_to in Meta.getCasts(this.type)) {
        return this.add(Meta.getCast(this.type, ty_to), ty_to);
    }
    var parents = Meta.getParents(this.type);
    for (var i = 0; i < parents.length; i++) {
        var r = this.cast(parents[i]).cast(ty_to);
        if (r) return r;
    }
    return false;
}

/**
 * In C++, the compiler will automatically figures out where identifiers
 * come from; e.g. if you access b->a, where b is class B which inherits
 * from A which defines field a, then the compiler will know it's A::a
 * you're referring to.  We don't have any step which does that in this
 * implementation, so we simulate it using an autocast function, which
 * takes a pointer and a member you're trying to look up, and casts
 * it to the version of the class which actually has the member.
 * Returns 'false' if we couldn't figure out a cast that works.
 * Importantly, this only looks at STATIC data (this.type), so it's
 * not a runtime cast (except for any delta adjustment.)
 *
 * This works for both virtual functions and data members.  This
 * function does NOT do unsafe downcasts!
 *
 * TODO: make it work for static functions too.
 */
Ptr.prototype.autocast = function(member) {
    if (Meta.hasDataOffset(this.type, member)) {
        return this;
    }
    if (member in Meta.getVirtualOffsets(this.type)) {
        return this;
    }
    if (Meta.hasGlobal(this.type, member)) {
        return this;
    }
    var parents = Meta.getParents(this.type);
    for (var i = 0; i < parents.length; i++) {
        var r = this.cast(parents[i]).autocast(member);
        if (r) return r;
    }
    return false;
}
