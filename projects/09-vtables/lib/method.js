var adt = require('adt'), data = adt.data, only = adt.only;

var Method = data(function() {
    return {
        Method : {
            name : only(String),
            impl : adt.any // only(Function)
        }
    };
});

module.exports = Method.Method;
