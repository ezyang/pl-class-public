#!/bin/bash
echo "STANDARD TESTS"
for i in simple simple2 simple3 simple4 simple5 simple6 fireplace; do
    echo "Testing $i"
    node cli.js "../tests/$i.js" > "tests/$i.out"
    diff -u "tests/$i.out" "tests_out/$i"
    echo "-----------------------------------------------------------"
done

# comment me to enable the multiple inheritance tests
exit 0

echo "MULTIPLE INHERITANCE TESTS"
for i in aerial DOD acyclic triple vehicle; do
    echo "Testing $i"
    node cli.js "../tests/$i.js" > "tests/$i.out"
    diff -u "tests/$i.out" "tests_out/$i"
    echo "-----------------------------------------------------------"
done
