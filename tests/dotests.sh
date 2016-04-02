#!/bin/bash

echo "Running tests for ../out/gojira in $PWD"
mkdir -p output

tests="`ls src`"

function get_expected_out() {
	cat $1 | grep '^;; => ' | sed 's/;; => //'
}

for module in $tests; do
	failed=0
	echo "  ====> $module"

	for thing in `ls src/$module | grep -e ".scm$"`; do
		prog=src/$module/$thing

		../out/gojira $prog > output/$thing.out;
		if [ ! "`get_expected_out $prog | diff - output/$thing.out`" ]; then
			echo "    [ ] Test passed: $thing"
		else
			echo "    [x] Test failed: $thing"
			((failed++))
		echo "        + diff:"

		#diff src/$module/$thing.out output/$thing.out | \
		get_expected_out $prog | diff - output/$thing.out |
		    sed 's/.*/        | &/g'
		fi
	done
done

if [ $failed -gt 1 ]; then
    echo "$failed tests failed."
elif [ $failed -gt 0 ]; then
    echo "$failed test failed."
else
    echo "All tests passed."
fi
