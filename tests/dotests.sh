#!/bin/bash

echo "Running tests for ../out/gojira in $PWD"
mkdir -p output

failed=0
for thing in `ls src | grep -e ".scm$"`; do
	../out/gojira src/$thing > output/$thing.out;
	if [ ! "`diff src/$thing.out output/$thing.out`" ]; then
		echo "    [ ] Test passed: $thing"
	else
		echo "    [x] Test failed: $thing"
		((failed++))
        echo "        + diff:"
        diff src/$thing.out output/$thing.out | \
            sed 's/.*/        | &/g'
	fi
done

if [[ $failed -gt 0 ]]; then
    echo "$failed test(s) failed."
else
    echo "All tests passed."
fi
