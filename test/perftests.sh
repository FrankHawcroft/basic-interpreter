#!/bin/sh

[ -f perf_log.txt ] && rm perf_log.txt

for test in t_accounts t_lexer t_lisp t_mm t_xhtml
do
  #[ -f gmon.out ] && rm gmon.out # uncomment to profile using gprof
  echo ">>>>>>" "${test}.bas" >> perf_log.txt
  ../src/nb --buf 30000 "${test}.bas" >> perf_log.txt 2>&1
  #mv gmon.out "gmon_${test}.out" # uncomment to profile using gprof
done

#gprof ../src/nb.exe *.out > profile.txt # uncomment to profile using gprof

egrep ': FAIL|nb: error|Assertion failed|\[Heap\] error|\[Stack\] error' perf_log.txt > /dev/null
if [ "$?" -eq "0" ]; then
	echo "Test batch failed!"
	exit 1
else
	echo "Test batch passed!"
	exit 0
fi
