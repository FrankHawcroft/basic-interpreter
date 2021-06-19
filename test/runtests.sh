#!/bin/sh

[ -f log_sh.txt ] && rm log_sh.txt
#[ -f all_prof.txt ] && rm all_prof.txt # uncomment to test interpreter's own profiling feature

echo ">>>>>> Platform is MinGW or similar Unix-like shell" > log_sh.txt

# Run the interpreter's self-testing.
echo ">>>>>> invoke startup self-testing only (empty program)" >> log_sh.txt
../src/nb --noprelude --self-test --exec "" >> log_sh.txt 2>&1

# Simple expressions.
echo ">>>>>> simple_exprs.sh (one-liners)" >> log_sh.txt
./simple_exprs.sh >> log_sh.txt 2>&1

for test in `ls c_*.bas`
do
  #[ -f gmon.out ] && rm gmon.out # uncomment if using gprof
  echo ">>>>>>" "${test}" >> log_sh.txt
  BAS_MODULE_PATH=../module export BAS_MODULE_PATH
  ../src/nb -b30k "${test}" >> log_sh.txt 2>&1 # profile "${test}_prof.txt" # uncomment to test interpreter's own profiling feature
  #mv gmon.out "gmon_${test}.out" # uncomment if using gprof
  
  # Uncomment to test interpreter's own profiling feature -
  #echo ">>>>>>" "${test}" >> all_prof.txt 
  #profsorter.pl "${test}_prof.txt" >> all_prof.txt
done

#gprof ../src/nb.exe *.out > gprof_profile.txt # uncomment if using gprof

egrep ': FAIL|nb: error|Assertion failed|\[Heap\] error|\[Stack\] error' log_sh.txt > /dev/null
if [ "$?" -eq "0" ]; then
	echo "Test batch failed!"
	exit 1
else
	echo "Test batch passed!"
	exit 0
fi
