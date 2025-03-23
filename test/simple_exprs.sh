# Simple expression evaluation tests that just require the 'print' statement,
# built-in operators and functions. The prelude is not loaded.
# All these tests should print non-zero/true values.

# set cmd "../src/nb --noprelude --exec "

echo "Some numeric literals"
../src/nb --noprelude --exec "print 1"
#mv gmon.out "gmon_print1.out" # uncomment if using gprof

echo "Relational operators"
../src/nb --noprelude --exec "print 1 = 1"
#mv gmon.out "gmon_print11.out" # uncomment if using gprof
../src/nb --noprelude --exec "print 2 > 1"
#mv gmon.out "gmon_print21.out" # uncomment if using gprof
../src/nb --noprelude --exec "print 1 < 2"
#mv gmon.out "gmon_print12.out" # uncomment if using gprof
../src/nb --noprelude --exec "print 2 >= 1"
#mv gmon.out "gmon_print2ge1.out" # uncomment if using gprof
../src/nb --noprelude --exec "print 1 <= 2"
#mv gmon.out "gmon_print1le2.out" # uncomment if using gprof
../src/nb --noprelude --exec "print 1 <> 2"
#mv gmon.out "gmon_print1ne2.out" # uncomment if using gprof

echo "Arithmetic operators"
../src/nb --noprelude --exec "print +1"
#mv gmon.out "gmon_printp1.out" # uncomment if using gprof
../src/nb --noprelude --exec "print -1"
#mv gmon.out "gmon_printn1.out" # uncomment if using gprof
../src/nb --noprelude --exec "print 1 + 1 = 2"
#mv gmon.out "gmon_print112.out" # uncomment if using gprof
../src/nb --noprelude --exec "print 2 - 1 = 1"
#mv gmon.out "gmon_print211.out" # uncomment if using gprof
../src/nb --noprelude --exec "print 2 * 3 = 6"
#mv gmon.out "gmon_print236.out" # uncomment if using gprof
../src/nb --noprelude --exec "print 4 / 2 = 2"
#mv gmon.out "gmon_print422.out" # uncomment if using gprof
../src/nb --noprelude --exec "print 5 / 2 = 2.5"
#mv gmon.out "gmon_print52.out" # uncomment if using gprof
../src/nb --noprelude --exec "print 5 \ 2 = 2"
#mv gmon.out "gmon_print5i2.out" # uncomment if using gprof
../src/nb --noprelude --exec "print 5 mod 2 = 1"
#mv gmon.out "gmon_print5m2.out" # uncomment if using gprof

echo "Bitwise operators"
../src/nb --noprelude --exec "print 1 bitand 1"
#mv gmon.out "gmon_print1ba1.out" # uncomment if using gprof
../src/nb --noprelude --exec "print 0 bitor 1"
#mv gmon.out "gmon_print0bo1.out" # uncomment if using gprof
../src/nb --noprelude --exec "print 0 bitxor 1"
#mv gmon.out "gmon_print0bx1.out" # uncomment if using gprof
../src/nb --noprelude --exec "print bitnot 0"
#mv gmon.out "gmon_printbn1.out" # uncomment if using gprof

echo "Other mathematical operators - TODO"

echo "Parenthesised expressions"
../src/nb --noprelude --exec "print (1) = 1"
#mv gmon.out "gmon_print1e1p.out" # uncomment if using gprof
../src/nb --noprelude --exec "print (1 + 1) = 2"
#mv gmon.out "gmon_print1p12p.out" # uncomment if using gprof
../src/nb --noprelude --exec "print (1 + 1) * 2 = 4"
#mv gmon.out "gmon_print1124p.out" # uncomment if using gprof
../src/nb --noprelude --exec "print (1 + 1) * (2 + 2) = 8"
#mv gmon.out "gmon_print11228p.out" # uncomment if using gprof
../src/nb --noprelude --exec "print 1 + ((1 * 2) + 2) = 5"
#mv gmon.out "gmon_print11225p.out" # uncomment if using gprof

echo "Built-in functions"
../src/nb --noprelude --exec "print sin(0) = 0"
#mv gmon.out "gmon_printsin.out" # uncomment if using gprof
../src/nb --noprelude --exec "print cos(0) = 1"
#mv gmon.out "gmon_printcos.out" # uncomment if using gprof
../src/nb --noprelude --exec "print tan(0) = 0"
#mv gmon.out "gmon_printtan.out" # uncomment if using gprof

echo "Finished!"
