#!/bin/sh

# Simple expression evaluation tests that just require the 'print' statement,
# built-in operators and functions. The prelude is not loaded.
# All these tests should print non-zero/true values.

alias testquickly='../src/nb --noprelude --exec'

echo "Numeric literals"
testquickly "print 1"
testquickly "print 1.0"
testquickly "print -1"

echo "Relational operators"
testquickly "print 1 = 1"
testquickly "print 2 > 1"
testquickly "print 1 < 2"
testquickly "print 2 >= 1"
testquickly "print 1 <= 2"
testquickly "print 1 <> 2"

echo "Arithmetic operators"
testquickly "print +1"
testquickly "print 1 + 1 = 2"
testquickly "print 2 - 1 = 1"
testquickly "print 2 * 3 = 6"
testquickly "print 4 / 2 = 2"
testquickly "print 5 / 2 = 2.5"
testquickly "print 5 \ 2 = 2"
testquickly "print 5 mod 2 = 1"
testquickly "print 1 ^ 1 = 1"

echo "Bitwise logical operators"
testquickly "print 1 bitand 1"
testquickly "print 0 bitor 1"
testquickly "print 0 bitxor 1"
testquickly "print bitnot 0"

echo "Logical operators"
testquickly "print 1 and 1"
testquickly "print 1 or 0"
testquickly "print 1 xor 0"
testquickly "print not 0"
testquickly "print 0 imp 1"
testquickly "print 0 eqv 0"

echo "String operators"
testquickly "print 1 & 0"

echo "Parenthesised expressions"
testquickly "print (1) = 1"
testquickly "print (1 + 1) = 2"
testquickly "print (1 + 1) * 2 = 4"
testquickly "print (1 + 1) * (2 + 2) = 8"
testquickly "print 1 + ((1 * 2) + 2) = 5"

echo "Built-in functions"
testquickly "print sin(0) = 0"
testquickly "print cos(0) = 1"
testquickly "print tan(0) = 0"

echo "Finished!"
