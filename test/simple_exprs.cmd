@echo off

rem Simple expression evaluation tests that just require the 'print' statement,
rem built-in operators and functions. The prelude is not loaded.
rem All these tests should print non-zero/true values.

setlocal

set intpath=%1
if "%intpath%" == "" (set intpath=..\src\nb.exe)

rem We should need neither the prelude nor all built-ins defined upfront.
rem This speeds up interpreter start up quite a lot.
set opts=-l -n -b128b -s

echo Some numeric literals
%intpath% %opts% --exec "print 1"

echo Relational operators
%intpath% %opts% --exec "print 1 = 1"
%intpath% %opts% --exec "print 2 > 1"
%intpath% %opts% --exec "print 1 < 2"
%intpath% %opts% --exec "print 2 >= 1"
%intpath% %opts% --exec "print 1 <= 2"
%intpath% %opts% --exec "print 1 <> 2"

echo Simple arithmetic operators
%intpath% %opts% --exec "print +1"
%intpath% %opts% --exec "print -1"
%intpath% %opts% --exec "print 1 + 1 = 2"
%intpath% %opts% --exec "print 2 - 1 = 1"
%intpath% %opts% --exec "print 2 * 3 = 6"
%intpath% %opts% --exec "print 4 / 2 = 2"
%intpath% %opts% --exec "print 5 / 2 = 2.5"
%intpath% %opts% --exec "print 5 \ 2 = 2"
%intpath% %opts% --exec "print 5 mod 2 = 1"

echo Bitwise operators
%intpath% %opts% --exec "print 1 bitand 1"
%intpath% %opts% --exec "print 0 bitor 1"
%intpath% %opts% --exec "print 0 bitxor 1"
%intpath% %opts% --exec "print bitnot 0"

echo Other mathematical operators - TODO

echo Parenthesised expressions
%intpath% %opts% --exec "print (1) = 1"
%intpath% %opts% --exec "print (1 + 1) = 2"
%intpath% %opts% --exec "print (1 + 1) * 2 = 4"
%intpath% %opts% --exec "print (1 + 1) * (2 + 2) = 8"
%intpath% %opts% --exec "print 1 + ((1 * 2) + 2) = 5"

echo Built-in functions
%intpath% %opts% --exec "print sin(0) = 0"
%intpath% %opts% --exec "print cos(0) = 1"
%intpath% %opts% --exec "print tan(0) = 0"

echo Finished simple expression tests!

endlocal
