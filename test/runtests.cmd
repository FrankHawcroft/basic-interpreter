@echo off

setlocal

rem Module path is set locally - comment out to use the environment variable -
set BAS_MODULE_PATH=..\module

set intpath=..\src\nb
rem C:\Dev\bas\016A\Debug\Basic.exe

echo Testing %intpath%

if exist log.txt move log.txt log_prev.txt > nul
if exist log_eh.txt move log_eh.txt log_eh_prev.txt > nul

echo ***** Platform is Windows CMD.EXE > log.txt

echo Started at: %time%

rem Run the interpreter's self-testing. This only works in a DEBUG build.
echo ***** invoke startup self-testing only (empty program) >> log.txt
%intpath% --noprelude --self-test --exec "" >> log.txt 2>&1

rem Simple expressions.
echo ***** simple_exprs.cmd (one-liners) >> log.txt
call simple_exprs.cmd %intpath% >> log.txt 2>&1

rem Different command line options.
echo ***** Testing command line options >> log.txt
%intpath% --execute "print 1 + 1" >> log.txt 2>&1
%intpath% -v -e"print 1 + 1" >> log.txt 2>&1
%intpath% -d -e"" >> log.txt 2>&1
%intpath% -o -e"for i = 1 to 3 | print i | next | if i < 3 | error 234 | endif" >> log.txt 2>&1
%intpath% -l -e"for i = 1 to 3 | print i | next | if i < 3 | error 234 | endif" >> log.txt 2>&1
%intpath% -g -e"for i = 1 to 3 | print i | next | if i < 3 | error 234 | endif" >> log.txt 2>&1
%intpath% -s -e"for i = 1 to 3 | print i | next | if i < 3 | error 234 | endif" >> log.txt 2>&1
%intpath% -c -e"for i = 1 to 3 | print i | next | if i < 3 | error 234 | endif" >> log.txt 2>&1
rem %intpath% --file ..\test\one_space.bas >> log.txt 2>&1
rem TODO --heap --profile --prelude --module-path
rem TODO more tricky programs for -o
	
rem More involved tests: 'c_' prefix for 'correctness' tests.
forfiles /m c_*.bas /c "cmd /c echo ***** @file >> log.txt && %intpath% --buf 30k @file >> log.txt 2>&1"

rem Error handling tests: 'e_' prefix.
forfiles /m e_*.bas /c "cmd /c echo ***** @file >> log_eh.txt && %intpath% @file >> log_eh.txt 2>&1

echo Finished at: %time%

findstr /c:": FAIL" /c:"nb: error" /c:"Assertion failed" /c:"[Heap] error" /c:"[Stack] error" log.txt > nul 2>&1
if errorlevel 1 (echo Test batch passed) else echo Test batch failed

if exist log_prev.txt start "Comparison" "C:\Program Files\WinMerge\WinMergeU" log_prev.txt log.txt

endlocal
