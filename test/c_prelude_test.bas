' Test use of functions defined in prelude.bas.

sub Test((TestName$), (Assertion?))
    print TestName, ": ";
    if Assertion then
       print "Passed!"
    else
       print "FAILED"
    endif
endsub

const MaxDelta! = 0.0001
def ApproxEqual?(x!, y!) as abs(x - y) <= MaxDelta

' The order of these tests follows the order of functions and statements in the prelude.

Test "TRUE and FALSE", true = 1 and false = 0 and true = (not false)

Test "ABS", abs(-100) = 100 and abs(0) = 0 and abs(100) = 100

Test "EXP", ApproxEqual(exp(1), 2.71828) and exp(0) = 1 '' no longer in prelude! TODO create math tests

Test "SGN", sgn(100) = 1 and sgn(0) = 0 and sgn(-100) = -1

Test "FIX", fix(0.7) = 0 and fix(-1.2) = -1

Test "INT", int(0.5) = 0 and int(-0.5) = -1 and int(100.3) = 100

Test "INSTR2", instr2("abxyc", "xy") = 3 ' and instr2("abcx", "xy") = 0 ' instr(1, "abxyc", "xy") = 3 ' instr(1, "abxyc", "xy") = 3 ' 

Test "MID2", mid2("xyz", 2) = "yz" and mid2("xyz", 3) = "z"

Test "LEFT", left("", 1) = "" and left("hello", 0) = "" _
     and left("hello", 1) = "h" and left("hello", 2) = "he" _ 
     and left("hello", 6) = "hello"

Test "RIGHT", right("", 1) = "" and right("hello", 0) = "" _
      and right("hello", 1) = "o" and right("hello", 2) = "lo" _
      and right("hello", 6) = "hello"

Test "STRING", string(0, "a") = "" and string(1, "x") = "x" _
     and string(5, "x") = "xxxxx"

Test "SPACE", space(0) = "" and space(4) = "    "

Test "UCASE", ucase("") = "" and ucase("abc") = "ABC" and ucase("_x_3") = "_X_3" and ucase("12") = "12"

Test "LCASE", lcase("") = "" and lcase("ABC") = "abc" and lcase("_C_3") = "_c_3" and lcase("12") = "12"

'' Replace has been removed to extprelude
'Test "REPLACE", replace("wxyz", "xy", "a") = "waz" _
'     and replace("abcde", "a", "XXXX") = "XXXXbcde" _
'	 and replace("abcdecde", "cde", "fg") = "abfgfg" _
'     and replace("wxyz", "", "a") = "wxyz" _
'     and replace("", "abc", "x") = "" _
'     and replace("abc", "bc", "") = "a"
	 
fld$ = "HI THERE  "
dat$ = "NEW!"
lset fld = dat
Test "LSET", fld = "NEW!      "

fld$ = "HI THERE  "
dat$ = "NEW!"
rset fld = dat
Test "RSET", fld = "      NEW!"

s$ = "Original string"
t$ = "XXX"
letmid s, 3, 2, t
letmid s, 14, 10, t
Test "LETMID", s = "OrXXinal striXX"

Test "CHR and ASC", chr(asc("a")) = "a"

Test "HEX", hex(0) = "0" and hex(10) = "A" and hex(0x100) = "100" _
     and hex(0x7FFF) = "7FFF" and hex(100000) = "186A0" _
     and hex(-32768) = "FFFF8000" and hex(-1) = "FFFFFFFF"

Test "OCT", oct(0) = "0" and oct(10) = "12" and oct(24) = "30"

Test "CSTR", cstr(0) = "0" and cstr(123.45) = "123.45" and cstr(-123.45) = "-123.45"

Test "CDBL", cdbl(10) = 10.0 and cdbl(-10) = -10.0

Test "CSNG", csng(10) = 10.0 and csng(-10) = -10.0

Test "CLNG", clng(0.5) = 1 and clng(-100.3) = -100

Test "CINT", cint(0.5) = 1 and cint(-100.3) = -100

'XFREE

'' Error cases - uncomment to check handling of error detection in prelude.
'setmid s, -1, -1, ""

