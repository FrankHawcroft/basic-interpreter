print "1A. int variable"
n% = 1
if n <> 1 then | print "Failed test!" | error 999 | endif
print "1B. long variable"
m& = 1
if m <> 1 then | print "Failed test!" | error 999 | endif
print "1C. float variable"
x! = 1
if x <> 1 then | print "Failed test!" | error 999 | endif
print "1D. double variable"
y# = 1
if y <> 1 then | print "Failed test!" | error 999 | endif
print "1E. string variable"
s$ = "x"
if s <> "x" then | print "Failed test!" | error 999 | endif
print "1F. char variable"
c@ = "x"
if c <> "x" then | print "Failed test!" | error 999 | endif
print "1G. Boolean variable"
b? = 1
if b <> 1 then | print "Failed test!" | error 999 | endif
print "2A. simple addition"
x = x + 1
if x <> 2 then | print "Failed test!" | error 999 | endif
print "2B. unary negation"
x = -x
if x <> -2 then | print "Failed test!" | error 999 | endif
print "2C. parentheses and type conv on assignment"
y = -(x * 2) - -(x * 2)
if y <> 0 then | print "Failed test!" | error 999 | endif
print "2D. type conv (int, float, double)"
n = 5! * 2#
if n <> 10 then | print "Failed test!" | error 999 | endif
print "2E. unary negation/unary plus/addition/subtraction overloading"
n = +---2-+-2
if n <> 0 then | print "Failed test!" | error 999 | endif
print "2F. polynomial (tests order of precedence of some common operators)"
x! = 2.0
y# = 2 * x ^ 3 + 3 * x ^ 2 + 1.5 * x + 1
if y <> 16 + 12 + 3 + 1 then | print "Failed test!" | error 999 | endif
print "3A. simple string concatenation"
s = "a" + "b"
if s <> "ab" then | print "Failed test!" | error 999 | endif
print "3B. string to char cat"
s = s + c
if s <> "abx" then | print "Failed test!" | error 999 | endif
print "3B(2). string cat with & operator - string and number"
s = "xyz" & 2
if s <> "xyz2" then | print "Failed test!" | error 999 | endif
print "3C. in - found"
b = "x" in "abcxyz"
if b <> 1 then | print "Failed test!" | error 999 | endif
print "3D. in - not found"
b = "x" in "abc"
if b <> 0 then | print "Failed test!" | error 999 | endif
print "4A. Boolean AND"
b = 1 and 0
if b <> 0 then | print "Failed test!" | error 999 | endif
print "4B. Boolean OR, NOT"
b = (not true) or (not false)
if b <> 1 then | print "Failed test!" | error 999 | endif
print "4C. Boolean with comparisons"
b = s = "xyz2" and n = 0
if b <> 1 then | print "Failed test!" | error 999 | endif
print "4D. Boolean with comparisons (2)"
b = (x > 0) or (c = "x")
if b <> 1 then | print "Failed test!" | error 999 | endif
print "4E. More obscure Boolean operators"
b = (1 imp 0) eqv (0 xor 1)
if b <> 0 then | print "Failed test!" | error 999 | endif
print "5A. Simple conditional assignment"
x = 0 | if x = 0 then | x = 1 | endif
if x <> 1 then | print "Failed test!" | error 999 | endif
print "5B. Simple IF ... ELSE"
if x < 0 then | x = 100 | else | x = -100 | endif
if x <> -100 then | print "Failed test!" | error 999 | endif
print "6A. Simple WHILE loop"
n = 1 | m = 10 | while n < m | n = n * 2 | wend
if n <> 16 or m <> 10 then | print "Failed test!" | error 999 | endif
print "7A. Simple REPEAT ... UNTIL"
n = 0 | repeat | n = n + 1 | until n = 10
if n <> 10 then | print "Failed test!" | error 999 | endif
print "8A. Simple FOR loop (new variable!)"
let m = 0 | for i = -5 to 5 | m = 10 * i | next
if m <> 50 then | print "Failed test!" | error 999 | endif
print "8B. Simple FOR loop (step size -1)"
let x = 0 | for i = 10 to 0 step -1 | x = i | next
if x <> 0 then | print "Failed test!" | error 999 | endif
print "9A. Simple SELECT"
m = 3 | select m | case 0 | x = 1 | case 1, 2 | x = 2 | otherwise | x = 3 | endselect
if x <> 3 then | print "Failed test!" | error 999 | endif
print "11A. Simple array creation and assignment to element"
dim a(10) | a(5) = 100
if a(5) <> 100 then | print "Failed test!" | error 999 | endif
print "11B. Use of array as FOR loop index"
for a(1) = 1 to 3 | next
if a(1) < 3 then | print "Failed test!" | error 999 | endif
print "12A. Simple intrinsic function - INSTR"
i = instr(1, "abcd", "bc")
m = instr(4, "abcdx", "dx")
if i <> 2 or m <> 4 then | print "Failed test!" | error 999 | endif
print "12B. Simple intrinsic function - INSTR (not found)"
i = instr(1, "abcdx", "xy")
if i <> 0 then | print "Failed test!" | error 999 | endif
print "13A. Subprogram with local hiding global variable"
n = 1 | sub p | n% = 2 | endsub | p
if n <> 1 then | print "Failed test!" | error 999 | endif
print "13B. Sub with params"
x = 1 | y = 2 | sub q((x), (y)) | x = 10 | y = 100 | endsub | q 5, 6
if x <> 1 or y <> 2 then | print "Failed test!" | error 999 | endif
print "13C. Recursive sub"
sub r((x)) | if x > 0 then | r x - 1 | endif | endsub | x = 3 | r x
if x <> 3 then | print "Failed test!" | error 999 | endif
print "13D. Sub with reference param hiding global variable"
sub ref_param(x#) | x = 5 | endsub | x = 2 | y = 3 | ref_param y
if x <> 2 or y <> 5 then | print "Failed test!" | error 999 | endif
print "Simple tests finished!"
