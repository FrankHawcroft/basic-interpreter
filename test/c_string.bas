'' Test built-in string operators and functions.
'' String functions defined in the prelude are tested by c_prelude_test.bas rather than here.
'' TODO needs to be expanded

s$ = ""

if len(s) <> 0 | print "Failed test!" | error 999 | endif

for i = 1 to 100
	s = s + "a"
next

if len(s) <> 100 | print "Failed test!" | error 999 | endif

if instr(1, s, "b") | print "Failed test!" | error 999 | endif

s = s + "b"

if instr(1, s, "b") <> 101 | print "Failed test!" | error 999 | endif

s = "xyz" + s

if instr(1, s, "xyz") <> 1 | print "Failed test!" | error 999 | endif

print "String tests completed."
