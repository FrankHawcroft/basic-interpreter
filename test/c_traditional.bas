rem Test some old-fashioned BASIC features.

'' Variable names in NEXT:
for i = 1 to 3
	for j = 1 to 3
		print i * j
	next j
next i
if i < 3 or j < 3 or i > 4 or j > 4 then '' allow some latitude in final values of i and j - programs shouldn't rely on final values
	print "NEXT with variable: FAILED; counter value is wrong", i, j
	error 234
endif

'' Single-line IF:
x% = 1
if x > 0 then let y% = 1 else let y% = 1000
if y = 1 then let y = y + 1
if y <> 2 then
	print "Single-line IF not behaving as expected, y =", y
	error 234
endif
