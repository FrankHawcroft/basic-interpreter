print callbyname("rnd")
if sin(0) <> callbyname("sin", 0) | print "Error: callbyname result incorrect for SIN(0)" | error 999 | endif
if callbyname("left", "abc", 1 + 1) <> "ab" | print "Error: callbyname result incorrect for LEFT" | error 999 | endif
