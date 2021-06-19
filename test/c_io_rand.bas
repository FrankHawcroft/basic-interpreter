'' Tests writing and reading 'R' mode files.

const testfile$ = "test.dat"

open "r", 1, testfile
pname$ = space(20)
addr$ = space(30)
phone$ = space(16)
field 1, pname, addr, phone

for i% = 1 to 5
	lset pname, "Person" & i
	lset addr, "Address" & i
	rset phone, "0123 4567 890"
	put 1
next

close 1

open "r", 1, testfile
field 1, pname, addr, phone

for i = 1 to lof(1)
	get 1, i - 1 '' TODO why is this not 1-based? Check against AB manual
	if loc(1) <> i - 1 | print "Failed test!" | error 999 | endif
	print left(pname, 7), left(addr, 8), phone
	if left(pname, 7)  <> "Person" & i | print "Failed test!" | error 999 | endif
	if left(addr, 8) <> "Address" & i | print "Failed test!" | error 999 | endif
	if phone <> "   0123 4567 890" | print "Failed test!" | error 999 | endif
next

close 1
kill testfile

print "Random access I/O test completed."
