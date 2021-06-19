'' Simple sequential file I/O test.
'' Tests modes 'O', 'A', and 'I' and printing to files.
'' TODO should test other file functions such as LOF, LOC

const testfile$ = "test.txt"

open testfile for output as 1
fprint 1, "Hello there"
write 1, "A second line", "written using write"
close 1

open "a", 1, testfile
fprint 1, "Further text"
close 1

open "i", 1, testfile

line1$ = ""
flineinput 1, line1
if line1 <> "Hello there" | print "Failed test!" | error 999 | endif

field1$ = ""
field2$ = ""
finput 1, field1, field2
if field1 <> "A second line" or field2 <> "written using write" | print "Failed test!" | error 999 | endif

line3$ = ""
flineinput 1, line3
if line3 <> "Further text" | print "Failed test!" | error 999 | endif

if not eof(1) | print "Failed test! - expected EOF = TRUE" | error 999 | endif

close 1
kill testfile

print "Sequential I/O test completed."
