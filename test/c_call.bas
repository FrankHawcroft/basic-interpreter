rem Test the traditional CALL syntax for subprograms.

sub p(q, r)
	q = r
endsub

x = 1
y = 2
call p(x, y)
if x <> 2 or y <> 2
	print "Failed: CALL, parameters not updated as expected", x, y
	error 234
else
	print "Old-style CALL succeeded"
endif
