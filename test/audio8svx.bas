open "i", 1, argv(1)

hdr$ = fread(1, 8)
if len(hdr) < 8 then
	print "Not an 8SVX file - too short!"
	error 255
endif

if left(hdr, 4) <> "FORM" then
	print "Not an IFF FORM!"
	error 255
endif

size& = cvl(right(hdr, 4))
print "Data size:", size

dim fdata@(size)
'for i = 1 to size
'	fdata(i) = fread(1, 1)
'next
n& = 0
while not eof(1) and n < size
	block$ = fread(1, 512)
	for i = 1 to len(block)
		fdata(n + i) = mid(block, i, 1)
	next
	n = n + len(block)
wend
if eof(1) and n < size then
	print "Malformed IFF - too short!"
	error 255
endif

print fdata(4) & fdata(3) & fdata(2) & fdata(1)
if fdata(1) & fdata(2) & fdata(3) & fdata(4) <> "8SVX" then
	print "Not an IFF 8SVX file!"
	error 255
endif
