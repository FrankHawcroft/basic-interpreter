'' Test the DATA, READ and RESTORE statements.

x! = 0
s$ = ""
n% = 0

read x, s, n
	
if x <> -5.4321 | error 999 | endif
if s <> "a string" | error 999 | endif
if n <> 10 | error 999 | endif

data -5.4321, "a string", 0xA

restore MoreData

dim sa$(4)
for i = 1 to 4
	read sa(i)
next

if sa(1) <> "w" or sa(2) <> "x" or sa(3) <> "y" or sa(4) <> "z" | error 999 | endif

'' Error case - trigger 'no more data':
'read x

MoreData:

data "w", "x", "y"
data "z"

print "Finished DATA tests."
