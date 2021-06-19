'' Simple array tests.

defint i, j

sub Test((TestName$), (Assertion?))
    print TestName, ": ";
    if Assertion then
       print "Passed!"
    else
       print "FAILED"
    endif
endsub

'' One-dimensional numeric array.

dim a(10)
for i = 1 to 10
	a(i) = i
next
for i = 1 to 10
	Test "Simple array " & i, a(i) = i
next

'' One-dimensional string array.

dim s$(10)
for i = 1 to 10
	s$(i) = string(i, "x")
next
for i = 1 to 10
	Test "String array " & i, s(i) = string(i, "x")
next

'' Two-dimensional numeric array.

dim m(4, 5)
for i = 1 to 4
	for j = 1 to 5
		m(i, j) = i * j
	next
next
for i = 1 to 4
	for j = 1 to 5
		Test "2D array " & i & ":" & j, m(i, j) = i * j
	next
next

'' Array parameter (one-dimensional).

sub p(a$())
	for i = 1 to 3
		Test "Array parameter " & i, a(i) = "abc" & i
		a(i) = "xyz" & i
	next
endsub

for i = 1 to 3
	s(i) = "abc" & i
next
p s
for i = 1 to 3
	Test "Array parameter - post " & i, s(i) = "xyz" & i
next

'' Array parameter (multi-dimensional).
'' Also LBOUND and UBOUND.

sub q(a%())
	for i = lbound(a, 1) to ubound(a, 1)
		for j = lbound(a, 2) to ubound(a, 2)
			a(i, j) = 10 * i + j
		next
	next
endsub

dim md%(2, 3)

q md

for i = 1 to 2
	for j = 1 to 3
		Test "2D array parameter - post " & i & ":" & j, md(i, j) = 10 * i + j
	next
next

'' SHARED array.

dim shared sh%(5)

sub r
	sh(1) = 20
endsub

r

Test "SHARED array", sh(1) = 20

'' Erasing and redimensioning:

erase a, s
dim a(2, 3)
dim s$(4)

a(1, 1) = 5
s(1) = "x"

Test "Redimensioned array access", a(1, 1) = 5 and s(1) = "x"
