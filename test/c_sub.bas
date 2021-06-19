'' Test subprograms and variable scope.

sub Empty
endsub

sub RefParam(x%)
	x = 2
endsub

sub ValParam((x%))
	x = 3
endsub

sub ArrayParam(a%())
	a(1) = 5
endsub

sub VariousParams(x%, s$(), (y#))
	x = 5
	s(3) = "x"
	s(4) = "y"
	y = 10
endsub

sub WithSharedVariable(d%)
	shared a%()

	d = 4
	a(1) = 10
endsub

x% = 1
dim a%(10)
dim t$(10)

Empty

RefParam x
if x <> 2 | print "Failed test!" | error 999 | endif

ValParam x
if x <> 2 | print "Failed test!" | error 999 | endif

ArrayParam a
if a(1) <> 5 | print "Failed test!" | error 999 | endif

VariousParams x, t, 5.0
if x <> 5 or t(3) <> "x" or t(4) <> "y" | print "Failed test!" | error 999 | endif

WithSharedVariable x
if x <> 4 or a(1) <> 10 | print "Failed test!" | error 999 | endif

print "Subprogram tests completed."
