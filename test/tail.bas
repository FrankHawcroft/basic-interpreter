def fact_iter&(n&, res&) where n <= 0 as res&
def fact_iter&(n&, res&) as fact_iter(n - 1, n * res)

def fact_rec&(n&) where n <= 0 as 1
def fact_rec&(n&) as n * fact_rec(n - 1)

for i = 1 to 10
	print i, fact_iter(i, 1), fact_rec(i), fact_iter(i, 1) = fact_rec(i)
next

'' STRING should definitely be defined tail-recursively in prelude ...

print string(10, "x")
s$ = string(1000, "x") '' will generally overflow stack if tail call not detected
print len(s)
