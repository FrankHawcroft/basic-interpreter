'' Naive implementation of the Sieve of Eratosthenes.

const n% = 1000

dim prime?(n)

prime(1) = false
for i = 2 to n
    prime(i) = true
next

for p% = 2 to n
    for factor% = 2 to p - 1
        if p mod factor = 0 then
            prime(p) = false
        endif
    next
next

for i = 1 to n
    if prime(i) then
        print i;
    endif
next
print
'xfree
