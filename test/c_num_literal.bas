'' Test various kinds of numeric literals (aka constants).
'' TODO need more of these tests

'' -- Decimal short integer:
i% = 1234
j% = 12345%
k% = 0

if i <> 1234 or j <> 12345 or k <> 0 | print "Failed test!" | error 999 | endif

'' -- Decimal long integer:
l& = 1234567
m& = 12345678&

if l <> 1234567 or m <> 12345678 | print "Failed test!" | error 999 | endif

'' -- Octal:

o% = &10
p& = &o10

if o <> 8 or p <> 8 | print "Failed test!" | error 999 | endif

'' -- Hexadecimal:

g% = 0xA%
h& = &hff

if g <> 10 or h <> 255 | print "Failed test!" | error 999 | endif

'' -- Single-precision floating point:

x! = 0.1
y! = 1.5e-1

if x <> 0.1 or y <> 0.15 | print "Failed test!" | error 999 | endif

'' -- Double-precision floating point:

d# = 10d5

if d <> 1000000 | print "Failed test!" | error 999 | endif

print "Numeric literal tests completed."
