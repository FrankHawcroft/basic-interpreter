'' Test some type conversions.

'' Textual types -- string and char:

a@ = "a"
s$ = a

a@ = s

if a <> s then | print "String <--> char conversion or comparison failed" | error 234 | endif

'' Boolean -- converting between Boolean and integer:

b? = not 1

if b <> 0 then | print "Int <--> boolean conversion or comparison failed" | error 234 | endif

'' Numeric types:

x! = 5
n% = cint(x) \ 2

if n <> 2 then | print "Float <--> int conversion or comparison failed" | error 234 | endif
