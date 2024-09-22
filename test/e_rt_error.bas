REM Test a deadend runtime error.

x = 1 'y '' undefined 'y'
x$ = "z" '' type mismatch

dim a(5)
'a(1, 2) = 1

x:

print x '' kind error ('type mismatch')
