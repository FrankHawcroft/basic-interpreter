REM Test a deadend runtime error.

x = y '' undefined 'y'

dim a(5)
'a(1, 2) = 1

x:

print x '' kind error ('type mismatch')
