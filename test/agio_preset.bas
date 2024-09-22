REM 1.1.2 Demo for PRESET
window 1
a=200
b=400
c=1
loop:
FOR x=a TO b STEP c
PSET x,100
PRESET x-40*c,100
NEXT x
SWAP a,b
c=-c
GOTO loop

