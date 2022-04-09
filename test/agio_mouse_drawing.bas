REM 1.1.1 Drawing with the Mouse
PRINT "Now You can draw with the Mouse"
window 1
WHILE INKEY$=""
	IF MOUSE(0)<>0 THEN
		x=MOUSE(1)
		y=MOUSE(2)
		PSET x,y
	END IF
WEND
