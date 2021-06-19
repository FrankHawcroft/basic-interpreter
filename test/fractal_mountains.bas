' Copyright 1987 Compute! Publications, Inc.
' All Rights Reserved.

'' Retrieved from http://www.atarimagazines.com/compute/issue87/Fractal_Mountains_For_Amiga.php
'' and modified to run on my BASIC interpreter.
'' Must be run with the --unsafe option if you want to save the picture!

CLEAR ,32767 '' Added ,
DEFSNG a - z
DIM Lv (64, 64)
DIM cmap$ (31)
PRINT" Copyright 1987"
PRINT"Compute! Publications, Inc."
PRINT" All Rights Reserved." : PRINT
RANDOMIZE
PRINT "Enter maximum variation (0 - 2) (1 is nice) " ; 
'' Added variable definitions:
max = 0
INPUT , max '' Added dummy first parameter
fiL$ = ""
PRINT "Enter a filename to save picture under."
INPUT "(Saving at end is optional.) " ; fiL$
PRINT "When image is finished, activate the window and then" '' Added helpful message
PRINT "press 's' to save and exit, or 'q' to exit without saving."
PRINT "*** The interpreter must be run with the --unsafe option to save! ***"
SCREEN 0, "", 320, 200, 5, 0x00011000 '' added display mode ID: NTSC_MONITOR_ID
WINDOW 3, "Mountain", 0, 0, 311, 186, 0, "Mountain", 2+4+8+16
FOR a = 0 TO 15
    PALETTE a, a/15, a/25, a/50
    PALETTE a + 16, a/15, a/15, a/15 : ac$ = CHR$(a*17) 
    '' Renamed a$: can't repeat var names with different types
    cmap$(a) = ac$ + CHR$(a * 10.2) + CHR$(a * 5.1)
    cmap$(a + 16) = ac$ + ac$ + ac$
NEXT
PALETTE 16, 0, .25, .5
cmap$(16) = CHR$(0) + CHR$(64) + CHR$(128)
COLOR 15
maxLv = 0
MakeMount: 
FOR iter = 6 TO 1 STEP -1
    sk = 2 ^ iter
    hL = sk/2
    PRINT "Doing Iteration" ; iter
    Dotops: 
    PRINT "Tops & Bottoms " ;
    FOR y = 0 TO 64 STEP sk
    	FOR x = hL TO 64 STEP sk
			ran = (RND-.5) * max * sk
			oLd = (Lv(x-hL, y) + Lv(x + hL, y)) / 2
			Lv(x, y) = oLd + ran
		NEXT x
    NEXT y
    Dobottoms: 
    PRINT "Sides " ;
    FOR x = 0 TO 64 STEP sk
    	FOR y = hL TO 64 STEP sk
			ran = (RND - .5) * max * sk
			oLd = (Lv(x, y - hL) + Lv(x, y + hL)) / 2
			Lv(x, y) = oLd + ran
		NEXT y
    NEXT x
    Docentres: 
    PRINT "Centers "
    FOR x = hL TO 64 STEP sk
    	FOR y = hL TO 64 STEP sk
			ran = (RND - .5) * max * sk
			oLd1 = (Lv(x + hL, y - hL) + Lv(x-hL, y + hL)) / 2
			oLd2 = (Lv(x - hL, y - hL) + Lv(x+hL, y + hL)) / 2
			oLd = (oLd1 + oLd2)/2
			Lv(x, y) = oLd + ran
			IF Lv(x, y) > maxLv THEN maxLv = Lv(x, y)
		NEXT y
    NEXT x
NEXT iter

snowLine = maxLv - maxLv/4
drawmount: 
'CLS '' should change to WCLS, but doesn't make any difference, currently, as nothing in window
xm = 3 '' value was missing ...
ym = 2
xshift = .5
yp = 70
FOR x = 0 TO 64
    IF Lv(x, 0) < 0 THEN Lv(x, 0) = 0
NEXT
FOR y = 0 TO 63
    IF Lv(0, y) < 0 THEN Lv(0, y) = 0
    FOR x = 0 TO 63
    	IF Lv (x, y) < 0 THEN Lv(x, y) = 0
		LvS = Lv(x, y) + Lv(x, y) + Lv(x, y) '' scalar had same name as array
		LvS = (LvS + Lv(x, y))/4
		a = x : b = y
		rx1 = xm * a + xshift * b
		ry1 = ym * b + yp -Lv(a, b)
		GOSUB getshade
		shade1 = shade
		a = x
		rx2 = xm * a + xshift * b
		ry2 = ym * b + yp - Lv(a, b)
		GOSUB getshade 
		shade2 = shade
		a = x : b = y
		rx3 = xm * a + xshift * b
		ry3 = ym * b + yp -Lv(a, b)
		GOSUB getshade 
		shade3 = shade
		a = x
		rx4 = xm * a + xshift * b
		ry4 = ym * b + yp -Lv(a, b)
		GOSUB getshade
		shade4 = shade
		a = x + .5 : b = y + .5
		rx = xm * a + xshift * b
		ry = ym * b + yp
		a = x : b = y
		ry = ry - LvS
		AREA rx, ry '' removed ( )
		AREA rx1, ry1
		AREA rx2, ry2
		COLOR shade1
		AREA FILL
		AREA rx, ry
		AREA rx2, ry2
		AREA rx4, ry4
		COLOR shade2
		AREA FILL
		AREA rx, ry
		AREA rx1, ry1
		AREA rx3, ry3
		COLOR shade3
		AREA FILL
		AREA rx, ry
		AREA rx3, ry3
		AREA rx4, ry4
		COLOR shade4
		AREA FILL
	NEXT x
NEXT y

'' Changed to event-driven rather than busy-loop style:
sub KeyHandler
    shared ac$
    ac = ucase(inkey)
endsub

ac = ""
on inkey call KeyHandler : enable inkey
while ac <> "S" and ac <> "Q"
      sleep
wend

''ender: '' removed spaces between label names and : 
''ac$ = INKEY
IF ac$ = "S" and fiL$ <> "" GOTO savepic
''IF ac$ <> "" THEN : GOTO ender : ENDIF

end2:
WINDOW CLOSE 3
SCREEN CLOSE 0
''WINDOW OUTPUT 1 '' no 'BASIC' window for us, running from the CLI
END

getshade: 
	c = x - (b - y)
	d = y + (a - x)
	xc = x + .5
	yc = y + .5
	xrun1 = xc - a
	xrun2 = xc - c
	yrun1 = yc - b
	yrun2 = yc - d
	rise1 = LvS - Lv (a, b)
	rise2 = LvS - Lv (c, d)
	yrise = ABS(rise1 *xrun2 - rise2 *xrun1)
	yrun = ABS(yrun1 * xrun2 - xrun1 *yrun2)
	IF yrun = yrise THEN : yrun = 1 : yrise = 1 : ENDIF '' changed to block IF
	xrise = ABS (rise1 * yrun2 - rise2 * yrun1)
	xrun = ABS (xrun1 * yrun2 - yrun1 * xrun2)
	IF xrun = xrise THEN : xrun = 1 : xrise = 1 : ENDIF
	xrise = xrise / 2
	yrise = yrise / 2
	xshade = 1 -ABS (xrise / (xrun + xrise))
	yshade = 1 -ABS (yrise / (yrun + yrise))
	shade = 14 * xshade *yshade + 1
	IF LvS > snowLine THEN shade = shade + 16
	IF LvS <= 0 THEN shade = 16
RETURN

savepic: 
	rastport& = WINDOW (3, 8) '' added window id
	bitmap& = PEEKL (rastport& + 4)
	topLine% = 60 - INT (maxLv)
	IF topLine < 0 THEN topLine = 0
	topadd% = topLine * 40
	DIM pLane&
	FOR a = 0 TO 4
		pLane& (a) = PEEKL (bitmap& + 8 + cint(a) * 4) + topadd '' added CINT
	NEXT
	bottomLine% = 186
	Lines& = bottomLine - topLine
	OPEN fiL$ FOR OUTPUT AS 1
	ac$ = MKL (Lines * 40 * 5 + 186)
	FPRINT 1, "FORM" ; ac$; "ILBMBMHD" ; MKL (20); '' was PRINT # 1, ...
	FPRINT 1, MKI (320) ; MKI (Lines); MKL (0) ;
	FPRINT 1, CHR$ (5) ; MKI(0) ; CHR$ (0) ;
	FPRINT 1, MKI (0) ; CHR$ (10) ; CHR$ (11) ;
	FPRINT 1, MKI (320); MKI (200) ;
	FPRINT 1, "CMAP" ; MKL (96) ;
	FOR a = 0 TO 31
		FPRINT 1, cmap$ (a) ;
	NEXT
	FPRINT 1, "BODY" ; MKL (Lines * 40 * 5) ;
	FOR a = 1 TO Lines
		FOR p = 0 TO 4
			FOR b = 0 TO 39 STEP 4
				FPRINT 1, MKL (PEEKL(pLane& (p) + cint(b))); '' added CINT
			NEXT b
			POKEL pLane& (p), -1
			PLane& (p) = pLane& (p) + 40
		NEXT p
	NEXT a
	CLOSE
	GOTO end2
