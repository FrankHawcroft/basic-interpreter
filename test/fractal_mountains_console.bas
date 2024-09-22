' Copyright 1987 Compute! Publications, Inc.
' All Rights Reserved.

'' Retrieved from: http://www.atarimagazines.com/compute/issue87/Fractal_Mountains_For_Amiga.php
'' Text-only version - graphics statements removed.

CLEAR ,32767
DEFSNG a - z
DIM Lv (64, 64)
DIM cmap$ (31)
PRINT" Copyright 1987"
PRINT"Compute! Publications, Inc."
PRINT" All Rights Reserved." : PRINT
RANDOMIZE
PRINT "Enter maximum variation (0 - 2) (1 is nice) " ;
max = 0
INPUT , max
const resx% = 320 | const resy% = 200
dim pix$(resx, resy)

FOR a = 0 TO 15
	ac$ = CHR(a*17) 
    cmap$(a) = ac$ + CHR(a * 10.2) + CHR(a * 5.1)
    cmap$(a + 16) = ac$ + ac$ + ac$
NEXT
cmap$(16) = CHR(0) + CHR(64) + CHR(128)
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

def incr(xa, xb) where xa <= xb as 1
def incr(xa, xb) as -1

def m(xa, ya, xb, yb) where xa = xb as 0
def m(xa, ya, xb, yb) as (yb - ya) / (xb - xa)

def charforcolour@(c) where c >= 16 as chr(asc("a") + c) ' asc("a") = 97
def charforcolour@(c) where c < 16 as hex(c)

sub plot(x1, y1, x2, y2, x3, y3, c)
	shared pix()
	
	for x = x1 to x2 step incr(x1, x2)
		pix(x, m(x1, y1, x2, y2) + y1) = charforcolour(c)
	next
	
	for x = x2 to x3 step incr(x2, x3)
		pix(x, m(x2, y2, x3, y3) + y2) = charforcolour(c)
	next
	
	for x = x3 to x1 step incr(x3, x1)
		pix(x, m(x3, y3, x1, y1) + y3) = charforcolour(c)
	next
end sub

snowLine = maxLv - maxLv/4
drawmount: 
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
		LvS = Lv(x, y) + Lv(x, y) + Lv(x, y)
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
		
		plot rx, ry, rx1, ry1, rx2, ry2, shade1
		plot rx, ry, rx2, ry2, rx4, ry4, shade2
		plot rx, ry, rx1, ry1, rx3, ry3, shade3
		plot rx, ry, rx3, ry3, rx4, ry4, shade4
	NEXT x
NEXT y

for y = 0 to resy
	for x = 0 to resx
		print pix(x, y);
	next
	print
next

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
