REM 1.4.3.3 Design im Listing
WINDOW 1, "Design patterns", (0,0)-(617,185),15 '' ( ) and -; flags arg position is different
OPTION BASE 1
a=8
DIM f$ (a)
REM    0123456789ABCDEF
f$(1)="            *** "
f$(2)="           **** "
f$(3)="          * *** "
f$(4)="        *   *** "
f$(5)="      ********* "
f$(6)="    **      *** "
f$(7)="  *****    *****"
f$(8)=""
REM    0123456789ABCDEF
CALL changeformat(f$(),a) '' () after array
CIRCLE (400,140),100 '' ( ) around coords
PAINT STEP(0,0),2,1 '' paint step
AREA (150,160)
AREA (500,100)
AREA (570,170)
AREAFILL 1
MOUSE ON
WHILE INKEY$=""
	IF MOUSE(0) <> 0 THEN
		b=MOUSE(l)
		c=MOUSE(2)
		IF b>O AND b<600 AND c>O AND c<172 THEN
			LINE (b,c)-(b+4,c+4),1,bf '' ( ) and -
		END IF
	END IF
WEND
SUB changeformat (fd$(1),g) STATIC
	DIM fdi% (g) '' var with same name but different type spec
	FOR i=l TO g
		fd$(i)=fd(i)+SPACE$(16)
		FOR j=O TO 3
			h=O
			FOR k=O TO 3
				IF MID$(fd$(i),j*4+k+1,1)<>" " THEN h=h+2^(3-k)
			NEXT k
			fdi%(i)=fdi%(i)+VAL("&h"+HEX$(h*2^(4*(3-j))))
		NEXT j
		PRINT i, HEX$ (fdi% (i) ) , fdi% (i)
	NEXT i
	PATTERN ,fdi%
END SUB
