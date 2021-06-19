REM --- Music - Amiga BASIC Sound/Graphics demo --- 20-July-85
DEFINT A-Z 

GOSUB InitSound
GOSUB InitGraphics

MainLoop:
  SOUND RESUME
  RESTORE Song
  GOSUB PlaySong
  GOTO MainLoop

InitGraphics:
  iDraw = 30
  iErase = 0
  ON TIMER(2) GOSUB TimeSlice
  TIMER ON
  RETURN

TimeSlice:
  'See if user closed window with go-away-box
  IF WINDOW(7)=0 THEN RETURN
  FOR linestep = 1 TO 15
    DrawLine iDraw,1
    DrawLine iErase,0
  NEXT linestep
  RETURN
  
SUB DrawLine(iStep, hue) STATIC
  winWidth = WINDOW(2)
  winHeight = WINDOW(3)
  iStep = (iStep + 1) MOD 60
  side! = INT(iStep / 15)
  I!=(iStep MOD 15)/15
  i1!=1-I!
  ON side!+1 GOSUB top,left,bottom,right
  EXIT SUB
top:
  LINE(winWidth*I!,0)-(winWidth,winHeight*I!),hue
  RETURN
left:
  LINE(winWidth,winHeight*I!)-(winWidth*i1!,winHeight),hue
  RETURN
bottom:
  LINE(winWidth*i1!,winHeight)-(0,winHeight*i1!),hue
  RETURN
right:
  LINE(0,winHeight*i1!)-(winWidth*I!,0),hue
  RETURN
END SUB
 
PlaySong:
  REM --- Array VO contains the default octave for a voice
  FOR v=0 TO 3
    READ VO(v)
    VO(v)=12*VO(v) + 3
  NEXT v

PlayLoop:
    SOUND WAIT
    FOR v=0 TO 3
        t#=VT#(v)
        Fi=-1
        READ p$
        IF p$="x" THEN RETURN
        FOR I=1 TO LEN(p$)
            Ci=INSTR(C$,MID$(p$,I,1))
            IF Ci<=8 THEN
                IF Fi>=0 THEN '' FH changed to multi-line IF
					SOUND F#(Fi),t#,volume,v: t#=VT#(v)
				END IF
                IF Ci=8 THEN Fi=0 ELSE Fi=CF(Ci)+VO(v)
            ELSEIF Ci<11 THEN   '# or -
                Fi=Fi+CF(Ci)
            ELSEIF Ci<17 THEN   '1 through 8
                t#=CT#(Ci)
            ELSEIF Ci<19 THEN   '< or >
                VO(v)=VO(v)+CF(Ci)
            ELSE                'ln
                I=I+1
                Ci = INSTR(C$,MID$(p$,I,1))
                VT#(v)=CT#(Ci)
                IF Fi<0 THEN t#=VT#(v)
            END IF
        NEXT I
        IF Fi>=0 THEN SOUND F#(Fi),t#,volume,v
    NEXT v
    SOUND RESUME
    GOTO PlayLoop

InitSound:
    REM --- F#() contains frequencies for notes in the diatonic scale
    REM --- octave 0, note A = F#(12) = 55 Hz
    DIM F#(88)
    Log2of27_5# = LOG(27.5#)/LOG(2#) '' FH renamed variable Log2of27.5
    FOR x=1 TO 88
        F#(x) = 2^(Log2of27_5# + x/12#)
    NEXT x

    REM --- Build fundamental wave form
    REM --- Controls the quality of the sound
    DIM Timbre(255)
    FOR I=0 TO 255
        READ Timbre(I)
    NEXT I

'These DATA statements were created with the following formula
'Reading these is much faster than executing SIN 1024 times
'   K#=2*3.14159265/256
'   FOR I=0 TO 255
'      Timbre(I)=31*(SIN(I*K#)+SIN(2*I*K#)+SIN(3*I*K#)+SIN(4*I*K#))
'   NEXT I
DATA 0, 8, 15, 23, 30, 37, 44, 51, 57, 63, 69, 74, 79, 83, 87, 91
DATA 93, 96, 98, 99, 100, 100, 100, 99, 98, 97, 95, 92, 89, 86, 83, 79
DATA 75, 71, 66, 62, 57, 52, 48, 43, 39, 34, 30, 25, 21, 18, 14, 11
DATA 8, 5, 3, 0,-1,-3,-4,-5,-5,-6,-6,-5,-5,-4,-3,-1
DATA 0, 2, 3, 5, 7, 9, 11, 13, 15, 17, 18, 20, 21, 23, 24, 25
DATA 26, 26, 27, 27, 27, 27, 27, 26, 25, 24, 23, 22, 20, 18, 17, 15
DATA 13, 11, 9, 7, 5, 3, 1,-1,-3,-5,-6,-8,-9,-10,-11,-12
DATA -12,-13,-13,-13,-13,-13,-12,-11,-11,-10,-8,-7,-6,-4,-3,-2
DATA 0, 2, 3, 4, 6, 7, 8, 10, 11, 11, 12, 13, 13, 13, 13, 13
DATA 12, 12, 11, 10, 9, 8, 6, 5, 3, 1,-1,-3,-5,-7,-9,-11
DATA -13,-15,-17,-18,-20,-22,-23,-24,-25,-26,-27,-27,-27,-27,-27,-26
DATA -26,-25,-24,-23,-21,-20,-18,-17,-15,-13,-11,-9,-7,-5,-3,-2
DATA 0, 1, 3, 4, 5, 5, 6, 6, 5, 5, 4, 3, 1, 0,-3,-5
DATA -8,-11,-14,-18,-21,-25,-30,-34,-39,-43,-48,-52,-57,-62,-66,-71
DATA -75,-79,-83,-86,-89,-92,-95,-97,-98,-99,-100,-100,-100,-99,-98,-96
DATA -93,-91,-87,-83,-79,-74,-69,-63,-57,-51,-44,-37,-30,-23,-15,-8

    WAVE 0,Timbre
    WAVE 1,Timbre
    WAVE 2,Timbre
    WAVE 3,Timbre

    volume = 100

    REM --- Array CF maps MML commands to frequency indices
    C$ = "cdefgabp#-123468<>l"
    DIM CF(19)
    FOR I=1 TO 19
        READ CF(I)
    NEXT I
    DATA 0,2,4,5,7,9,11,0,1,-1, 0,0,0,0,0,0, -12,12,0

    REM --- Array CT# maps MML commands to duration values
    DIM CT#(19)
    FOR I=1 TO 18
        READ CT#(I)
    NEXT I
    REM --- p1,p2,p3,p4,p6,p8 correspond to 36.4 ... 4.55 time units
    DATA 0,0,0,0,0,0,0,0,0,0, 36.4,18.2,12.133333,9.1,6.0666667,4.55, 0,0,0
    RETURN


REM --- 1st 4 numbers are the default octave for each voice (0-7)
REM --- ln changes the duration for subsequent notes in this voice.
REM --- l1 = full note, l2 = half note, l4 = quarter notes etc.
REM --- > increments the octave for this voice
REM --- < decrements the octave for this voice
REM --- a through g plays that note.
REM --- They may be followed by # (sharp) or -(flat).
REM --- They may also be followed by a digit which specifies
REM --- the duration for that note.
REM --- p causes a pause, or rest.

Song:
DATA 1,3,3,3
DATA l2g>ge, l2p2de, l2p2l6g3f#g3a, l6p6gab>dcced
DATA <b>e<e, ge<b, b3ab3ge3d, dgf#gd<bgab
DATA ab>c, a>dc, e3f#g3de3<b, >cdedc<babg
DATA df#d, c<a>f#, a3>da3ga3f#, f#gadf#a>c<ba
DATA gec, g<g>e, d3f#g3f#g3a, bgab>dcced
DATA <b>ed, ge<b, b3ab3ge3g, dgf#gd<bgab
DATA cc#d, >ced, a3f#g3e<a3>c, e>dc<bagdgf#
DATA <gp3>g6d3<b6, dp2b3g6, <b3>gb3>dg3d, gb>dgd<bgb>d
DATA g>f#e, d<gg, l2<g1g, l2<b1>c
DATA f#ed, agf#, a1b, d1d
DATA ef#g, gag, bag, c1<b
DATA dp3d6d3d6, f#a3a6>d3d6, al6d3ef#3g, l6adef#aga>c<b
DATA <d>p3d6d3d6, f#3a6f#3d6<a3>d6, a3>c<a3f#d3f#, >c<af#df#a>c<ba
DATA gf#e, dde, g3dg3f#g3a, bgab>dcced
DATA b<b>e, gd<b, b3ag3f#e3g, dgf#gd<bgab
DATA cd<d, l4>c<a>d<b>c<al2, a3gf#3ga3c, e>dc<bagdgf#
DATA g>ge, b>de, <b3>dg3f#g3a, gbab>dcced
DATA <b>e<e, ge<b, b3ab3ge3d, dgf#gd<bgab
DATA ab>c, a>dc, e3f#g3de3<b, >cdedc<babg
DATA df#d, c<a>f#, a3>f#a3ga3f#, f#gadf#a>c<ba
DATA gec, g<g>e, d3f#g3f#g3a, bgab>dcced
DATA <b>ed, ge<b, b3ab3ge3g, dgf#gd<bgab
DATA cc#d, >ced, a3f#g3e<a3>c, e>dc<bagdgf#
DATA <g>f#e, d<gg, l2b1>c, l2g1g
DATA f#ed, agf#, d1d, a1b
DATA ef#g, gag, c1<b, bag
DATA dp3d6d3d6, f#l6a3a>d3d, al6d3ef#3g, l6ddef#aga>c<b
DATA <dp3>d6d3d6, f#3af#3d<a3>d, a3>c<a3f#d3f#, >c<af#df#a>c<ba
DATA gf#e, l2dde, l2b1>c, bgab>dcced
DATA b<b>e, gd<b, d1<b, dgf#gd<bgab
DATA cd<d, l4>c<a>d<b>c<a, a4b8>c8<ba, e>dc<bagdgf#
DATA g>ge, l2b>de, l6g3dg3f#g3a, gbab>dcced
DATA <b>e<e, ge<b, b3ab3ge3d, dgf#gd<bgab
DATA ab>c, a>dc, e3f#g3de3<b, >cdedc<babg
DATA df#d, c<a>f#, a3>da3ga3f#, f#gadf#a>c<ba
DATA gec, g<g>e, d3f#g3f#g3a, bgab>dcced
DATA <b>ed, ge<b, b3ab3ge3g, dgf#gd<bgab
DATA cc#d, >ced, a3f#g3e<a3>c, e>dc<bagdgf#
DATA <gp3>g6f#3e6, dp3g6d3e6, <b3>gb3>dg3<g, gb>dgd<bdb>c#
DATA dc<b, f#dd, l2a1b, d<def#ag#g#ba
DATA a>a4g4f4e4, e<a>a, >c1c, a>c<b>c<aecde
DATA d<b>e, aag#, <bb4>c8d8<b, f>dcd<bg#ef#g#
DATA a>fd, e<a>f#, al6a3g#a3b, a>c<b>ceddfe
DATA cfe, afc, >c3<b>c3<af3a, eag#aec<ab>c
DATA dd#e, df#e, a3g#a3f#<b3>d, fedc<baeag#
DATA <a>ab, c<ag, >l2c1d, a>ceap3l2d
DATA >c<ae, >cag, e1e, l6ecdegfgb-a
DATA fdg, df#g, dd4e8f8d, a>c<b>c<afdef
DATA cec, geg, l6c3<g>c3<ge3d, egfgec<gab-
DATA fdg, fag, c3ef3ab3>d, a>c<b>c<afdef
DATA cp3c6<b3>d6, gp3d6d3d6, c3<g>c3<a>d3<f#, ecdegf#gba
DATA <g>ge, dde, l2b1>c, bgab>dcced
DATA <b>e<e, ge<b, d1d, dgf#gd<bgab
DATA ab>c, a>dc, c<b1, >cdedc<babg
DATA dp3d6d3d6, cl6<a3a>d3d, l6a3c#d3ef#3g, f#def#aga>c<b
DATA <dp3>d6d3d6, f#3af#3d<a3>d, a3>c<a3f#d3f#, >c<af#df#a>c<ba
DATA gf#e, l2dde, l2b1>c, bgab>dcced
DATA b<b>e, gd<b, d1<b, dgf#gd<bgab
DATA cd<d, l4>c<a>d<b>c<a, a4b8>c8<ba, e>dc<bagdgf#
DATA g1g2, l2gp3>g6d3g6, gl6<b3>dg3d, gb>dgd<bgb>a
DATA g1g2, dp3g6e3c6, <b3g>d3b>c2, fd<bgb>ded<a
DATA g1g2, <ap3>d6<b3>e6, c3<ab2b3g, f#a>cd<bgegb
DATA g1g2, <e3a6f#3>a6f#3d6, a2a3f#d3f#, >c<af#df#a>c<ba
DATA g>ge, dde, g3dg3f#g3a, bgab>dcced
DATA <b>e<e, ge<b, b3ab3ge3d, dgf#gd<bgab
DATA ab>c, a>d<c, e3f#g3de3<b, >cdedc<babg
DATA df#d, c<a>f#, a3>da3ga3f#, f#gadf#a>c<ba
DATA gec, g<g>e, d3f#g3f#g3a, bgab>dcced
DATA <b>ed, ge<d, b3ab3ge3g, dgf#gd<bgab
DATA cc#d, d1d2, a3f#g3e<a3>c, e>dc<bagdgf#
DATA <g1g2, p2, <b1b2, g1g2
DATA p1,p1,p1,p1
DATA x
