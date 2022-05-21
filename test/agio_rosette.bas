REM 1.1.2 Rosette
pi=3.1415296#
f=0.8 ' .5 'Specifies the relation of height and width
edges=0
INPUT "How many Edges: ",edges
window 1
WCLS
IF edges<> -1 THEN edges=edges+1
REM Predetermined Values
radius=100 'Radius of Maximum Circle
inside=3 'Count of "inside" lines
outside=3 'Count of "outside" lines
FOR t=-inside/10 TO outside/10 STEP .1
	FOR angle=0 TO 2*pi STEP .01
		x=radius*COS(angle)+t*radius*COS(angle*edges)
		y=radius*SIN(angle) +t*radius*SIN (angle*edges)
		PSET 200+x,y*f+200
	NEXT angle
NEXT t
'wait 10
'xfree
