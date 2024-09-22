screen 1, "screen", 640, 512, 3, -1
window 1, "x", 20, 20, 300, 200, 1, "y", 0

linepat% = 0xCCCC
dim areapat%(2)
areapat%(0) = 0x5555 | areapat%(1) = 0xAAAA  '| areapat%(2) = 0x1111 | areapat%(3) = 0xAAAA

pattern linepat, areapat

area 20,20
area 60,20
area 60,60
area 20,60
PALETTE 1, 0.5, 0.5, 0.5
color 1,2
areafill

circle 100,100,50

paint 100,100,1

color 3
line 20,20,60,60
line 80,80,140,140

color 2
for i = 1 to 100
   pset 20 + random(40), 20 + random(40)
next

'wait 5

'' Changed to event-driven rather than busy-loop style:
sub KeyHandler
    shared ac$
    ac = inkey
endsub

ac$ = ""
on inkey call KeyHandler | enable inkey
while ac = ""
      sleep
wend
goto QuitNoisily

QuitNoisily:
	print ac
	wcls
	wlocate 10,10
	wprint "Exiting ..."
	beep
	wait 1
	windowclose 1
	END
