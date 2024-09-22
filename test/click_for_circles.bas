window 1
on mouse goto Mouser | mouse on
on inkey goto Keyer | inkey on

Loop:

key$ = ""
repeat
	sleep
until key = "q"
END

Mouser:
	circle mouse(1), mouse(2), 10
	resume Loop

Keyer:
	key = inkey
	resume next
