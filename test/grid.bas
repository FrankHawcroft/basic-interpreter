defint a-z

screen 0, "", 640, 512, 5, -1
window 1, "Hi there", 0, 0, 640, 512, 0, "Testing", 2+4+8+16

const cols = 26
const rows = 20

def ycoord(r) as 20 * r + r - 1
def xcoord(c) as 20 * c + c - 1

sub DrawBoard
	for r = 1 to rows
		pen = r mod 2
		y = ycoord(r)
		for c = 1 to cols
			x = xcoord(c)
			FillRect x, y, 20, 20, pen
			pen = not pen
		next
	next
endsub

sub DrawCharacter((c), (r))
	FillRect xcoord(c) + 5, ycoord(r) + 5, 10, 10, 2
endsub

sub FillRect((x), (y), (w), (h), (pen))
	area x,y
	area x + w,y
	area x + w,y + h
	area x,y + h
	colour pen
	areafill
endsub

sub KeyHandler
	shared cc, cr
	
	k$ = inkey
	print k, len(k)
	if len(k) = 4 then print "Codes:", cvi(left(k, 2)), cvi(right(k, 2))
	
	if len(k) = 4 then
		select k
		case mki(76) & mki(-32768) ' up
			if cr > 1 then let cr = cr - 1
		case mki(77) & mki(-32768) ' down
			if cr < rows then let cr = cr + 1
		case mki(78) & mki(-32768) ' right
			if cc < cols then let cc = cc + 1
		case mki(79) & mki(-32768) ' left
			if cc > 1 then let cc = cc - 1
		endselect
		DrawCharacter cc,cr
	endif
	if k = "q" then end
endsub

cc = 1
cr = 1
DrawBoard
DrawCharacter cc,cr

on inkey call KeyHandler | enable inkey
repeat
	sleep
forever
