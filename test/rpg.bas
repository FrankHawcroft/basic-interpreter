' It's a role-playing game!

dim map$(10)
for i = 1 to 10 : read map(i) : next
data "+--------------------+"
data "|                    |"
data "|                    |"
data "|                    |"
data "|                    |"
data "|                    |"
data "|                    |"
data "|                    |"
data "|                    |"
data "+--------------------+"

cx% = 10 | cy% = 9

window 1
on inkey goto get_cmd
inkey on

cmd$ = ""
repeat
	put_char cx, cy
	wcls
	for i = 1 to 10 : wprint map(i) : next
	wprint "Command?";
	'sleep
	'line input ,cmd
	if cmd = "n" then move_char cx, cy, 0, -1
	if cmd = "s" then move_char cx, cy, 0, 1
	if cmd = "e" then move_char cx, cy, 1, 0
	if cmd = "w" then move_char cx, cy, -1, 0
	if cmd = "q" then end
forever

sub put_char((x%), (y%))
	shared map(), cx, cy
	letmid map(cy), cx, 1, "*"
end sub

sub move_char(x%, y%, (dx%), (dy%))
	shared map(), cmd
	letmid map(y), x, 1, " "
	if 1 < x + dx and x + dx < 21 then x = x + dx
	if 1 < y + dy and y + dy < 10 then y = y + dy
	put_char x, y
	cmd = ""
end sub

get_cmd: cmd = inkey | print "got a key" | resume next
