' It's a role-playing game!

def Dice%(n%, sides%) where n = 1 as random(sides)
def Dice%(n%, sides%) as Dice(1, sides) + Dice(n - 1, sides)

'' Map:

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

'' Stats:

const max_pc% = 4 '' player characters
const max_npc% = 2 '' non-player characters
const max_ch% = max_pc + max_npc
dim c_name$(max_ch)
dim c_class$(max_ch)
dim c_str%(max_ch)
dim c_con%(max_ch)
dim c_dex%(max_ch)
dim c_int%(max_ch)
dim c_wis%(max_ch)
dim c_cha%(max_ch)
dim c_hp%(max_ch)

gosub make_party
window 1
on inkey goto get_cmd
inkey on

cmd$ = ""
repeat
	put_char cx, cy
	wcls
	for i = 1 to 10 : wprint map(i) : next
	wprint "Command?";
	sleep
	'line input ,cmd
	if cmd = "n" then move_char cx, cy, 0, -1
	if cmd = "s" then move_char cx, cy, 0, 1
	if cmd = "e" then move_char cx, cy, 1, 0
	if cmd = "w" then move_char cx, cy, -1, 0
	if cmd = "p" then gosub show_party
	if cmd = "q" then end
forever

get_cmd: cmd = inkey | print "got a key" | resume next

make_party:
	for i = 1 to max_pc | read c_name(i) | next
	data "Sir Felgar", "Chiasmus", "Bulbo", "Marissa"
	for i = 1 to max_pc | read c_class(i) | next
	data "Warrior", "Bard", "Halfling", "Mage"
	for i = 1 to max_pc
		c_str%(i) = Dice(3, 6)
		c_con%(i) = Dice(3, 6)
		c_dex%(i) = Dice(3, 6)
		c_int%(i) = Dice(3, 6)
		c_wis%(i) = Dice(3, 6)
		c_cha%(i) = Dice(3, 6)
		c_hp(i) = 12 | if c_con(i) > 12 then c_hp(i) = c_hp(i) + 2
	next
	return
	
show_party:
	wcls
	wprint "Key", "Name",, "HP"
	for i = 1 to max_ch
		wprint chr(asc("A") + i - 1), c_name(i),, c_hp(i)
	next
	wprint "Press a key to continue ..."
	sleep
	return

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
