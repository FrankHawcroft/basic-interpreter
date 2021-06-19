' Text adventure engine.

defstr a-z

'' -------
'' Parsing
'' -------

def starts_with?(s, t) as left(s, len(t)) = t

def initial_lower(s) as lcase(left(s, 1)) & mid2(s, 2)

def non_sentence(s) where right(s, 1) = "." as left(s, len(s) - 1)
def non_sentence(s) as s

def without_article(s) where starts_with(s, "a ") as mid2(s, 3)
def without_article(s) as s

'' ----------
'' Directions
'' ----------

def standard_direction(d) where d = "north" as "n"
def standard_direction(d) where d = "south" as "s"
def standard_direction(d) where d = "east" as "e"
def standard_direction(d) where d = "west" as "w"
def standard_direction(d) where d = "up" as "u"
def standard_direction(d) where d = "down" as "d"
def standard_direction(d) as d

def opposite(d) where d = "n" as "s"
def opposite(d) where d = "s" as "n"
def opposite(d) where d = "e" as "w"
def opposite(d) where d = "w" as "e"
def opposite(d) where d = "u" as "d"
def opposite(d) where d = "d" as "u"

def is_direction?(s) where len(s) = 1 as s in "nsewud"
def is_direction?(s) as standard_direction(s) in "nsewud"

const num_directions% = 6

dim all_directions(num_directions)
dim long_direction_names(num_directions)

for i% = 1 to num_directions | read all_directions(i), long_direction_names(i) | next
data "n", "north"
data "s", "south"
data "e", "east"
data "w", "west"
data "u", "up"
data "d", "down"

'' -------------------------------
'' Game objects - locations, props
'' -------------------------------

const max_obj% = 100

const k_location@ = "L"
const k_prop@ = "P"

dim o_id(max_obj)
dim o_title(max_obj)
dim o_desc(max_obj)
dim o_short_desc(max_obj)
dim o_visits%(max_obj)
dim o_kind@(max_obj)
dim o_at%(max_obj)
dim o_exit(max_obj)

nobj% = 0

'' ---------
'' Inventory
'' ---------

const at_inventory% = -1

const max_inv% = 20

dim inventory%(max_inv)

sub read_objects
	shared o_id(), o_title(), o_desc(), o_short_desc(), o_kind(), nobj
	
	cid% = 1
	repeat
		read o_id(cid), o_title(cid), o_desc(cid), o_short_desc(cid), o_kind(cid)
		
		if o_short_desc(cid) = "" then
			o_short_desc(cid) = o_title(cid)
		endif
		
		cid = cid + 1
	until o_id(cid - 1) = ""
	
	nobj = cid - 2
	
	data "start", "Living room", "This is the living room of your house.", "", "L"
	data "kitchen", "Kitchen", "This is the kitchen of your house. It has an oven, dishwasher, table, chairs, and some cupboards.", "", "L"
	
	data "plate", "Plate", "A china dining plate.", "", "P"
	
	data "", "", "", "", ""
endsub

sub is_at((obj), (locn))
	shared o_at()
	
	o% = 0 | l% = 0
	find_obj obj, o
	find_obj locn, l
	o_at(o) = l
endsub

sub adjacent((locn1), (dir), (locn2))
	shared o_exit()
	
	l1% = 0 | l2% = 0
	find_obj locn1, l1
	find_obj locn2, l2
	o_exit(l1) = o_exit(l1) & dir & ":" & locn2 & ";"
	o_exit(l2) = o_exit(l2) & opposite(dir) & ":" & locn1 & ";"
endsub

sub find_obj((oid), n%)
	shared nobj, o_id()
	n = 0
	for i% = 1 to nobj
		if oid = o_id(i)
			n = i
			exitsub
		endif
	next
endsub

sub describe_current
	shared cloc, o_title(), o_desc(), o_at(), o_exit(), all_directions(), long_direction_names(), nobj
	
	print o_title(cloc)
	print o_desc(cloc)
	
	for i% = 1 to nobj
		if o_at(i) = cloc
			print "There is a "; non_sentence(without_article(initial_lower(o_desc(i)))); " here."
		endif
	next
	
	for i = 1 to ubound(all_directions, 1)
		if instr(1, o_exit(cloc), all_directions(i) & ":")
			print "There is an exit leading "; long_direction_names(i); "."
		endif
	next
endsub

sub write_current
	shared cloc, o_short_desc(), o_visits()
	
	if o_visits(cloc) > 0 then | print o_short_desc(cloc) | else | describe_current | endif
	'print cloc
	
	o_visits(cloc) = o_visits(cloc) + 1
endsub

sub parse((t), verb, dobj, iobj)
	t = lcase(t)
	
	verb = "" | dobj = "" | iobj = ""
	
	if starts_with(t, "inventory") or t = "i"
		verb = "inventory"
	endif
	
	if starts_with(t, "quit")
		verb = "quit"
	endif
	
	if starts_with(t, "take") or starts_with(t, "get")
		verb = "take"
		dobj = mid2(t, 5 + starts_with(t, "take"))
		'print dobj
	endif
	
	if starts_with(t, "go")
		verb = "go"
		dobj = mid2(t, 3)
	endif
	
	if starts_with(t, "look") or t = "l"
		'' todo looking at particular things ...
		verb = "look"
	endif
	
	if is_direction(t)
		verb = "go"
		dobj = t
	endif
endsub

sub try_move_to((direction))
	shared cloc, o_exit()
	
	direction = standard_direction(direction) + ":"
	idx% = instr(1, o_exit(cloc), direction)
	if idx > 0
		new_loc_id = mid(o_exit(cloc), idx + 2, instr(idx + 1, o_exit(cloc), ";") - idx - 2)
		'print new_loc_id
		find_obj new_loc_id, cloc
	else
		print "You can't go that way."
	endif
endsub

sub try_add_to_inventory((what))
	shared o_at(), o_kind(), inventory(), cloc

	to_take% = 0
	find_obj what, to_take '' todo not actually an oid - match on descriptions!
	'print to_take, o_kind(to_take)
	if to_take = 0
		print "There's no "; what; " here."
	elseif o_at(to_take) = at_inventory
		print "You already have it!"
	elseif o_at(to_take) <> cloc
		print "There's no "; what; " here."
	elseif o_kind(to_take) <> k_prop
		print "That's not something you can take with you."
	else
		for i% = 1 to max_inv
			if inventory(i) = 0
				inventory(i) = to_take
				o_at(to_take) = at_inventory
				print "Taken!"
				exitsub
			endif
		next
		if max_inv = 0
			print "You're not able to carry anything."
		else
			print "You're already carrying too many things to take that as well."
		endif
	endif
endsub

sub list_inventory
	shared inventory(), o_short_desc()
	
	nothing? = true
	for i% = 1 to max_inv
		if inventory(i) <> 0
			if nothing 
				print "You are carrying: "
			endif
			print o_short_desc(inventory(i))
			nothing = false
		endif
	next
	if nothing 
		print "You are carrying nothing."
	endif
endsub

sub act((verb), (dobj), (iobj))
	shared cloc
	
	select verb
	case ""
		'' Do nothing ...
	case "quit"
		cloc = 0
	case "look"
		'' todo looking at specific things
		describe_current
	case "go"
		try_move_to dobj
	case "take"
		try_add_to_inventory dobj
	case "inventory"
		list_inventory
	otherwise
		print "Sorry, I don't understand that."
	endselect
endsub

cloc% = 1

read_objects
'sort_objects

adjacent "start", "e", "kitchen"

is_at "plate", "kitchen"

t = "" 
repeat
	write_current
	lineinput "> ", t
	v = "" | dobj = "" | iobj = ""
	parse t, v, dobj, iobj
	act v, dobj, iobj
until cloc = 0
print "Thanks for playing!"
