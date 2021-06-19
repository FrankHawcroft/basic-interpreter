'' Minesweeper - console version.

const Rows% = 10 '' TODO make option
const Cols% = 10

const RowName$ = "1234567890ABCDEF"
const ColName$ = "ABCDEFGHIJKLMNOP"

def NameToVal%(nc@, lut$) as instr(1, lut, nc)

def Valid?(r%, c%) as r >= 1 and c >= 1 and r <= Rows and c <= Cols

def ValidNeighbour?(r%, c%, dr%, dc%) as Valid(r + dr, c + dc) and (dr <> 0 or dc <> 0)

dim Mined?(Rows, Cols)
dim Adjacent%(Rows, Cols)
dim Mark@(Rows, Cols)
dim Considered?(Rows, Cols)

sub DrawBoard
	shared Mark@(), Adjacent%()
	
	print '' having this extra print keeps formatting readable if piping stdin from a file
	'' (because BASIC prints the INPUT prompt char even if a non-interactive stream)
	print "  ";
	for i = 1 to Cols 
		print mid(ColName, i, 1);
	next
	print
	for i = 1 to Rows
		print mid(RowName, i, 1); " ";
		for j = 1 to Cols
			select Mark(i, j)
			case "m"
				print "*";
			case "p"
				print "?";
			case "c"
				print "-";
			case " "
				if Adjacent(i, j) > 0
					print mid(str(Adjacent(i, j)), 2, 1);
				else
					print " ";
				endif
			otherwise
				error 255 '' unknown status!
			endselect
		next
		print " "; mid(RowName, i, 1)
	next
	print "  ";
	for i = 1 to Cols 
		print mid(ColName, i, 1);
	next
	print | print
endsub

sub Clean
	shared Considered?()
	
	for i = 1 to Rows
		for j = 1 to Cols
			Considered(i, j) = false
		next
	next
endsub

sub Sweep((r%), (c%), Considered?())
	shared Mined?(), Adjacent%(), Mark@()

	if not Considered(r, c)
		if not Mined(r, c)
			Mark(r, c) = " "
		endif
	
		Considered(r, c) = true
		
		if Adjacent(r, c) = 0
			for dr% = -1 to 1
				for dc% = -1 to 1
					if ValidNeighbour(r, c, dr, dc) ''' dr <> 0 or dc <> 0) and Valid(r + dr, c + dc)
						Sweep r + dr, c + dc, Considered
					endif
				next
			next
		endif
	endif
endsub

sub PrintHelp
	
endsub

'' TODO randomisation
for i = 1 to Rows
	for j = 1 to Cols
		Mined(i, j) = rnd < .2 '' TODO make mine proportion option
		Mark(i, j) = "c"
	next
next

for i = 1 to Rows
	for j = 1 to Cols
		Adjacent(i, j) = 0
		for dr% = -1 to 1
			for dc% = -1 to 1
				if ValidNeighbour(i, j, dr, dc)
					if Mined(i + dr, j + dc)
						Adjacent(i, j) = 1 + Adjacent(i, j)
					endif
				endif
			next
		next
	next
next

cmd$ = ""
r% = 0 | c% = 0
PrintHelp
repeat
	DrawBoard
	lineinput "> ", cmd
	cmd = ucase(cmd)
	if len(cmd) = 3
		c = NameToVal(mid(cmd, 2, 1), ColName)
		r = NameToVal(mid(cmd, 3, 1), RowName)
		''print r, c
		if left(cmd, 1) = "M" '' mark as mine
			if Mark(r, c) <> " " | Mark(r, c) = "m" | endif
		elseif left(cmd, 1) = "P" '' possible mine
			if Mark(r, c) <> " " | Mark(r, c) = "p" | endif
		elseif left(cmd, 1) = "C" '' clear marking
			if Mark(r, c) <> " " | Mark(r, c) = "c" | endif
		elseif left(cmd, 1) = "S" '' sweep mine
			if Mined(r, c)
				print "BOOM!"
				END
			else
				Clean
				Sweep r, c, Considered
			endif
		endif
		'' TODO check for win!
	else
		PrintHelp
	endif
until cmd = "Q"
