'' Conway's Game of Life.

'' Dimensions:

const rows% = 20
const cols% = 20
const generations% = 10

'' Current and next generation state:

dim curr_alive?(rows, cols)
dim next_alive?(rows, cols)

'' The rules of the game:

def new_state?(x%, y%, neighbours%) where neighbours = 3 as 1
def new_state?(x%, y%, neighbours%) where curr_alive(x, y) as neighbours = 2
def new_state?(x%, y%, neighbours%) as 0

sub count_neighbours((x%), (y%), count%)
	shared curr_alive?()
	
	start_x% = x - 1 | if start_x = 0 then let start_x = 1
	end_x% = x + 1 | if end_x = rows + 1 then let end_x = rows
	
	start_y% = y - 1 | if start_y = 0 then let start_y = 1
	end_y% = y + 1 | if end_y = cols + 1 then let end_y = cols
	
	count = 0
	
	for i = start_x to end_x
		for j = start_y to end_y
			if (i <> x or j <> y) and curr_alive(i, j) then let count = count + 1
		next
	next
endsub

'' Display for a cell:

def display@(x%, y%) where curr_alive(x, y) as "*"
def display@(x%, y%) as " "

'' Initialise game state randomly:

randomize
for i = 1 to rows
	for j = 1 to cols
		curr_alive(i, j) = rnd >= .5
	next
next

'' Iterate through the desired number of generations:

gen% = 1
on timer(2) goto Iterate
enable timer

Loop:
	if gen <= generations then sleep
	'xfree
	END

Iterate:
	'' Display the grid:
	
	print "Generation"; gen
	for i = 1 to rows
		for j = 1 to cols
			print display(i, j);
		next
		print
	next
	print
	
	'' Evolve:
	
	neighbours% = 0
	for i = 1 to rows
		for j = 1 to cols
			count_neighbours i, j, neighbours
			next_alive(i, j) = new_state(i, j, neighbours)
		next
	next
	
	'' Replace current state with new:
	
	for i = 1 to rows
		for j = 1 to cols
			curr_alive(i, j) = next_alive(i, j)
		next
	next
	
	gen = gen + 1
	goto Loop ' TODO why doesn't resume work here? Bug in event handling.
