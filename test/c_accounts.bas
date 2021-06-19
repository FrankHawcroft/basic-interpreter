'' This program is to help reconcile shared shopping expenses, bills, etc.
'' There is one required command-line parameter, the name of the input file.
'' Each line in this file describes an item of expenditure and must have this
'' format:
''
''	description,payers,amount,consumers
''
'' description
''	is purely for reference and is ignored by the program.
''
'' payers and consumers
''	are the coded name or names of those who spent money on the item, and
''	those who used the item.  Each flatmate must have a one-letter code;
''	these are defined in the array ShortName@().  To specify multiple
''	flatmates who bought the item (each paying an equal amount), or used
''	the item, join multiple code letters together; e.g. JCF.  The special
''	entry * can be used as a shorthand for all flatmates.  This is useful
''	especially where an 'exceptional' item is being categorised after it
''	has already been accounted for in a more general entry: a * in the
''	payers field specifies that each flatmate has already paid an equal
''	share for the item, and the consumers field can then give the flatmates
''	who actually used the item.
''
'' amount
''	is the price in dollars, without a $ sign.
''
'' To do list:
''	- add a field for the date?  Could do filtering on these.
''	- add special char (e.g. -) which copies field from the prev line.
''	- add special char which duplicates entire prev line.
''		These 2 things would be handy to save typing for phone bills.

option base 1

const Flatmates% = 2

dim FName$(Flatmates)
for I% = 1 to Flatmates | read FName(I) | next
data "Flan", "Margot"

dim ShortName@(Flatmates)
for I = 1 to Flatmates | read ShortName(I) | next
data "F", "M"

AllNames$ = ""
for I = 1 to Flatmates
	AllNames = AllNames + ShortName(I)	
next

def PStr$(N&) where N >= 0 as mid(str(N), 2, 1000)
def Cents&(D!) as (D + 0.001) * 100
def Dollars$(C&) where C mod 100 < 10 as "$" + PStr(C \ 100) + ".0" + PStr(C mod 100)
def Dollars$(C&) as "$" + PStr(C \ 100) + "." + PStr(C mod 100)

LineNum% = 1

sub Warn((Msg$))
	shared LineNum

	print "skipped line", LineNum, ": ", Msg
endsub

sub FindWho((Names$), Who?(), Count%)
	shared ShortName@(), AllNames$

	if Names = "*" then
		Names = AllNames
	endif
	Count = 0
	Names = ucase(Names)
	for I% = 1 to Flatmates
		Found% = ShortName(I) in Names
		Who(I) = Found
		Count = Count + Found
	next
endsub

sub PrintMatrix
	shared Owes&()

	for I% = 1 to Flatmates
		for J% = 1 to Flatmates
			print Owes(I, J);;	
		next
		print
	next
endsub

sub Transfer(Owes&(), (I%), (J%), (K%))
	'' Change a debt I -> J -> K to go directly from I -> K.
	'' The array describes a directed graph without loops.  This routine will either leave the
	'' number of edges in the graph the same, or remove one or two edges.

	Amount& = Owes(I, J)
	if Owes(J, K) < Amount then
		Amount = Owes(J, K)
	endif

	Owes(I, J) = Owes(I, J) - Amount
	Owes(J, K) = Owes(J, K) - Amount

	if Owes(K, I) then
		Owes(K, I) = Owes(K, I) - Amount
		if Owes(K, I) < 0 then
			Owes(I, K) = -Owes(K, I)
			Owes(K, I) = 0
		endif
	else
		Owes(I, K) = Owes(I, K) + Amount
	endif
endsub

'' Spenders and consumers:
dim Paid?(Flatmates)
dim Used?(Flatmates)

dim Owes&(Flatmates, Flatmates)

open "i",1,"car-things.txt"
'print lof(1)
while not eof(1)
	Item$ = ""
	Payers$ = ""
	Cost! = 0
	Users$ = ""

	finput 1, Item, Payers, Cost, Users
	'print Item, Payers, Cost, Users

	'' Find the flatmate(s) who bought the item:

	NumPayers% = 0
	FindWho Payers, Paid, NumPayers

	if NumPayers <> 0 then
		'' Work out how many flatmates shared the item:

		SharedBy% = 0
		FindWho Users, Used, SharedBy

		if SharedBy > 0 then
			Divisor% = NumPayers * SharedBy

			for I = 1 to Flatmates
				if Used(I) then
					for J% = 1 to Flatmates
						if Paid(J) then
							Owes(I, J) = Owes(I, J) + Cents(Cost) \ Divisor
						endif
					next
				endif
			next
		else
			Warn "no one used " + Item + "."
		endif
	else
		Warn "don't know who " + Payers + " is."
	endif
	LineNum = LineNum + 1
wend
close 1

'PrintMatrix

'' Remove loops and opposing edges, ensuring totals are positive:

for I = 1 to Flatmates
	for J = 1 to I
		if Owes(I, J) > Owes(J, I) then
			Owes(I, J) = Owes(I, J) - Owes(J, I)
			Owes(J, I) = 0
		else
			Owes(J, I) = Owes(J, I) - Owes(I, J)
			Owes(I, J) = 0
		endif
	next
next

'PrintMatrix

'' Remove transitive debts.

for Iterations = 1 to Flatmates - 2
	for I = 1 to Flatmates
		for J = 1 to Flatmates
			if Owes(I, J) then
				for K = 1 to Flatmates
					if Owes(J, K) then
						Transfer Owes, I, J, K
					endif
				next
			endif
		next
	next
next

'PrintMatrix

'' Display the summary:

for I = 1 to Flatmates
	for J = 1 to Flatmates
		if Owes(I, J) then
			print FName(I), " owes ", FName(J), " ", Dollars(Owes(I, J))
		endif
	next
next

'XFREE
