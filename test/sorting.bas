'' Various simple sorting algorithms.

sub BubbleSort(A&())	
	repeat
		Sorted? = true
		for i = lbound(A, 1) to ubound(A, 1) - 1 '' TODO found I can put more 'to' parts and they're ignored
			if A(i) < A(i + 1)
				Sorted = false
				swap A(i), A(i + 1)
			endif
		next
	until Sorted
end sub

sub SelectionSort(A&())
	for i = lbound(A, 1) to ubound(A, 1) - 1
		MaxValIdx% = i
		for j = i + 1 to ubound(A, 1)
			if A(j) > A(MaxValIdx) then MaxValIdx = j '' TODO shouldn't IFTHENLET if assignment contains side effects
		next
		swap A(i), A(MaxValIdx)
	next
end sub

sub InsertionSort(A&())
	for i = lbound(A, 1) + 1 to ubound(A, 1)
		j% = i
		Finished? = false
		repeat
			if A(j - 1) < A(j) then | swap A(j), A(j - 1) | else | Finished = true | endif
			j = j - 1
		until j <= lbound(A, 1) or Finished
	next
end sub

sub Partition(A&(), (Low%), (High%), P%)
	Pivot& = A((Low + High) \ 2)
	LI% = Low - 1 ' left index
	RI% = High + 1 ' right index
	repeat
		repeat
			LI = LI + 1
		until A(LI) <= Pivot
		repeat
			RI = RI - 1
		until A(RI) >= Pivot
		if LI < RI then swap A(LI), A(RI)
	until LI >= RI
	P = RI
end sub

sub QuickSort(A&(), (Low%), (High%))
	if Low >= High then exitsub '' done! '' TODO 'exit sub' with a space causes strange cf stack or jumping behaviour
	P% = -1
	Partition A, Low, High, P
	QuickSort A, Low, P
	QuickSort A, P + 1, High
end sub

sub MakeRandom(A&())
	for i = lbound(A, 1) to ubound(A, 1) '' TODO found by typo letter l for digit 1 that 'undefined var' name isn't printed any more
		A(i) = random(100)
	next
end sub

sub PrintArray(A&())
	for i = lbound(A, 1) to ubound(A, 1)
		print A(i);
	next
	print , "Sorted? "; IsSorted(lbound(A, 1))
end sub

option base 1

Size% = 10
dim A&(Size)

def IsSorted?(i%) where i >= ubound(A, 1) as 1
def IsSorted?(i%) as A(i) >= A(i + 1) and IsSorted(i + 1) '' only checks descending order!

MakeRandom A
PrintArray A
print ">>> BubbleSort >>>"
BubbleSort A
PrintArray A
print

MakeRandom A
PrintArray A
print ">>> SelectionSort >>>"
SelectionSort A
PrintArray A
print

MakeRandom A
PrintArray A
print ">>> InsertionSort >>>"
InsertionSort A
PrintArray A
print

MakeRandom A
PrintArray A
print ">>> QuickSort >>>"
QuickSort A, lbound(A, 1), ubound(A, 1)
PrintArray A
print

