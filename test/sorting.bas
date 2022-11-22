'' Various simple sorting algorithms.

sub BubbleSort(A&())
	shared Size
	
	repeat
		Sorted? = true
		for i = 1 to Size - 1
			if A(i) < A(i + 1)
				Sorted = false
				print "Swapping ", A(i), A(i + 1)
				swap A(i), A(i + 1)
			endif
		next
		for i = 1 to Size
			print A(i);
		next
		print
	until Sorted
end sub


Size% = 10
dim A&(Size)

for i = 1 to Size
	A(i) = random(100)
next
for i = 1 to Size
	print A(i);
next
print

BubbleSort A

for i = 1 to Size
	print A(i);
next
print
