n% = 0

repeat
	input , n
	select n
	case 1
		print 1
	case 2
		print 2
	case 3
		print 3
	otherwise
		print "some other value, =", n
	endselect
until n > 5
