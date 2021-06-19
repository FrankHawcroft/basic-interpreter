for i = 1 to argc
	open "i", 1, argv(i)
	while not eof(1)
		s$ = ""
		flineinput 1, s
		print s
	wend
	close 1
next
