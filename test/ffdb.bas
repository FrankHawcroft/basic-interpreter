'' A simple flat file database.

const MaxField = 64
const MaxRecord = 100

dim DB$(MaxField, MaxRecord)
dim FName$(MaxField)
dim FType$(MaxField)

NField% = 0
NRecord% = 0

repeat
	cmd$ = ""
	lineinput , cmd
	if cmd = "q" then
		end
	elseif left(cmd, 2) = "af" then
		NField = NField + 1
		FName(NField) = mid(cmd, 3, instr(cmd, " ") - 3)
		FType(NField) = mid(cmd, instr(cmd, " ") + 1, 1)
	elseif cmd = "ar" then
		NRecord = NRecord + 1
		for i = 1 to NField
			input FName(i), DB(i, NRecord)
		next
	elseif cmd = "vr" then
		'let m = 0 | for i = -5 to 5 | m = 10 * i | next
		for i = 1 to NField
			print FName(i);
		next
		print
		for i = 1 to NRecord
			for j = 1 to NField
				print DB(j, i);
			next
			print
		next
	endif
forever
