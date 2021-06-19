'' Pack.bas -- pack a BASIC program so it takes up less space.

'' TODO detect MERGE statements and append merged files.
'' TODO option to rename ids more intelligently so that most frequently used get 1-char names (i.e. use symtab).
'' TODO option to pack commonly used keywords by replacing with special 1-byte symbols - would require interpreter changes ...
'' TODO option to replace numeric and/or string literals with named constants (further option - only if it would save space)
'' TODO option to remove unneccessary type specifiers (further option - use DEFtype statements (when supported ...))
'' TODO option to remove unneccessary keywords, e.g. THEN, AS, etc.
'' TODO 'module safe mode' where subs, functions, globals consts and vars wouldn't be packed
'' TODO option to include only needed prelude definitions, and rename them too

'' Use the lexical analyser:
merge "lexer.bas" | gosub Lx

const NeedSpaceBetween$ = LxUCLetter + LxDecimalDigit + "_"

const Name1stCh$ = LxUCLetter
const NameRestCh$ = LxUCLetter + LxDecimalDigit + "_"
NextName$ = "A"

const MaxName = 100
dim NameIn$(MaxName)
dim NameOut$(MaxName)
NameCount% = 0

const NIllegal% = 6
dim Illegal$(NIllegal)
for I = 1 to NIllegal | read Illegal(I) | next
data "AS", "IF", "IN", "ON", "OR", "TO"

def IsIllegal?(T$) as AnyIllegal(T, 1)
def AnyIllegal?(T$, Idx%) where Idx > NIllegal as false
def AnyIllegal?(T$, Idx%) where Illegal(Idx) = T as true
def AnyIllegal?(T$, Idx%) as AnyIllegal(T, Idx + 1)

'print IsIllegal("A"), IsIllegal("FOO"), IsIllegal("AS"), IsIllegal("TO")

const ColsPerLine% = 80
CurrCol% = 0

sub Rename(T$)
	shared NextName, NameIn(), NameOut(), NameCount, Illegal()
	
	for I = 1 to NameCount
		if NameIn(I) = T then
			T = NameOut(I)
			exitsub
		endif
	next

	NameCount = NameCount + 1
	NameIn(NameCount) = T
	T = NextName
	NameOut(NameCount) = NextName

	I = len(NextName)
	Carry? = true
	while I > 0 and (Carry or IsIllegal(NextName))
		ChSet$ = Name1stCh
		if I > 1 then
			ChSet$ = NameRestCh
		endif
		
		ChIdx% = instr(1, ChSet, mid(NextName, I, 1))
		Carry = ChIdx = len(ChSet)
		if Carry then
			setmid NextName, I, 1, left(ChSet, 1)
		else
			setmid NextName, I, 1, mid(ChSet, ChIdx + 1, 1)
		endif
		
		'Allowed? = true
		'for J = 1 to NIllegal
		'	if Illegal(J) = NextName then
		'		Allowed = false
		'	endif
		'next
		
		'if Allowed then
			I = I - 1
		'endif
	wend

	if Carry then
		NextName = string(len(NextName) + 1, "A")
	endif
endsub

sub LxParse((L$), Token$(), (Count))
	shared CurrCol

	if CurrCol >= ColsPerLine then
		print
		CurrCol = 0
	elseif CurrCol > 0 then
		print "|";
		CurrCol = CurrCol + 1
	endif
	
	Previous$ = ""
	for I = 1 to Count
		T$ = Token(I)

		if left(T, 1) in LxUCLetter then
			IsKeyword? = false
			LxGetIsKeyword T, IsKeyword

			if not IsKeyword then
				Rename T
			elseif T = "PRINT" then
				T = "?"
			endif
		endif
		
		if right(Previous, 1) in NeedSpaceBetween _
		and left(T, 1) in NeedSpaceBetween then
			print " ";
			CurrCol = CurrCol + 1
		endif

		print T;
		CurrCol = CurrCol + len(T)

		Previous = T
	next
	
	if Count > 0 then

	endif
endsub

sub LxError((Msg$))
	print "*** lex error:", Msg
	fail 21
endsub

open "i",1,argv(1)
LxReadFile 1

if CurrCol > 0 then | print | endif

close 1
END
