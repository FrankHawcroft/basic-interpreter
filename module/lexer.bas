'' LEXER.BAS
'' =========
''
'' This is a library for scanning BASIC programs.
''
'' Usage as a library is:
''
''   merge "lexer.bas" | gosub Lx
''
'' LxReadFile is the major subprogram provided, which will scan an entire
'' BASIC program file.
''
'' Programs that call LxReadFile must define two subprograms -
''
''	 LxParse((StatementText$), Tokens$(), (NTokens%))
''
'' will be called after each statement has been scanned.
''
''   LxError((Msg$))
''
'' will be called each time a scanning error occurs.
''
'' In LxError, the label LxReadFileResume can be used as the destination for
'' a RESUME statement, to continue scanning at the next statement in the
'' program.
''
'' LxLine is a global variable containing the current line number in the input
'' file. There are some other constants and variables which may be useful too.
''
'' Lexer.bas can also be run as a standalone program, in which case it
'' takes one command-line argument, which should be the filename of a BASIC
'' program to scan. It will then scan and print out the tokens in
'' the program.
''
'' E.g.
''
''   nb lexer.bas myprogram.bas
''

'' TODO return text of whole stmt for multi-line stmts, not just last line.

'' Example subprogram implementations -

sub LxParse((L$), Tokens$(), (Count%))
	'' This subprogram should be defined in the client module when using 
	'' lexer.bas as a library.
	'' L contains the text of the first line of the statement.
	'' Tokens contains the tokens.
	'' Count is the number of tokens.

	'' Example LxParse:
	'' Just prints out each original line as a comment, followed by the
	'' tokens.

	print "REM ", L
	for I = 1 to Count
		print Tokens(I), " ";
	next
	print
endsub

sub LxError((Msg$))
	'' This subprogram should be be defined in the client module.
	'' Typically it will print the error message, and discard the current
	'' input line by doing a RESUME into the loop which is reading lines of
	'' code.

	shared LxLine

	print "**** lexical error on line", LxLine, ": ", Msg
	resume LxReadFileResume
endsub

'' Testbed mode -

gosub Lx
open "i", 1, argv(1)
LxReadFile 1

END

'' Or - library-only mode -

'if executed then
'	print "lexer.bas cannot be run as a program"
'	fail 21
'endif

'' The library entry point:
Lx:

defint a-z

'' Constants
'' =========

'' General limits:

const LxMaxLineLength = 255	'' maximum number of characters on a line
const LxMaxTokens = 80		'' maximum number of tokens in a statement

'' Character sets:

const LxWhitespace$ = " " + chr(9)
const LxDecimalDigit$ = "0123456789"
const LxOctalDigit$ = "01234567"
const LxHexDigit$ = "0123456789abcdefABCDEF"
const LxLCLetter$ = "abcdefghijklmnopqrstuvwxyz"
const LxUCLetter$ = ucase(LxLCLetter)
const LxLetter$ = LxLCLetter + LxUCLetter
const LxNameBodyChar$ = LxLetter + LxDecimalDigit + "_"
const LxTypeSpec$ = "$%?@&!#"
const LxSingleCharSymbol$ = "(),=+-;*^:/\" + LxTypeSpec
const LxLtGt$ = "<>"
const LxSymbolChar$ = LxSingleCharSymbol + LxLtGt
const LxQuote@ = chr(34)
const LxNumberFirst$ = LxDecimalDigit + ".&"
const LxSign$ = "+-"

'' Keywords:

const LxKwTableSize = 200
dim LxKw$(LxKwTableSize)

LxI% = 1
repeat
	read LxKw(LxI)
	LxI = LxI + 1
until LxKw(LxI - 1) = ""
const LxKwCount = LxI - 2

'' The table of keywords must be in alphabetical order for quick searching.
data "ABS"
data "ACTIVATE"
data "AND"
data "ARGC"
data "ARGV"
data "AS"
data "ASC"
data "ATN"
data "BEEP"
data "BITAND"
data "BITNOT"
data "BITOR"
data "BITXOR"
data "BREAK"
data "CALL"
data "CASE"
data "CDBL"
data "CHDIR"
data "CHR"
data "CINT"
data "CLEAR"
data "CLNG"
data "CLOSE"
data "CLS"
data "COLOR"
data "COLOUR"
data "CONST"
data "COS"
data "CPX"
data "CPY"
data "CSNG"
data "CVB"
data "CVI"
data "CVL"
data "CVS"
data "DAMAGEDWINDOW"
data "DATA"
data "DATE"
data "DEF"
data "DIM"
data "DISABLE"
data "ELSE"
data "ELSEIF"
data "ENABLE"
data "END"
data "ENDIF"
data "ENDSELECT"
data "ENDSUB"
data "EOF"
data "EQV"
data "ERL"
data "ERR"
data "ERROR"
data "EXECUTED"
data "EXIT"
data "EXITSUB"
data "EXP"
data "FAIL"
data "FALSE"
data "FIELD"
data "FILES"
data "FINPUT"
data "FIX"
data "FLINEINPUT"
data "FOCUS"
data "FOR"
data "FOREVER"
data "FORGET"
data "FPRINT"
data "FRE"
data "GET"
data "GOSUB"
data "GOTO"
data "HEX"
data "IF"
data "IMP"
data "IN"
data "INIT"
data "INKEY"
data "INPUT"
data "INSTR"
data "INT"
data "KILL"
data "LEFT"
data "LEN"
data "LET"
data "LINE"
data "LINEINPUT"
data "LOC"
data "LOCAL"
data "LOF"
data "LOG"
data "LSET"
data "MENU"
data "MENUPICKED"
data "MENUSTATE"
data "MERGE"
data "MID"
data "MKB"
data "MKI"
data "MKL"
data "MKS"
data "MOD"
data "MOUSE"
data "MOVETO"
data "NAME"
data "NEXT"
data "NOT"
data "OCT"
data "ON"
data "OPEN"
data "OR"
data "OTHERWISE"
data "OUTPUTWINDOW"
data "PALETTE"
data "PCOL"
data "POINT"
data "PRESET"
data "PRINT"
data "PROW"
data "PSET"
data "PUT"
data "RANDOM"
data "RANDOMIZE"
data "READ"
data "REM"
data "REPEAT"
data "RESET"
data "RESUME"
data "RETURN"
data "RIGHT"
data "RND"
data "RSET"
data "RUN"
data "SCREEN"
data "SCREENCLOSE"
data "SCREENINFO"
data "SELECT"
data "SETMID"
data "SGN"
data "SHARED"
data "SIN"
data "SLEEP"
data "SPACE"
data "SQR"
data "STATUS"
data "STEP"
data "STR"
data "STRING"
data "SUB"
data "SUSPEND"
data "SWAP"
data "TAN"
data "THEN"
data "TIME"
data "TIMER"
data "TO"
data "TOFRONT"
data "TROFF"
data "TRON"
data "TRUE"
data "UCASE"
data "UNTIL"
data "VAL"
data "WAIT"
data "WEND"
data "WHERE"
data "WHILE"
data "WINDOW"
data "WINDOWCLOSE"
data "WINDOWHEIGHT"
data "WINDOWOUTPUT"
data "WINDOWTABS"
data "WINDOWWIDTH"
data "WLOCATE"
data "WPRINT"
data "WRITE"
data "XFREE"
data "XOR"
data "XSTACK"
data "" '' sentinel

'' Search the table (LxKw) for a possible keyword/built-in using binary search.
'' The table must be sorted in ascending order, and must contain names in
'' uppercase only; case differences are not significant when comparing BASIC
'' names. If the key is found in the table, its index is returned, otherwise -1.

def LxFindKwRec%(S$, Low%, High%) where Low > High or Low < lbound(LxKw, 1) or High > LxKwCount as -1
def LxFindKwRec%(S$, Low%, High%) where LxKw((Low + High) / 2) = S as (Low + High) / 2
def LxFindKwRec%(S$, Low%, High%) where LxKw((Low + High) / 2) < S as LxFindKwRec(S, 1 + (Low + High) / 2, High)
def LxFindKwRec%(S$, Low%, High%) as LxFindKwRec(S, Low, (Low + High) / 2 - 1)

'print LxFindKwRec("END", 1, LxKwCount), LxKw(LxFindKwRec("END", 1, LxKwCount))
'print LxFindKwRec("ABS", 1, LxKwCount), LxKw(LxFindKwRec("ABS", 1, LxKwCount))
'print LxFindKwRec("XSTACK", 1, LxKwCount), LxKw(LxFindKwRec("XSTACK", 1, LxKwCount))
'print LxFindKwRec("NONEXISTENT", 1, LxKwCount)

def LxIsKeyword?(S$) as LxFindKwRec(ucase(S), 1, LxKwCount) > -1

'' Globals
'' =======

LxLine% = 0		'' the line number

'' Subprograms
'' ===========

sub LxReadFile((FileID%))
	shared LxLine

	dim Tokens$(LxMaxTokens)

	LxLine% = 0
	SplitIndex% = 0
	TokenCount% = 0
	L$ = ""

	LxReadFileResume:

	while not eof(FileID)
		LxReadStatement FileID, L, SplitIndex, Tokens, TokenCount
		if TokenCount > 0 then
			LxParse L, Tokens, TokenCount
		endif
	wend
endsub

sub LxReadStatement((File), TheLine$, SplitIndex, Token$(), Count)
	shared LxLine

	Count = 0
	StmtFinished? = false
	while not (eof(File) or StmtFinished)
		if SplitIndex = 0 then
			LxLine = LxLine + 1
			flineinput File, TheLine
		else
			TheLine = mid(TheLine, SplitIndex + 1, LxMaxLineLength)
			SplitIndex = 0
		endif
		StmtFinished? = false
		LxTokenise TheLine, Token, Count, StmtFinished, SplitIndex
	wend
	if not StmtFinished then
		LxError "continuation on last line of file"
	endif
endsub

sub LxTokenise((Text$), Token$(), Count, Finished?, SplitIndex)
	'' NOTE: Count is an IN and an OUT parameter.
	'' Finished and SplitIndex are OUT only parameters.

	Finished = true
	SplitIndex = 0

	Idx% = 1
	Length% = len(Text)
	LxSkipWhitespace Text, Idx
	while Idx <= Length
		C@ = mid(Text, Idx, 1)
		if C in LxLetter then
			'' Name or keyword or operator.

			Word$ = ""
			LxCollectChars Text, LxNameBodyChar, Word, Idx

			'' NOTE: all words are converted to upper case here!
			Word = ucase(Word)

			if Word = "REM" then
				Idx = Length + 1
			else
				Count = Count + 1
				Token(Count) = Word
			endif
		elseif C in LxSingleCharSymbol then
			'' Operator, delimiter, :, or type specifier.

			Idx = Idx + 1
			Count = Count + 1
			Token(Count) = C
		elseif C in LxNumberFirst then
			'' Numeric literal.

			Count = Count + 1
			LxGetNumber Text, Idx, Token(Count)
		elseif C = LxQuote then
			'' String or character literal.

			S$ = ""
			repeat
				S = S + C
				Idx = Idx + 1
				C = mid(Text, Idx, 1)
			until C = "" or C = LxQuote
			Idx = Idx + 1
			if C = LxQuote then
				Count = Count + 1
				Token(Count) = S + LxQuote
			else
				LxError "string without closing quote"
			endif
		elseif C in LxLtGt then
			'' One of: >  <  >=  <= <>
			
			Count = Count + 1
			Token(Count) = C
			Idx = Idx + 1
			C = mid(Text, Idx, 1)
			if C = "=" or (Token(Count) = "<" and C = ">") then
				Token(Count) = Token(Count) + C
				Idx = Idx + 1
			endif
		elseif C = "'" then
			'' The rest of the line is a comment.
			Idx = Length + 1
		elseif C = "_" then
			'' Continuation.  Anything following on this line is
			'' ignored.
			'' TO DO: actually the only thing that can follow is a
			'' comment.
			
			Finished = false
			Idx = Length + 1
		elseif C = "|" then
			'' Statement separator.  This line contains more than
			'' one statement.

			SplitIndex = Idx + 1		
			Idx = Length + 1
		else
			LxError "unrecognised character " + C
		endif
		LxSkipWhitespace Text, Idx
	wend
	
	'' Ensure token array is terminated by an empty string:
	Token(Count + 1) = ""
endsub

sub LxSkipWhitespace((Text$), Idx) static
	while mid(Text, Idx, 1) in LxWhitespace
		Idx = Idx + 1
	wend
endsub

sub LxGetNumber((Text$), Idx, Token$)
	'' Supports all possible numeric literals: decimal, hex or octal
	'' integers, floating point with optional exponent, and trailing
	'' type specifiers.
	'' NOTE: literals are assumed to be unsigned!
	'' TO DO: this routine assumes Amiga data type sizes

	Token = ""

	'' 1. Optional prefix specifying the base.

	'' Detect hex and octal literals:

	Octal? = false
	Hexadecimal? = false
	MinimumLength% = 1
	'' A literal too big to be a long integer is assumed to be a floating-
	'' point literal; hence the default maximum length is rather large.
	MaximumLength% = 100

	C@ = mid(Text, Idx, 1)
	if C = "&" then
		if mid(Text, Idx + 1, 1) in "hH" then
			Hexadecimal = true
			MinimumLength = 3	'' &h1
			MaximumLength = 10	'' &hFFFFFFFF 
			Token = "&H"
			Idx = Idx + 2
		elseif mid(Text, Idx + 1, 1) in "oO" then
			Octal = true
			MinimumLength = 3	'' &o1
			MaximumLength = 13	'' &o17777777777
			Token = "&O"
			Idx = Idx + 2
		else
			Octal = true
			MinimumLength = 2	'' &1
			MaximumLength = 12	'' &17777777777
			Token = "&O"
			Idx = Idx + 1
		endif
	elseif C = "0" then
		if mid(Text, Idx + 1, 1) in "xX" then
			Hexadecimal = true
			MinimumLength = 3	'' 0x1
			MaximumLength = 10	'' 0xFFFFFFFF
			Token = "&H"
			Idx = Idx + 2
		endif
	endif

	'' 2. The main part

	FloatingPoint? = false

	if Hexadecimal then
		LxCollectChars Text, LxHexDigit, Token, Idx
	elseif Octal then
		LxCollectChars Text, LxOctalDigit, Token, Idx
	else
		'' Decimal integer or floating-point literal.

		'' Whole part:

		LxCollectChars Text, LxDecimalDigit, Token, Idx

		'' Optional fractional part:

		if mid(Text, Idx, 1) = "." then
			FloatingPoint = true
			MinimumLength = MinimumLength + 1
			Token = Token + "."
			Idx = Idx + 1
			LxCollectChars Text, LxDecimalDigit, Token, Idx
		endif

		'' Optional exponent:

		if mid(Text, Idx, 1) in "eE" then
			FloatingPoint = true
			MinimumLength = MinimumLength + 2
			Token = Token + "E"
			Idx = Idx + 1
			if mid(Text, Idx, 1) in LxSign then
				MinimumLength = MinimumLength + 1
				Token = Token + mid(Text, Idx, 1)
				Idx = Idx + 1
			endif
			LxCollectChars Text, LxDecimalDigit, Token, Idx
		endif
	endif

	'' 3. Optional type specifier

	C = mid(Text, Idx, 1)
	if C in LxTypeSpec then
		Token = Token + C
		Idx = Idx + 1
		MinimumLength = MinimumLength + 1
		MaximumLength = MaximumLength + 1

		'' Check for illegal type spec (char or string or Boolean):

		if C in "@$?" then
			LxError "illegal type spec for numeric literal"
		endif
		
		'' Check that type spec is consistent with format:

		if (C in "!#" and (Octal or Hexadecimal)) _
		or (C in "%&" and FloatingPoint) then
			LxError "type spec inconsistent with literal format"
		endif

		'' Adjust max length for decimal long integer literals:

		if C = "&" and not (Octal or Hexadecimal) then
			MaximumLength = 11	'' 2147483647&
		endif

		'' Adjust max length for magnitude of small integer type:

		if C = "%" then
			if Octal then
				'' Difference between lengths of 17777777777
				'' and 77777
				MaximumLength = MaximumLength - 6
			elseif Hexadecimal then
				MaximumLength = MaximumLength - 4
			else
				MaximumLength = MaximumLength - 5
			endif
		endif
	endif

	'' Check token length:

	if len(Token) < MinimumLength or MaximumLength < len(Token) then
		LxError "bad literal " + Token
	endif

	Token = ucase(Token)
endsub

sub LxCollectChars((Text$), (CharSet$), Token$, Idx) static
	'' Collect characters from Text starting at position Idx, which are in
	'' CharSet.  They are appended to Token.

	StartIdx% = Idx
	if mid(Text, Idx, 1) in CharSet then
		repeat
			Idx = Idx + 1
		until not (mid(Text, Idx, 1) in CharSet)
	endif
	Token = Token + mid(Text, StartIdx, Idx - StartIdx)
endsub

'' Procedural version of the LxIsKeyword function -
sub LxGetIsKeyword((Token$), IsKeyword?)
	'' Check whether the Token is in the list of keywords.  Note that
	'' operator and other symbols are not in the keyword list; only names
	'' that could be confused with identifiers are included.

	shared LxKw$(), LxKwCount

	Index% = 0
	LxBSearch LxKw, LxKwCount, Token, Index
	IsKeyword = Index > 0
endsub

sub LxBSearch(Table$(), (Size), (Key$), Index%)
	Key = ucase(Key)
	Low% = 1
	High% = Size
	Index = (Low + High) / 2
	repeat
		Cur$ = Table(Index)
		if Cur < Key then
			Low = Index + 1
		elseif Cur > Key then
			High = Index - 1
		else
			exitsub
		endif
		Index = (Low + High) / 2
	until Low > High
	Index = 0
endsub

RETURN '' return to client module
