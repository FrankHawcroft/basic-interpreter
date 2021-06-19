'' Version of ../module/lexer.bas, modified to be a test program for the interpreter.

' '' Testbed:
gosub Lx

open "i", 1, "c_lexer.bas"
TotalTokens& = 0
LxReadFile 1
print "Total tokens in program:", TotalTokens
close 1
'xfree
END

'if executed then
'	print "lexer.bas cannot be run as a program"
'	fail 21
'endif

'' The library entry point:
Lx:

'' Constants
'' =========

'' General limits:

const LxMaxLineLength% = 255	'' maximum number of characters on a line
const LxMaxTokens% = 80			'' maximum number of tokens in a statement

'' Character sets for lexical analysis:

const LxWhitespace$ = " " + chr(9)
const LxDecimalDigit$ = "0123456789"
const LxOctalDigit$ = "01234567"
const LxHexDigit$ = "0123456789abcdefABCDEF"
const LxLCLetter$ = "abcdefghijklmnopqrstuvwxyz"
const LxUCLetter$ = ucase(LxLCLetter)
const LxLetter$ = LxLCLetter + LxUCLetter
const LxNameBodyChar$ = LxLetter + LxDecimalDigit + "_"
const LxTypeSpec$ = "$%?@&!"
const LxSingleCharSymbol$ = "(),=+-;*^:/\" + LxTypeSpec
const LxLtGt$ = "<>"
const LxSymbolChar$ = LxSingleCharSymbol + LxLtGt
const LxQuote@ = chr(34)
const LxNumberFirst$ = LxDecimalDigit + ".&"
const LxSign$ = "+-"

'' Keywords:

const LxKwTableSize% = 200
dim LxKw$(LxKwTableSize)
LxI% = 1

'' The table of keywords must be in alphabetical order.

LxKw(LxI) = "ABS" | LxI = LxI + 1
LxKw(LxI) = "ACTIVATE" | LxI = LxI + 1
LxKw(LxI) = "AND" | LxI = LxI + 1
LxKw(LxI) = "ARGC" | LxI = LxI + 1
LxKw(LxI) = "ARGV" | LxI = LxI + 1
LxKw(LxI) = "AS" | LxI = LxI + 1
LxKw(LxI) = "ASC" | LxI = LxI + 1
LxKw(LxI) = "ATN" | LxI = LxI + 1
LxKw(LxI) = "BEEP" | LxI = LxI + 1
LxKw(LxI) = "BITAND" | LxI = LxI + 1
LxKw(LxI) = "BITNOT" | LxI = LxI + 1
LxKw(LxI) = "BITOR" | LxI = LxI + 1
LxKw(LxI) = "BITXOR" | LxI = LxI + 1
LxKw(LxI) = "BREAK" | LxI = LxI + 1
LxKw(LxI) = "CALL" | LxI = LxI + 1
LxKw(LxI) = "CASE" | LxI = LxI + 1
LxKw(LxI) = "CDBL" | LxI = LxI + 1
LxKw(LxI) = "CHDIR" | LxI = LxI + 1
LxKw(LxI) = "CHR" | LxI = LxI + 1
LxKw(LxI) = "CINT" | LxI = LxI + 1
LxKw(LxI) = "CLEAR" | LxI = LxI + 1
LxKw(LxI) = "CLNG" | LxI = LxI + 1
LxKw(LxI) = "CLOSE" | LxI = LxI + 1
LxKw(LxI) = "CLS" | LxI = LxI + 1
LxKw(LxI) = "COLOR" | LxI = LxI + 1
LxKw(LxI) = "COLOUR" | LxI = LxI + 1
LxKw(LxI) = "CONST" | LxI = LxI + 1
LxKw(LxI) = "COS" | LxI = LxI + 1
LxKw(LxI) = "CPX" | LxI = LxI + 1
LxKw(LxI) = "CPY" | LxI = LxI + 1
LxKw(LxI) = "CSNG" | LxI = LxI + 1
LxKw(LxI) = "CVB" | LxI = LxI + 1
LxKw(LxI) = "CVI" | LxI = LxI + 1
LxKw(LxI) = "CVL" | LxI = LxI + 1
LxKw(LxI) = "CVS" | LxI = LxI + 1
LxKw(LxI) = "DAMAGEDWINDOW" | LxI = LxI + 1
LxKw(LxI) = "DATE" | LxI = LxI + 1
LxKw(LxI) = "DEF" | LxI = LxI + 1
LxKw(LxI) = "DIM" | LxI = LxI + 1
LxKw(LxI) = "DISABLE" | LxI = LxI + 1
LxKw(LxI) = "ELSE" | LxI = LxI + 1
LxKw(LxI) = "ELSEIF" | LxI = LxI + 1
LxKw(LxI) = "ENABLE" | LxI = LxI + 1
LxKw(LxI) = "END" | LxI = LxI + 1
LxKw(LxI) = "ENDIF" | LxI = LxI + 1
LxKw(LxI) = "ENDSELECT" | LxI = LxI + 1
LxKw(LxI) = "ENDSUB" | LxI = LxI + 1
LxKw(LxI) = "EOF" | LxI = LxI + 1
LxKw(LxI) = "EQV" | LxI = LxI + 1
LxKw(LxI) = "ERL" | LxI = LxI + 1
LxKw(LxI) = "ERR" | LxI = LxI + 1
LxKw(LxI) = "ERROR" | LxI = LxI + 1
LxKw(LxI) = "EXECUTED" | LxI = LxI + 1
LxKw(LxI) = "EXITSUB" | LxI = LxI + 1
LxKw(LxI) = "EXP" | LxI = LxI + 1
LxKw(LxI) = "FAIL" | LxI = LxI + 1
LxKw(LxI) = "FALSE" | LxI = LxI + 1
LxKw(LxI) = "FIELD" | LxI = LxI + 1
LxKw(LxI) = "FILES" | LxI = LxI + 1
LxKw(LxI) = "FINPUT" | LxI = LxI + 1
LxKw(LxI) = "FIX" | LxI = LxI + 1
LxKw(LxI) = "FLINEINPUT" | LxI = LxI + 1
LxKw(LxI) = "FOCUS" | LxI = LxI + 1
LxKw(LxI) = "FOR" | LxI = LxI + 1
LxKw(LxI) = "FOREVER" | LxI = LxI + 1
LxKw(LxI) = "FORGET" | LxI = LxI + 1
LxKw(LxI) = "FPRINT" | LxI = LxI + 1
LxKw(LxI) = "FRE" | LxI = LxI + 1
LxKw(LxI) = "GET" | LxI = LxI + 1
LxKw(LxI) = "GOSUB" | LxI = LxI + 1
LxKw(LxI) = "GOTO" | LxI = LxI + 1
LxKw(LxI) = "HEX" | LxI = LxI + 1
LxKw(LxI) = "IF" | LxI = LxI + 1
LxKw(LxI) = "IMP" | LxI = LxI + 1
LxKw(LxI) = "IN" | LxI = LxI + 1
LxKw(LxI) = "INIT" | LxI = LxI + 1
LxKw(LxI) = "INKEY" | LxI = LxI + 1
LxKw(LxI) = "INPUT" | LxI = LxI + 1
LxKw(LxI) = "INSTR" | LxI = LxI + 1
LxKw(LxI) = "INT" | LxI = LxI + 1
LxKw(LxI) = "KILL" | LxI = LxI + 1
LxKw(LxI) = "LEFT" | LxI = LxI + 1
LxKw(LxI) = "LEN" | LxI = LxI + 1
LxKw(LxI) = "LET" | LxI = LxI + 1
LxKw(LxI) = "LINE" | LxI = LxI + 1
LxKw(LxI) = "LINEINPUT" | LxI = LxI + 1
LxKw(LxI) = "LOC" | LxI = LxI + 1
LxKw(LxI) = "LOCAL" | LxI = LxI + 1
LxKw(LxI) = "LOF" | LxI = LxI + 1
LxKw(LxI) = "LOG" | LxI = LxI + 1
LxKw(LxI) = "LSET" | LxI = LxI + 1
LxKw(LxI) = "MENU" | LxI = LxI + 1
LxKw(LxI) = "MENUPICKED" | LxI = LxI + 1
LxKw(LxI) = "MENUSTATE" | LxI = LxI + 1
LxKw(LxI) = "MERGE" | LxI = LxI + 1
LxKw(LxI) = "MID" | LxI = LxI + 1
LxKw(LxI) = "MKB" | LxI = LxI + 1
LxKw(LxI) = "MKI" | LxI = LxI + 1
LxKw(LxI) = "MKL" | LxI = LxI + 1
LxKw(LxI) = "MKS" | LxI = LxI + 1
LxKw(LxI) = "MOD" | LxI = LxI + 1
LxKw(LxI) = "MOUSE" | LxI = LxI + 1
LxKw(LxI) = "MOVETO" | LxI = LxI + 1
LxKw(LxI) = "NAME" | LxI = LxI + 1
LxKw(LxI) = "NEXT" | LxI = LxI + 1
LxKw(LxI) = "NOT" | LxI = LxI + 1
LxKw(LxI) = "OCT" | LxI = LxI + 1
LxKw(LxI) = "ON" | LxI = LxI + 1
LxKw(LxI) = "OPEN" | LxI = LxI + 1
LxKw(LxI) = "OR" | LxI = LxI + 1
LxKw(LxI) = "OTHERWISE" | LxI = LxI + 1
LxKw(LxI) = "OUTPUTWINDOW" | LxI = LxI + 1
LxKw(LxI) = "PALETTE" | LxI = LxI + 1
LxKw(LxI) = "PCOL" | LxI = LxI + 1
LxKw(LxI) = "POINT" | LxI = LxI + 1
LxKw(LxI) = "PRESET" | LxI = LxI + 1
LxKw(LxI) = "PRINT" | LxI = LxI + 1
LxKw(LxI) = "PROW" | LxI = LxI + 1
LxKw(LxI) = "PSET" | LxI = LxI + 1
LxKw(LxI) = "PUT" | LxI = LxI + 1
LxKw(LxI) = "RANDOM" | LxI = LxI + 1
LxKw(LxI) = "RANDOMIZE" | LxI = LxI + 1
LxKw(LxI) = "READ" | LxI = LxI + 1
LxKw(LxI) = "REM" | LxI = LxI + 1
LxKw(LxI) = "REPEAT" | LxI = LxI + 1
LxKw(LxI) = "RESET" | LxI = LxI + 1
LxKw(LxI) = "RESUME" | LxI = LxI + 1
LxKw(LxI) = "RETURN" | LxI = LxI + 1
LxKw(LxI) = "RIGHT" | LxI = LxI + 1
LxKw(LxI) = "RND" | LxI = LxI + 1
LxKw(LxI) = "RSET" | LxI = LxI + 1
LxKw(LxI) = "RUN" | LxI = LxI + 1
LxKw(LxI) = "SCREEN" | LxI = LxI + 1
LxKw(LxI) = "SCREENCLOSE" | LxI = LxI + 1
LxKw(LxI) = "SCREENINFO" | LxI = LxI + 1
LxKw(LxI) = "SELECT" | LxI = LxI + 1
LxKw(LxI) = "SETMID" | LxI = LxI + 1
LxKw(LxI) = "SGN" | LxI = LxI + 1
LxKw(LxI) = "SHARED" | LxI = LxI + 1
LxKw(LxI) = "SIN" | LxI = LxI + 1
LxKw(LxI) = "SLEEP" | LxI = LxI + 1
LxKw(LxI) = "SPACE" | LxI = LxI + 1
LxKw(LxI) = "SQR" | LxI = LxI + 1
LxKw(LxI) = "STATUS" | LxI = LxI + 1
LxKw(LxI) = "STEP" | LxI = LxI + 1
LxKw(LxI) = "STR" | LxI = LxI + 1
LxKw(LxI) = "STRING" | LxI = LxI + 1
LxKw(LxI) = "SUB" | LxI = LxI + 1
LxKw(LxI) = "SUSPEND" | LxI = LxI + 1
LxKw(LxI) = "SWAP" | LxI = LxI + 1
LxKw(LxI) = "TAN" | LxI = LxI + 1
LxKw(LxI) = "THEN" | LxI = LxI + 1
LxKw(LxI) = "TIME" | LxI = LxI + 1
LxKw(LxI) = "TIMER" | LxI = LxI + 1
LxKw(LxI) = "TO" | LxI = LxI + 1
LxKw(LxI) = "TOFRONT" | LxI = LxI + 1
LxKw(LxI) = "TROFF" | LxI = LxI + 1
LxKw(LxI) = "TRON" | LxI = LxI + 1
LxKw(LxI) = "TRUE" | LxI = LxI + 1
LxKw(LxI) = "UCASE" | LxI = LxI + 1
LxKw(LxI) = "UNTIL" | LxI = LxI + 1
LxKw(LxI) = "VAL" | LxI = LxI + 1
LxKw(LxI) = "WAIT" | LxI = LxI + 1
LxKw(LxI) = "WEND" | LxI = LxI + 1
LxKw(LxI) = "WHERE" | LxI = LxI + 1
LxKw(LxI) = "WHILE" | LxI = LxI + 1
LxKw(LxI) = "WINDOW" | LxI = LxI + 1
LxKw(LxI) = "WINDOWCLOSE" | LxI = LxI + 1
LxKw(LxI) = "WINDOWHEIGHT" | LxI = LxI + 1
LxKw(LxI) = "WINDOWOUTPUT" | LxI = LxI + 1
LxKw(LxI) = "WINDOWTABS" | LxI = LxI + 1
LxKw(LxI) = "WINDOWWIDTH" | LxI = LxI + 1
LxKw(LxI) = "WLOCATE" | LxI = LxI + 1
LxKw(LxI) = "WPRINT" | LxI = LxI + 1
LxKw(LxI) = "WRITE" | LxI = LxI + 1
LxKw(LxI) = "XFREE" | LxI = LxI + 1
LxKw(LxI) = "XOR" | LxI = LxI + 1
LxKw(LxI) = "XSTACK" | LxI = LxI + 1

const LxKwCount% = LxI - 1

'' Globals
'' =======

LxLine% = 0		'' the line number

'' Subprograms
'' ===========

sub LxParse((L$), Tokens$(), (Count%)) static
	'' This subprogram should be defined in the client module.
	'' L contains the text of the first line of the statement.
	'' Tokens contains the tokens.
	'' Count is the number of tokens.

	'' Example LxParse:
	'' Just prints out each original line as a comment, followed by the
	'' tokens.

	'print "REM ", L
	'for I = 1 to Count
	'	print Tokens(I), " ";
	'next
	'print

	'' Simple quiet version for interpreter testing:
	shared TotalTokens
	TotalTokens = TotalTokens + Count
endsub

sub LxError((Msg$))
    '' This subprogram should be be defined in the client module.
    '' Typically it will print an error message, and discard the current
    '' input line by doing a RESUME into the loop which is reading lines of
    '' code.

    shared LxLine

    print "**** lex error on line", LxLine, ": ", Msg
    fail 21
endsub

sub LxReadFile((FileID%)) static
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

sub LxReadStatement((File%), TheLine$, SplitIndex%, Token$(), Count%) static
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

sub LxTokenise((Text$), Token$(), Count%, Finished?, SplitIndex%) static
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

sub LxSkipWhitespace((Text$), Idx%) static
	while mid(Text, Idx, 1) in LxWhitespace
		Idx = Idx + 1
	wend
endsub

sub LxGetNumber((Text$), Idx%, Token$) static
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
			MinimumLength = MinimumLength + 2
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

		if (C = "!" and (Octal or Hexadecimal)) _
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

sub LxCollectChars((Text$), (CharSet$), Token$, Idx%) static
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

RETURN '' return to client module
