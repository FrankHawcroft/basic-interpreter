rem
rem LISP.BAS
rem
rem Library of LISP-like functions for BASIC programs.
rem
rem Run this file to use it as an interactive interpreter.
rem
rem To use this library in a program include these statements at the start:
rem
rem  MERGE "lisp.bas"
rem  GOSUB L_LISP
rem
rem Functions and statements with names beginning with I_Lisp_ are internal to
rem the library.
rem

''
'' Interactive mode
'' ================
''

if executed then
	gosub L_LISP
	
	I_Lisp_Interactive = 1

	I_Lisp_MainLoop:

	NextLine$ = ""
	Result$ = ""
	on error call I_Lisp_InteractiveErrorHandler
	while NextLine <> "-"
		if NextLine <> "" then
			EvalStrStr NextLine, Result
			print Result
		endif
		lineinput "] ", NextLine
	wend
	'xfree
	end
endif

''
'' LISP types
'' ==========
''
'' LISP objects are represented by BASIC strings.  The advantage of this 
'' approach is its simplicity.  There are several disadvantages:
''
''	Speed -- extracting a sub-structure means a string allocation
''		and copy.
''	Size limit -- no object may be larger than the largest string, which
''		with this interpreter is usually 32767 bytes, unless built with the
''		QSTRING_LONG_STRINGS option (see: src/qstring.h). 
''	Possible misinterpretation -- the string can be treated as something 
''		else in a non-LISP-using part of the program.  Non-LISP strings
''		can accidentally be passed to these routines, causing errors.
''		(The function Validlispp(x$) can be used to test if a string
''		_looks_ like a LISP structure.)
''
'' Everything is either an atom or a pair.  
''
'' An atom is represented by a string of the form:
''
''	Atcccc...
''
''	The first character is capital "A".
''	The character t denotes the type and may be one of the following:
''		p	built-in procedure
''		m	built-in macro (i.e. syntax)
''		i	integer
''		s	symbol
''		b	Boolean
''	cccc... is the contents of the atom.  Its interpretation depends on the
''	type:
''		p	the procedure number (2 bytes)
''		m	the macro number (2 bytes)
''		i	a signed integer (2 bytes)
''		s	the symbol name (variable length)
''		b	a single byte, "T" or "F"
''
'' The shortest possible atom is "A", representing the null value ()
''
'' A pair is represented by a string of the form:
''
''	Pnnaaaa...dddd....
''
''	Capital "P" at the beginning tells us this is a pair.
''	nn is a 16-bit binary integer giving the offset in the pair of the CDR.
''		This offset is 1-relative and includes the first 3 bytes.
''	aaaa... is the CAR and can be an atom or a pair.
''	dddd... is the CDR and can be an atom or a pair.
''
'' The shortest possible pair is "PnnAA", a pair of nulls, where the "nn" part
'' would actually be the 16-bit word 0x0002.
''

rem
rem
rem L_LISP -- library entry point
rem
rem

L_LISP:

DEFINT a-z '' TODO shouldn't do this in a library ... add explicit types where necessary

''
'' General constants
'' =================
''

I_Lisp_Interactive% = 0
I_Lisp_ErrorCode% = 222	' A 'unique' error code

' TO DO: make use of these constants:

LsFalse% = 0
LsTrue% = not LsFalse

''
'' Type constants
'' ==============
''
'' I_Lisp_Proc_type
'' I_Lisp_Macro_type
'' I_Lisp_Int_type
'' I_Lisp_Symbol_type
'' I_Lisp_Boolean_type
''

const I_Lisp_Proc_type$ = "p"
const I_Lisp_Macro_type$ = "m"
const I_Lisp_Int_type$ = "i"
const I_Lisp_Symbol_type$ = "s"
const I_Lisp_Boolean_type$ = "b"

''
'' Constructors
'' ============
''
'' Null
'' Procedure
'' Macro
'' Integer
'' Symbol
'' Boolean
'' Cons
''

const Null$ = "A"
def I_Lisp_Atom$(type$, c$) as "A" + type + c
def Procedure$(c%) as I_Lisp_Atom(I_Lisp_Proc_type, mki(c))
def Macro$(c%) as I_Lisp_Atom(I_Lisp_Macro_type, mki(c))
def Integer$(c%) as I_Lisp_Atom(I_Lisp_Int_type, mki(c))

def Boolean$(c%) where c <> 0 as I_Lisp_Atom(I_Lisp_Boolean_type, "T")
def Boolean$(c%) as I_Lisp_Atom(I_Lisp_Boolean_type, "F")

def Symbol$(c$) as I_Lisp_Atom(I_Lisp_Symbol_type, c)

def Cons$(a$, d$) as "P" + mki(4 + len(a)) + a + d

''
'' Predicates
'' ==========
''
'' Nullp (Note that Nullp(x) --> Atomp(x))
'' Atomp 
'' Pairp
'' Procedurep
'' Macrop
'' Integerp
'' Symbolp
'' Booleanp
'' Eqp(x, y) -- true iff x and y are atoms and x = y.  A 'shallow' equality 
''		test.
'' Equalp(x, y) -- true iff x = y.  A 'deep' equality test.
''
'' NOTE: there is no Eqvp()
''

def Atomp(x$) as left(x, 1) = "A"
def Pairp(x$) as left(x, 1) = "P"
def Nullp(x$) as x = "A"
def Procedurep(x$) as Atomp(x) and mid(x, 2, 1) = I_Lisp_Proc_type
def Macrop(x$) as Atomp(x) and mid(x, 2, 1) = I_Lisp_Macro_type
def Integerp(x$) as Atomp(x) and mid(x, 2, 1) = I_Lisp_Int_type
def Symbolp(x$) as Atomp(x) and mid(x, 2, 1) = I_Lisp_Symbol_type
def Booleanp(x$) as Atomp(x) and mid(x, 2, 1) = I_Lisp_Boolean_type
def Eqp(x$, y$) as Atomp(x) and Atomp(y) and x = y
def Equalp(x$, y$) as x = y

''
'' I_Lisp_Quotep -- true iff x is a pair with car QUOTE
''

'const I_Lisp_QuotePrefix$ = Cons(Symbol("QUOTE"), Null)
'const I_Lisp_QPLength% = len(I_Lisp_QuotePrefix)
'def I_Lisp_Quotep(x$) as left(x, I_Lisp_QPLength) = I_Lisp_QuotePrefix

''
'' Accessors
'' =========
''
'' Integerval
'' Procedurenumber
'' Macronumber
'' Symbolname
'' Booleanval
'' Car
'' Cdr
'' SafeCar (accepts Null)
'' SafeCdr
''

def Integerval%(p$) as cvi(mid(p, 3, 2))
def Procedurenumber%(p$) as cvi(mid(p, 3, 2))
def Macronumber%(p$) as cvi(mid(p, 3, 2))
def Symbolname$(p$) as mid(p, 3, 32767)

def Booleanval%(p$) where mid(p, 3, 1) = "T" as 1
def Booleanval%(p$) as 0

def Car$(p$) as mid(p, 4, cvi(mid(p, 2, 2)) - 4)
def Cdr$(p$) as mid(p, cvi(mid(p, 2, 2)), 32767)

def SafeCar$(p$) where Nullp(p) as Null
def SafeCar$(p$) as Car(p)

def SafeCdr$(p$) where Nullp(p) as Null
def SafeCdr$(p$) as Cdr(p)

''
'' Validation
'' ==========
''
'' Validlispp(x) -- true iff x is a valid LISP object.
'' LsMaybeValidp(x) --	If true, x _might_ be a valid LISP object.
''			If false, x is _definitely_ not a valid LISP object.
''			This function is quicker than LsValidp.
''

def Validlispp(x$) where Nullp(x) as 1
def Validlispp(x$) where Atomp(x) as I_Lisp_TypeValid(mid(x, 2, 1))
def Validlispp(x$) where Pairp(x) and len(x) >= 5 as I_Lisp_CarCdrValid(x)
def Validlispp(x$) as 0

def I_Lisp_TypeValid(t$) where t = I_Lisp_Proc_type as 1
def I_Lisp_TypeValid(t$) where t = I_Lisp_Macro_type as 1
def I_Lisp_TypeValid(t$) where t = I_Lisp_Int_type as 1
def I_Lisp_TypeValid(t$) where t = I_Lisp_Symbol_type as 1
def I_Lisp_TypeValid(t$) where t = I_Lisp_Boolean_type as 1
def I_Lisp_TypeValid(t$) as 0

def I_Lisp_CarCdrValid(p$) as Validlispp(car(p)) and Validlispp(cdr(p))
 
def LsMaybeValidp(x$) where not (Atomp(x) or Pairp(x)) as LsFalse
def LsMaybeValidp(x$) where Pairp(x) and len(x) < 5 as LsFalse
def LsMaybeValidp(x$) where Atomp(x) as I_Lisp_TypeValid(mid(x, 2, 1))
def LsMaybeValidp(x$) as LsTrue

''
'' Conversion
'' ==========
''
'' StringToLisp
'' LispToString
'' LsPrint
''

const I_Lisp_MaxTokens = 80

sub StringToLisp((St$), Lisp$)
	' Tokenise the string:

	dim T$(I_Lisp_MaxTokens)
	I_Lisp_Tokenise St, T
	'I_Lisp_PrintTokens T	

	' Parse into the list:
	
	Lisp$ = ""
	if T(1) <> "" then
		I_Lisp_Build T, Lisp
	endif	
endsub

sub LispToString((Lisp$), St$)
	if Nullp(Lisp) then
		St$ = "()"
	elseif Atomp(Lisp) then
		if Integerp(Lisp) then
			St$ = str(Integerval(Lisp))
		elseif Booleanp(Lisp) then
			if Booleanval(Lisp) then
				St$ = "#T"
			else
				St$ = "#F"
			endif
		elseif Symbolp(Lisp) then
			St$ = Symbolname(Lisp)
		elseif Procedurep(Lisp) then
			ProcNum% = Procedurenumber(Lisp)
			N$ = str(MinArgCount(ProcNum))
			N$ = right(N, len(N) - 1)
			St$ = "#<PROC(" + N + ") " + ProcedureName(ProcNum)
			St$ = St + ">"
		elseif Macrop(Lisp) then
			ProcNum% = Macronumber(Lisp)
			St$ = "#<MACRO " + ProcedureName(ProcNum) + ">"
		else
			I_Lisp_Error "illegal object"
		endif
'	elseif I_Lisp_Quotep(Lisp) then
'		D$ = ""
'		LispToString Cdr(Lisp), D
'		St$ = "(QUOTE " + D + ")"
	else
		' Dotted pair or list

		St$ = "("
		Finished% = 0
		repeat
			First$ = car(Lisp)
			Rest$ = cdr(Lisp)
			A$ = ""
			LispToString First, A
			St = St + A + " "
			if Atomp(Rest) and not Nullp(Rest) then
				St = St + ". "
				A = ""
				LispToString Rest, A
				St = St + A + " "
				Finished = 1
			else
				Lisp = Rest
				Finished = Nullp(Rest)
			endif
		until Finished
		if right(St, 1) = " " then
			St = left(St, len(St) - 1)
		endif
		St = St + ")"
	endif
endsub

''
'' LsPrint -- print LISP.  Handy for debugging.
''

sub LsPrint((S$))
	V$ = ""
	LispToString S, V
	print V
endsub

''
'' I_Lisp_Tokenise -- tokenise a lisp expression
''
'' Special tokens that stand alone:
''	( . ) '
''
'' Blanks, tabs, newlines are ignored
'' An integer is a sequence of decimal digits __only__
'' A symbol is any other sequence of chars, converted to ucase.
''	(For the purposes of tokenisation, we can treat ints and syms the same)
''
'' In:
''	The expression E$
'' Result:
''	The array T is filled with tokens, terminated with an empty string ""

sub I_Lisp_Tokenise((E$), T$())

	const whitespace$ = " " + chr(9) + chr(10)
	const special$ = "(.)'"
	const separator$ = whitespace + special

	I% = 1		' next character
	N% = 0		' number of tokens
	length% = len(E)
	'print E; I
	while mid(E, I, 1) in whitespace
		I = I + 1
	wend
	while I <= length
		C$ = mid(E, I, 1)
		if C in special then
			N = N + 1
			I = I + 1
			T(N) = C
		else
			token$ = C
			I = I + 1
			C = mid(E, I, 1)
			while I <= length and not (C in separator)
				token = token + C
				I = I + 1
				C = mid(E, I, 1)
			wend
			N = N + 1
			T(N) = ucase(Token)
		endif
		while mid(E, I, 1) in whitespace
			I = I + 1
		wend
	wend
	N = N + 1
	T(N) = ""
endsub

''
'' I_Lisp_PrintTokens
''
''	Debugging subprogram.
''

sub I_Lisp_PrintTokens(T$())
	I% = 1
	while T(I) <> ""
		print T(I); "";
		I = I + 1
	wend
	print
endsub

''
'' I_Lisp_Build -- build list from tokens
''
''   Examples:
''
''	A dotted pair
''		( s . t )
''	translates to a cons with car s and cdr t
''
''	A list
''		( s t u )
''	translates to the following structure:
''		( s . ( t . ( u . () ) ) )
''	where () is a null list
''
'' In:
''	The tokens T$() in a ""-terminated array
'' Result:
''	The list L$

sub I_Lisp_Build(T$(), L$)
	N% = 1
	L = ""
	I_Lisp_Expr T, N, L
	'print t(n)
	if T(N) <> "" then
		I_Lisp_Error "illegal expression"
	endif
endsub

''
'' Expression Parsing
'' ================== 
''
'' This is done by recursive descent.
''
''	T	is the array of tokens
''	N	is the current position in the token array
''	L	is the output list
''

sub I_Lisp_Expr(T$(), N%, L$)
	select T(N)
	case ".", ")", ""
		I_Lisp_SyntaxError T(N)
	case "("
			' Rest of list
			' Could be empty, or list of expressions

		N = N + 1
		S$ = ""
		I_Lisp_Tail T, N, S
		L = S
	case "'"
		' Quoted expression
		N = N + 1
		S$ = ""
		I_Lisp_Expr T, N, S
		L = Cons(Procedure(m_Quote), Cons(S, Null))
	otherwise
		' Symbol, integer, ... --> atom

		I_Lisp_Item T(N), L
		N = N + 1 		' ???
	endselect
endsub

sub I_Lisp_Tail(T$(), N%, L$)
	select T(N)
	case ""
		I_Lisp_Error "mismatched ( )"
	case ")"
		N = N + 1
		L = Null
	case "."
		' Dotted pair.
		' The dot must be followed by _one_ expression.

		N = N + 1
		D$ = ""
		I_Lisp_Expr T, N, D
		I_Lisp_Match T, N, ")"
		L = D
	case "(", "'"
		' Nested list

		A$ = ""
		D$ = ""
		I_Lisp_Expr T, N, A
		I_Lisp_Tail T, N, D
		L = Cons(A, D)
	otherwise
		' Symbol, integer, ... --> atom

		I_Lisp_Item T(N), L
		N = N + 1 		' ???
		S$ = ""
		I_Lisp_Tail T, N, S
		L = Cons(L, S)
	endselect
endsub

''
'' I_Lisp_Item
''
''	T is the token, L is the resulting atom.
''

sub I_Lisp_Item((T$), L$)
	if left(T, 1) in "0123456789" then
		L = Integer(val(T))
	elseif T = "#F" then
		L = Boolean(0)
	elseif T = "#T" then
		L = Boolean(1)		
	else
		ProcNum% = BuiltinProcNum(T)

		if ProcNum <> -1 then
			if IsMacro(ProcNum) then
				L = Macro(ProcNum)
			else
				L = Procedure(ProcNum)
			endif
		else
			L = Symbol(T)
		endif
	endif
endsub

''
'' Expression Evaluation
'' =====================
''
''	Eval -- evaluate LISP S and return result in Res.
''	EvalStr -- evaluate expression (in readable form) and return result.
''	EvalStrStr -- evaluate readable expression, return readable result.
''

sub Eval((S$), Res$)
	const MaxArg = 20

	Res = ""
	if Nullp(S) or Integerp(S) or Booleanp(S) then
		'print "Null or integer, = " + S
		Res$ = S
	elseif Procedurep(S) or Macrop(S) then
		' Macros, procedures evaluate to themselves:
		Res$ = S
	elseif Symbolp(S) then
		' Symbols are assumed to be variables and evaluate to their
		' value; an error occurs if the symbol is not bound:

		lm_LookUpSymbol Symbolname(S), Res
		
		if Res = "" then
			I_Lisp_Error "symbol " + Symbolname(S) + " not bound"
		endif
		'Res$ = Symbol("#<SYM " + Symbolname(S) + ">")
	else
		' A pair -- a macro or procedure call

		' Evaluate car:

		A$ = Car(S)
		Eval A, A
		
		if not (Macrop(A) or Procedurep(A)) then
			I_Lisp_Error "attempt to call non-procedure"
		endif
		AMacro% = IsMacro(Procedurenumber(A))

		' If procedure, evaluate list of parameters, in cdr.
		' If macro, parameters are not evaluated.

		D$ = Cdr(S)
		I% = 0
		dim Arg$(MaxArg)
		if not AMacro then
			while not Nullp(D)
				I = I + 1
				Eval Car(D), Arg(I)
				D = Cdr(D)
			wend
		else
			while not Nullp(D)
				I = I + 1
				Arg(I) = Car(D)
				D = Cdr(D)
			wend
		endif

		' Apply evaluated car to list of arguments:

		I_Lisp_EvalProc Procedurenumber(A), Arg, I, Res
	endif
endsub

sub EvalStr((Expr$), Res$)
	S$ = ""
	StringToLisp Expr, S
	Eval S, Res
endsub

sub EvalStrStr((Expr$), Value$)
	'print "Entered evalstrstr"
	'print "expr = " + expr + ", value = " + value
	EvalStr Expr, Value
	'print "after evalstr expr = " + expr + ", value = " + value
	LispToString Value, Value
	'print "after lisptostring value = " + value
	'xfree
endsub

''
'' Macro and procedure numbers
''
'' Note that these don't overlap!
''

const m_Quote = 1
const m_If = 2
const m_Car = 3
const m_Cdr = 4
const m_Cons = 5
const m_Atom = 6
const m_Null = 7
const m_Length = 8
const m_Not = 9
const m_Plus = 10
const m_LessThan = 11
const m_Eq = 12
const m_Equal = 13
const m_List = 14
const m_Define = 15
const m_Set = 16

''
'' BuiltInProcNum
''

def BuiltInProcNum%(s$) where s = "QUOTE" as m_Quote
def BuiltInProcNum%(s$) where s = "IF" as m_If
def BuiltInProcNum%(s$) where s = "CAR" as m_Car
def BuiltInProcNum%(s$) where s = "CDR" as m_Cdr
def BuiltInProcNum%(s$) where s = "CONS" as m_Cons
def BuiltInProcNum%(s$) where s = "ATOM?" as m_Atom
def BuiltInProcNum%(s$) where s = "NULL?" as m_Null
def BuiltInProcNum%(s$) where s = "LENGTH" as m_Length
def BuiltInProcNum%(s$) where s = "NOT" as m_Not
def BuiltInProcNum%(s$) where s = "+" as m_Plus
def BuiltInProcNum%(s$) where s = "<" as m_LessThan
def BuiltInProcNum%(s$) where s = "EQ?" as m_Eq
def BuiltInProcNum%(s$) where s = "EQUAL?" as m_Equal
def BuiltInProcNum%(s$) where s = "LIST" as m_List
def BuiltInProcNum%(s$) where s = "DEFINE" as m_Define
def BuiltInProcNum%(s$) where s = "SET!" as m_Set
def BuiltInProcNum%(s$) as -1

''
'' ProcedureName
''

def ProcedureName$(p%) where p = m_Quote as "QUOTE"
def ProcedureName$(p%) where p = m_If as "IF"
def ProcedureName$(p%) where p = m_Car as "CAR"
def ProcedureName$(p%) where p = m_Cdr as "CDR"
def ProcedureName$(p%) where p = m_Cons as "CONS"
def ProcedureName$(p%) where p = m_Atom as "ATOM?"
def ProcedureName$(p%) where p = m_Null as "NULL?"
def ProcedureName$(p%) where p = m_Length as "LENGTH"
def ProcedureName$(p%) where p = m_Not as "NOT"
def ProcedureName$(p%) where p = m_Plus as "+"
def ProcedureName$(p%) where p = m_LessThan as "<"
def ProcedureName$(p%) where p = m_Eq as "EQ?"
def ProcedureName$(p%) where p = m_Equal as "EQUAL?"
def ProcedureName$(p%) where p = m_List as "LIST"
def ProcedureName$(p%) where p = m_Define as "DEFINE"
def ProcedureName$(p%) where p = m_Set as "SET!"
'' default: error, unknown proc number

''
'' IsMacro
''

def IsMacro%(p%) as p = m_Quote or p = m_If or p = m_Define or p = m_Set

''
'' MinArgCount
''
'' Default is 1
'' 

def MinArgCount(p%) where p = m_If or p = m_Cons or p = m_LessThan as 2
def MinArgCount(p%) where p = m_Eq or p = m_Equal or p = m_Set as 2
def MinArgCount(p%) where p = m_List as 0
def MinArgCount(p%) as 1

''
'' MaxArgCount
''
'' Default is 1
''

def MaxArgCount(p%) where p = m_Cons or p = m_LessThan or p = m_Set as 2
def MaxArgCount(p%) where p = m_Eq or p = m_Equal or p = m_Define as 2
def MaxArgCount(p%) where p = m_If as 3
def MaxArgCount(p%) where p = m_Plus or p = m_List as 32767
def MaxArgCount(p%) as 1

'' I_Lisp_Macrop
''
'' True iff the string is a macro name.  Assumed to be in ucase already.
''

def I_Lisp_Macrop(x$) where x = "QUOTE" as 1
def I_Lisp_Macrop(x$) where x = "IF" as 1
def I_Lisp_Macrop(x$) where x = "DEFINE" as 1
def I_Lisp_Macrop(x$) where x = "SET!" as 1
def I_Lisp_Macrop(x$) as 0

''
'' I_Lisp_EvalProc
''
'' Evaluate a built-in procedure or macro.
''
'' NOTE: may change elements of Arg()
''

sub I_Lisp_EvalProc((Proc%), Arg$(), (Count%), Res$)
	'' Check argument count:

	I_Lisp_ArgCount Count, MinArgCount(Proc), MaxArgCount(Proc)

	select Proc
	case m_Quote
		' *** MACRO ***
		' 'P --> P 
		' (QUOTE P) --> P

		Res = Arg(1)
	case m_If
		' *** MACRO ***
		' (IF P TrueClause {FalseClause}) --> result of TrueClause, if
		'  	P is #T, otherwise result of FalseClause if it is
		'	present, or () if no FalseClause.

		Pred$ = ""
		Eval Arg(1), Pred
		I_Lisp_AssertBoolean Pred
		if Booleanval(Pred) then
			Eval Arg(2), Res
		elseif Count = 3 then
			Eval Arg(3), Res
		else
			Res = Null
		endif
	case m_Car
		' (CAR P) --> first element of pair P

		I_Lisp_AssertPairOrNull Arg(1)
		Res = SafeCar(Arg(1))
	case m_Cdr
		' (CDR P) --> second element of pair P

		I_Lisp_AssertPairOrNull Arg(1)
		Res = SafeCdr(Arg(1))
	case m_Cons
		' (CONS A D) --> pair with car A and cdr D

		Res = Cons(Arg(1), Arg(2))
	case m_Atom
		' (ATOM? X) --> TRUE iff X is an atom

		Res = Boolean(Atomp(Arg(1)))
	case m_Null
		' (NULL? X) --> TRUE iff X is null

		Res = Boolean(Nullp(Arg(1)))
	case m_Length
		' (LENGTH P) --> length of list P

		I_Lisp_AssertPairOrNull Arg(1)
		TheLength% = 0
		while not Atomp(Arg(1))
			Arg(1) = Cdr(Arg(1))
			TheLength = TheLength + 1
		wend
		Res = Integer(TheLength)
	case m_Not
		' (NOT B) --> #T iff B = #F
		' In this interpreter, #F does not equal ()

		I_Lisp_AssertBoolean Arg(1)
		Res = Boolean(not Booleanval(Arg(1)))
	case m_Plus
		' (+ I1 I2 ... In) --> sum of I1 ... In
		' For _integer_ addition only!
		
		Vi% = 0
		for I = 1 to Count
			I_Lisp_AssertInteger Arg(I)
			Vi = Vi + Integerval(Arg(I))
		next
		Res = Integer(Vi)
	case m_LessThan
		' (< I1 I2) --> #T iff I1 < I2
		' For _integer_ comparison only!
		
		I_Lisp_AssertInteger Arg(1)
		I_Lisp_AssertInteger Arg(2)
		Res = Boolean(Integerval(Arg(1)) < Integerval(Arg(2)))
	case m_Eq
		' (EQ? O1 O2) --> #T iff O1 and O2 represent the same symbol,
		'		integer, or Boolean value.
		' The 'shallowest' equality test.

		Res = Boolean(Eqp(Arg(1), Arg(2)))		
	case m_Equal
		' (EQUAL? O1 O2) --> #T iff O1 equals O2
		' This is a "deep" equality comparison.

		Res = Boolean(Equalp(Arg(1), Arg(2)))
	case m_List
		' (LIST A B C ... ) --> list containing elements A, B, C etc.

		Res = Null
		for I = Count to 1 step -1
			Res = Cons(Arg(I), Res)
		next
	case m_Define
		' *** MACRO ***
		' (DEFINE Name {Value}) --> Name
		' Define or modify a global variable Name with value Value.  
		' The default Value is ()
		' Yields the symbol Name.  (Standard behaviour??)

		I_Lisp_AssertSymbol Arg(1)
		SymName$ = Symbolname(Arg(1))
		if Count = 2 then
			Eval Arg(2), Res
		else
			Res = Null
		endif
		SymIndex% = 0
		lm_LookUpSymbolIndex SymName, SymIndex
		if SymIndex = 0 then
			lm_AddNewSymbol SymName, Res
		else
			lm_SetSymbol SymIndex, Res
		endif
		Res = Arg(1)
	case m_Set
		' *** MACRO ***
		' (SET! Name Value) --> Name
		' Set a global variable Name to the value Value.  
		' Unlike DEFINE, the variable must exist or an error occurs.
		' Yields the symbol Name.  (Standard behaviour??)

		I_Lisp_AssertSymbol Arg(1)
		SymName$ = Symbolname(Arg(1))
		Eval Arg(2), Res
		SymIndex% = 0
		lm_LookUpSymbolIndex SymName, SymIndex
		if SymIndex = 0 then
			I_Lisp_Error "undefined variable " + SymName
		else
			lm_SetSymbol SymIndex, Res
		endif
		Res = Arg(1)
	'case m_Lambda
	'	' *** MACRO ***
	'	' (LAMBDA (A1 A2 ...) Expr) --> procedure with params 
	'	'				(A1 A2 ...)
	'
	'	ProcObject$ = ""
	'
	'	I_Lisp_AssertPairOrNull Arg(1)
	'	PL$ = Arg(1)
	'	PCount% = 0
	'	while not Nullp(PL)
	'		PName$ = Car(PL)
	'		I_List_AssertSymbol PName
	'		PName = Symbolname(PName)
	'		ProcObject = ProcObject + mki(len(PName)) + PName
	'		PCount = PCount + 1
	'		PL = Cdr(PL)
	'	wend

	'	ProcObject = mki(PCount) + ProcObject
	'	CodeOffset% = 2 + len(ProcObject)
	'	ProcObject = mki(CodeOffset) + ProcObject + Arg(2)
	'	Res = Userproc(ProcObject)
	otherwise
		I_Lisp_Error "[internal] undefined procedure no. " + str(Proc)
	endselect
endsub

''
'' I_Lisp_ArgCount -- check procedure argument count against bounds.
''	Fail with an error if it's outside them.
''

sub I_Lisp_ArgCount((Count%), (Min%), (Max%))
	if Count < Min or Count > Max then
		Msg$ = "procedure expects"
		if Min = Max then
			Msg = Msg + str(Min)
		else
			Msg = Msg + str(Min) + " to" + str(Max) 
		endif
		Msg = Msg + " parameter(s)"
		I_Lisp_Error Msg
	endif
endsub

''
'' Symbol table management
'' =======================
''

'' The symbol table:

const lm_MaxSymbols = 20

dim lm_SymName$(lm_MaxSymbols + 1)
'' last entry is reserved
dim lm_SymValue$(lm_MaxSymbols + 1)
let lm_SymCount = 0
let lm_SymValue(lm_MaxSymbols + 1) = ""

sub lm_LookUpSymbol((TheName$), Value$)
	shared lm_SymValue()

	Value$ = ""
	SymIndex% = 0
	lm_LookUpSymbolIndex TheName, SymIndex
	if SymIndex <> 0 then
		Value = lm_SymValue(SymIndex)
	endif
endsub

'' POST: index will be 0 if TheName not found, else a valid symbol index.

sub lm_LookUpSymbolIndex((TheName$), Index%)
	shared lm_SymName(), lm_SymCount

	Index% = 0

	'' Set up sentinel:
	''  All names are assumed to already be in ucase.
	lm_SymName(lm_SymCount + 1) = TheName

	'' Find the name:

	I% = 0
	repeat
		I = I + 1
	until lm_SymName(I) = TheName

	if I <= lm_SymCount then
		Index = I
	endif
endsub

'' PRE: TheName is known not to exist, and already in ucase.

sub lm_AddNewSymbol((TheName$), (Value$))
	shared lm_SymName(), lm_SymValue(), lm_SymCount

	'' TO DO: nice check for array index out of bounds.
	lm_SymCount = lm_SymCount + 1
	lm_SymName(lm_SymCount) = TheName
	lm_SymValue(lm_SymCount) = Value
endsub

sub lm_SetSymbol(Index%, (Value$))
	shared lm_SymValue()

	lm_SymValue(Index) = Value
endsub

''
'' I_Lisp_AssertPairOrNull
''
'' Cause an error if the LISP is a non-null atom
''

sub I_Lisp_AssertPairOrNull((L$))
	if not (Pairp(L) or Nullp(L)) then
		I_Lisp_Error "expected a pair or null"
	endif
endsub

''
'' I_Lisp_AssertInteger
'' 
'' Cause an error if not integer.
''

sub I_Lisp_AssertInteger((L$))
	if not Integerp(L) then
		I_Lisp_Error "expected an integer"
	endif
endsub

''
'' I_Lisp_AssertBoolean
''
'' Cause an error if not Boolean.
''

sub I_Lisp_AssertBoolean((L$))
	if not Booleanp(L) then
		I_Lisp_Error "expected a Boolean"
	endif
endsub

''
'' I_Lisp_AssertSymbol
''
'' Cause an error if not a symbol.
''

sub I_Lisp_AssertSymbol((L$))
	if not Symbolp(L) then
		I_Lisp_Error "expected a symbol"
	endif
endsub

''
'' Error handling
'' ==============
''
'' When an error occurs, we drop out to the top-level loop if in interactive
'' mode; otherwise, we abort.
''

sub I_Lisp_Match(T$(), N, (Token$))
	if T(N) <> Token then
		I_Lisp_Error "expected " + Token + " got " + T(N)
	endif
	N = N + 1
endsub

sub I_Lisp_InteractiveErrorHandler
	print "(resuming at top-level)"
	resume I_Lisp_MainLoop
endsub

sub I_Lisp_Error((Message$))
	shared I_Lisp_Interactive, I_Lisp_ErrorCode

	print "Error: ", Message
	if I_Lisp_Interactive then
		on error call I_Lisp_InteractiveErrorHandler
		error I_Lisp_ErrorCode
	else
		error 240
		'' end
	endif
endsub

sub I_Lisp_SyntaxError((Token$))
	if Token = "" then
		I_Lisp_Error "unexpected end of expression"
	else
		I_Lisp_Error "unexpected " + Token
	endif
endsub

RETURN 

' End of L_LISP

' End of LISP.BAS
