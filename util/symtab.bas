'' Build a symbol table for a BASIC program.

'' TO DO: types still not reliably assigned.
'' Need to give vars a type when defined unambiguously, but not if they might
'' get it from elsewhere (in SHARED etc.) Perhaps add another pass which assigns
'' default types to local vars which haven't yet got them? (Can't do to globals,
'' as they might be externally defined.) Consider: 'I' in CheckConsistent vs
'' shared vars vs global vars w/o type specs.

'' Symbols:
''	Variables
''		- global vs local
''			- SHARED
''		- parameters
''			- reference
''			- value
''			- DEF or SUB
''		- arrays
''		- explicitly typed or not
''		- constants
''	Labels
''		- defined or just referenced (GOTO, GOSUB), due to a MERGE stmt.
''		- note RESUME -- subprogram containing the target isn't known
''	Subprograms
''		- also may be defined or just referenced
''	Functions
''		- ditto, defined or refd
''		- return type

'' Tested with:
'' symtab.bas -- fixed prob with implied assignment stmts
'' accounts.bas -- allow redeclaration of fcn params as hack to enable multi-part fcns
'' lexer.bas -- OK
'' lisp.bas -- incr table size to 250
'' metamake.bas -- OK

'' Data for each symbol:
'' - name
'' - context: subprogram or fcn name, global, or unknown.
'' - kind: variable, label, subprogram, function
'' - type (not labels, subs): or unknown/undetermined
'' - attributes: constant, parameter (ref or value or fcn??), array, shared?

'' Usage of symbols:
''	In fcn expression(s): var, fcn
''	In ON: sub
''	In GOSUB, GOTO: label in same context (can be external if global)
''	In RESUME: label in _any_ context
''	Otherwise (except in decls, lists in DEF, SHARED, SUB, LOCAL): var, fcn

merge "lexer.bas" | gosub Lx

'' Symbol kinds:

const K_Variable@ = "V"
const K_Subprogram@ = "S"
const K_Function@ = "F"
const K_Label@ = "L"

'' Symbol attributes -- all except A_External apply only to variables.
'' A_External can apply to any kind of symbol; in fact, the symbol can have
'' more than one possible kind if it is only referred to and not defined.
'' E.g. in the statement
''	x = f
'' where 'f' is not defined in this source file, it could be a function or a
'' variable.

const A_Const@ = "c"			'' declared CONST
const A_ExplicitlyTyped@ = "e"	'' type is definite
const A_Array@ = "a"			'' an array
const A_FcnParameter@ = "f"		'' function parameter
const A_ValParameter@ = "v"		'' value parameter (SUB)
const A_RefParameter@ = "r"		'' reference parameter (SUB)
const A_Shared@ = "s"			'' SHARED variable
const A_External@ = "x"			'' not defined in this file

const MaxIdent = 500

dim IdName$(MaxIdent)	'' the symbol
dim IdCtxt$(MaxIdent)	'' name of containing subprogram/fcn, or "" if global
dim IdKind$(MaxIdent)	'' kind(s) -- could be more than one, if external
dim IdType$(MaxIdent)	'' type, if variable or function, otherwise ""
dim IdAttr$(MaxIdent)	'' attributes (see above)
IdCount% = 0

const MaxRef = 250

dim RfName$(MaxRef)
dim RfCtxt$(MaxRef)	'' name of containing subprogram/fcn
dim RfKind$(MaxRef)	'' possible kind(s)
dim RfType$(MaxRef)	'' if explicit type spec; "" otherwise
dim RfLine%(MaxRef)	'' line number of the _first_ reference
RfCount% = 0

'' The name of the current subprogram or function, "" if none
Context$ = ""

'' File IDs:
const InFID = 1
const OutFID = 2

def IsIdentifier?(T$) as left(T, 1) in LxLetter

def IsSubParameter?(Attrs$) as A_RefParameter in Attrs or A_ValParameter in Attrs

def PStr$(n%) where n >= 0 as mid2(str(n), 2)
def PStr$(n%) as str(n)
			 
sub LxParse((TheLine$), Tok$(), (Count))
	shared Context

	'' 1. Create any symbol definitions from the statement.
	'' 2. Check usage of symbols in the statement. It is possible that
	'' some symbols will not actually be defined in this source file --
	'' just referred to. In that case, tentative definitions are made.
	'' Usage of external symbols is still checked, to some extent.

	'' 1. Create definitions.

	StartIdx% = 1

	if StartIdx > Count then EXITSUB

	if IsIdentifier(Tok(1)) and Tok(2) = ":" then
		Define Tok(1), K_Label, "", ""
		StartIdx = 3
	endif

	if StartIdx > Count then EXITSUB

	T$ = Tok(StartIdx)
	TN$ = Tok(StartIdx + 1)
	TNN$ = Tok(StartIdx + 2)
	TNNN$ = Tok(StartIdx + 3)

	if T = "CONST" then
		DefineMaybeTyped TN, TNN, K_Variable, A_Const, false
		StartIdx = StartIdx + 3
	elseif T = "DEF" then
		'' note: fcn may be 'defined' multiple times so long as type
		'' and types of its parameters are the same.  This is to allow
		'' piecewise functions.

		DefineMaybeTyped TN, TNN, K_Function, "", false
		'' not allowed in sub -- this check is done in Define

		Context = TN
		I% = StartIdx + 2
		while I <= Count and Tok(I) <> "AS" and Tok(I) <> "WHERE"
			if IsIdentifier(Tok(I)) then
				DefineMaybeTyped Tok(I), Tok(I + 1), _
					K_Variable, A_FcnParameter, false
			endif
			I = I + 1
		wend
		StartIdx = I + 1

		'' to do: check for identical param lists in all decls.
	elseif T = "DIM" then
		DefineMaybeTyped TN, TNN, K_Variable, A_Array, false
		StartIdx = StartIdx + 3
	elseif T = "ENDSUB" then
		if Context = "" then
			SymbolError "ENDSUB not in SUB"
		endif
		Context = ""
	elseif T = "FOR" then
		'' note: could be subscripted array var.  Examples:
		'' 	for i = 1 to 5
		''	for a(x) = 1 to 5
		''	for a%(x) = 1 to 5

		if TNN <> "(" and TNNN <> "(" then
			DefineMaybeTyped TN, TNN, K_Variable, "", true
			StartIdx = StartIdx + 3
		else
			'' Cannot be a definition.  An array variable must
			'' be declared by a DIM statement, or as a parameter or
			'' SHARED variable.
			
			StartIdx = StartIdx + 1		'' just skip FOR
		endif
	elseif T = "LET" then
		'' note: could be subscripted array var.

		if TNN <> "(" and TNNN <> "(" then
			DefineMaybeTyped TN, TNN, K_Variable, "", true
		endif
		
		'' Otherwise, cannot be a definition.  An array variable must
		'' be declared by a DIM statement, or as a parameter or
		'' SHARED variable.

		StartIdx = StartIdx + 1
	elseif T = "LOCAL" then
		for I = StartIdx + 1 to Count
			if IsIdentifier(Tok(I)) then
				DefineMaybeTyped Tok(I), Tok(I + 1), _
				  K_Variable, "", false
			endif
		next
		StartIdx = Count + 1
	elseif T = "SHARED" then
		'' to do: vars, possibly arrays, global, not in current sub.
		'' Names can't conflict with local vars, params etc.
		'' This is a tricky one to manage.  Ideally I want a way to
		'' have multiple contexts on a variable.  Actually, is there
		'' any reason not to do that?

		'' Can only be in SUB -- checked in Define

		for I = StartIdx + 1 to Count
			if IsIdentifier(Tok(I)) then
				Attributes$ = ""
				if I < Count _
				and (Tok(I + 1) = "(" or Tok(I + 2) = "(") then
					Attributes$ = A_Array
				endif
				DefineMaybeTyped Tok(I), Tok(I + 1), _
				  K_Variable, Attributes + A_Shared, true
			endif
		next
		StartIdx = Count + 1
	elseif T = "SUB" then
		'' to do: define sub.  Define params, which can be arrays, etc.
		'' All sorts of things to do if completely general ... CONST
		'' dimensions, variable dimensions.  Value vs. reference.

		'' check not in SUB -- done by Define

		Define TN, K_Subprogram, "", ""
		Context = TN
		
		I% = StartIdx + 3
		while I < Count
			CT$ = Tok(I)
			if CT = "(" then
				I = I + 1	'' skip (
				DefineMaybeTyped Tok(I), Tok(I + 1), _
				  K_Variable, A_ValParameter, false
			elseif IsIdentifier(CT) then
				'' to do: complicated array param stuff.
				'' Currently anything inside the ( ) is ignored

				if Tok(I + 1) = "(" or Tok(I + 2) = "(" then
					DefineMaybeTyped CT, Tok(I + 1), _
					  K_Variable, _
					  A_RefParameter + A_Array, false
					while I < Count and Tok(I) <> ")"
						I = I + 1
					wend
				else
					DefineMaybeTyped CT, Tok(I + 1), _
					  K_Variable, A_RefParameter, false
				endif
			endif
			I = I + 1
		wend

		'' to do: syntax checks? (e.g. closing paren at end?)

		StartIdx = Count + 1
		'' to do: and no need to process anything else ...
	elseif Count >= StartIdx + 3 then
		'' Check for an implied LET statement.
		'' note: could be subscripted array var.

		if left(TN, 1) in LxTypeSpec and TNN <> "(" then
			DefineMaybeTyped T, TN, K_Variable, "", false
			StartIdx = StartIdx + 1
		endif
	endif

	'' 2. Check usage and record references to undefined symbols for
	'' checking later.

	if T = "GOSUB" or T = "GOTO" then
		Reference TN, TNN, "L"
	elseif T = "ON" then
		BeforeCall? = true
		for I = StartIdx + 1 to Count
			if Tok(I) = "CALL" then
				BeforeCall = false
			elseif BeforeCall then
				Reference Tok(I), Tok(I + 1), "VF"
			else
				Reference Tok(I), Tok(I + 1), "S"
			endif
		next
	elseif T = "RESUME" then
		'' to do: think about this more
		Reference TN, TNN, "L"
	else
		if StartIdx <= 3 then
			'' Stmt of form
			''	x: y ...
			'' or
			''	y <args>
			'' or
			''	x = ...
			'' etc.

			Reference Tok(StartIdx), Tok(StartIdx + 1), "VS"
			StartIdx = StartIdx + 1
		endif

		for I = StartIdx to Count
			Reference Tok(I), Tok(I + 1), "VF"
		next
	endif

	'' to do: in compiler, will happen in pass 2

	'if T = "GOSUB" or T = "GOTO" then
	'	Mention TN, K_Label, "", ""
	'elseif T = "RESUME" then
	'	'' Create a special label entry with unknown context.
	'
	'	SavedContext$ = Context
	'	Context = "?"
	'	Mention TN, K_Label, "", ""
	'	Context = SavedContext
	'elseif T = "ON" then
	'	'' to do: mentions sub after CALL keyword.
	'else
	'
	'endif

	if T = "DEF" then
		Context = ""
	endif
endsub

sub LxError((Msg$))
	shared LxLine

	'' to do: error recovery
	print argv(1); "("; PStr(LxLine); "): lex error - "; Msg
	fail 1
endsub

sub SymbolError((Msg$))
	shared LxLine

	'' to do: error recovery
	print argv(1); "("; PStr(LxLine); "): error - "; Msg
	fail 1
endsub

sub Reference((PossIdent$), (PossTypeSpec$), (Kinds$))
	shared LxLine
	shared Context
	shared RfName(), RfCtxt(), RfKind(), RfType(), RfLine(), RfCount

	'' Unlike Define, this is called on (almost) every token in the file.
	'' (Tokens passed to Define being the obvious exception.)

	'' Check we haven't run off the end of the token array.
	'' I put this check in because it is easier than adding checks to all
	'' the TN, TNN, etc. in LxParse

	if PossIdent = "" then EXITSUB

	'' Only interested in the token if it is in fact an identifier.

	if not IsIdentifier(PossIdent) then EXITSUB

	'' And if it isn't a keyword.  We don't check syntax!

	KW? = false
	LxGetIsKeyword PossIdent, KW
	if KW then EXITSUB

	'' Now, hopefully most of the time symbols will be defined earlier in
	'' the file than they are first referenced.  With GOTO and MERGE
	'' this is not always the case.  It is fine to refer to a symbol defined
	'' later in the file, or in a different file.  The symbol table should
	'' include these symbols.  Each is recorded the first time it is 
	'' encountered; future uses are checked against the first one for
	'' consistency.  After the whole file has been scanned, the
	'' references are checked against the definitions.  References which
	'' still have no definition are added to the symbol table, but tagged
	'' as externally-defined.

	if not left(PossTypeSpec, 1) in LxTypeSpec then
		PossTypeSpec = ""
	endif

	ActualKind$ = ""
	ActualType$ = ""
	ActualAttrs$ = ""
	FullLookUp PossIdent, Context, ActualKind, ActualType, ActualAttrs

	if ActualKind <> "" then
		CheckConsistent PossIdent, LxLine, Kinds, PossTypeSpec, _
		  ActualKind, ActualType, ActualAttrs, true
	else
		'' Add a forward-reference record for this usage of the symbol.

		RfCount = RfCount + 1
		'' Sentinel:
		RfName(RfCount) = PossIdent
		RfCtxt(RfCount) = Context

		I% = 0
		repeat
			I = I + 1
		until RfName(I) = PossIdent and RfCtxt(I) = Context

		if I = RfCount then 
			RfKind(RfCount) = Kinds
			RfType(RfCount) = PossTypeSpec
			RfLine(RfCount) = LxLine
		else
			RfCount = RfCount - 1
			CheckConsistent PossIdent, LxLine, Kinds, _
			  PossTypeSpec, RfKind(I), RfType(I), "", false
		endif
	endif
endsub

sub CheckConsistent((RefId$), (RefLine), (RefKinds$), (RefType$), _
	(ActualKinds$), (ActualType$), (ActualAttrs$), (ComparingAgainstDefn?))

	shared LxLine

	if ComparingAgainstDefn then
		Earlier$ = "definition"
	else
		Earlier$ = "earlier usage"
	endif

	KindsMatch? = false
	for I = 1 to len(RefKinds)
		if mid(RefKinds, I, 1) in ActualKinds then
			KindsMatch = true
		endif
	next

	if not KindsMatch then
		'' Fake line number in error msg:
		LxLine = RefLine
		SymbolError "usage of " + RefId + " inconsistent with " _
		  + Earlier
	endif

	if A_ExplicitlyTyped in ActualAttrs _
	and RefType <> "" and RefType <> ActualType then
		LxLine = RefLine
		SymbolError RefId + RefType + " was declared " + ActualType
	endif
endsub

sub DefineMaybeTyped((Ident$), (PossType$), (Kind$), (Attrs$), (ChkImplicit?))
	if left(PossType, 1) in LxTypeSpec then
		Define Ident, Kind, PossType, Attrs + A_ExplicitlyTyped
	elseif ChkImplicit then
		Define Ident, Kind, "", Attrs
	else
		Define Ident, Kind, "", Attrs + A_ExplicitlyTyped
	endif
endsub

sub Define((Ident$), (Kind$), (Type$), (Attrs$))
	shared Context

	'' Check for a keyword.

	KW? = false
	LxGetIsKeyword Ident, KW
	if KW then
		SymbolError Ident + " is a reserved word"
	endif

	'' Check for nested definitions of subprograms or functions.

	if (Kind = K_Function or Kind = K_Subprogram) and Context <> "" then
		SymbolError "DEF and SUB are not allowed inside SUB"
	endif

	'' Check for use of SHARED outside a subprogram.

	if A_Shared in Attrs and Context = "" then
		SymbolError "SHARED is only allowed inside a subprogram"
	endif

	'' Check for an erroneous type specifier.

	if (Kind = K_Label or Kind = K_Subprogram) and Type <> "" then
		SymbolError Ident + " cannot have a type specifier"
	endif

	'' See if the symbol has previously been defined.

	ActualKind$ = ""
	ActualType$ = ""
	ActualAttrs$ = ""
	LookUp Ident, Context, ActualKind, ActualType, ActualAttrs

	'' If this is genuinely a definition, create a symtab entry for it.

	if ActualKind = "" then
		if (Kind = K_Variable or Kind = K_Function) _
		and Type = "" and A_ExplicitlyTyped in Attrs then
			Type = "%"
		endif

		AddSymbol Ident, Context, Kind, Type, Attrs ''''+ A_Internal
		EXITSUB
	endif

	'' Otherwise, this is the second 'definition' of the symbol in the
	'' current context.  Because the LET and FOR statements may be used to
	'' define variables as well as assigning to previously defined 
	'' variables, it is OK to 'define' the same variable several times.  
	'' Because functions may be defined piecewise, it is OK to 'define' 
	'' the function several times.
	'' For labels and subprograms, though, only one definition is allowed.

	'' Check for duplicate definitions.  In my BASIC, everything lives in 
	'' the same namespace.

	if Kind <> ActualKind or Kind = K_Subprogram or Kind = K_Label then
		print Kind, ActualKind
		SymbolError "redefinition of " + Ident
	endif

	'' More specific redefinition errors:

	'' to do: the following won't detect
	''	def foo(bar, bar) as ...
	'' because want to allow
	''	def foo(bar) where ... as ...
	''	def foo(bar) where ... as ...

	if IsSubParameter(Attrs) and IsSubParameter(ActualAttrs) then
		SymbolError "duplicate parameter name " + Ident
	endif

	if A_Shared in Attrs then
		SymbolError Ident _
		  + " is already defined as a local variable or parameter"
	endif

	if A_Const in ActualAttrs then
		SymbolError "redefinition of constant " + Ident
	endif

	if A_Array in ActualAttrs then
		SymbolError "redefinition of array " + Ident
	endif

	if A_Const in Attrs then
		SymbolError "redefinition of " + Ident + " as constant"
	endif

	if A_Array in Attrs then
		SymbolError "redefinition of " + Ident + " as array"
	endif

	'' Type checking.

	if Type <> "" then
		'' Cases like this must be allowed for:
		''
		'' gosub DefineVar
		'' let x = 5		'' implicitly defined integer
		'' ...
		'' DefineVar:
		'' x! = 0		'' explicitly defined floating
		'' return 

		if not (A_ExplicitlyTyped in ActualAttrs) then 
			Redefine Ident, Context, Kind, Type, _
			  ActualAttrs + A_ExplicitlyTyped
		elseif ActualType <> Type then
			SymbolError "type mismatch for " + Ident _
			  + ": defined as " + ActualType + " not " + Type
		endif
	endif
endsub

sub Mention((Ident$), (KindSet$), (Type$))
	shared Context

	ActualKind$ = ""
	ActualType$ = ""
	ActualAttrs$ = ""
	LookUp Ident, Context, ActualKind, ActualType, ActualAttrs

	if ActualKind <> "" then
		'' Check that this usage is consistent with the definition.

		if not ActualKind in KindSet then
			SymbolError "inconsistent usage of " + Ident
		endif

		if Type <> "" and Type <> ActualType then
			SymbolError "type mismatch for " + Ident _
			  + ": defined as " + ActualType + " not " + Type
		endif

		'' Don't add a definition to the external symbols list.
	else
		
	endif
endsub

sub AddSymbol((Ident$), (Context$), (Kind$), (Type$), (Attrs$))
	'' pre: it is guaranteed that the symbol is not already defined.

	shared IdName(), IdCtxt(), IdKind(), IdType(), IdAttr(), IdCount

	IdCount = IdCount + 1
	IdName(IdCount) = Ident
	IdCtxt(IdCount) = Context
	IdKind(IdCount) = Kind
	IdType(IdCount) = Type
	IdAttr(IdCount) = Attrs
endsub

sub Find((Ident$), (Context$), Idx)
	shared IdName(), IdCtxt(), IdCount

	Idx% = 0
	for I = 1 to IdCount
		if IdName(I) = Ident and IdCtxt(I) = Context then
			Idx = I
			EXITSUB
		endif
	next
endsub

sub LookUp((Ident$), (Context$), Kind$, Type$, Attrs$)
	shared IdKind(), IdType(), IdAttr()

	Idx% = 0
	Find Ident, Context, Idx
	if Idx > 0 then
		Kind = IdKind(Idx)
		Type = IdType(Idx)
		Attrs = IdAttr(Idx)
	else
		Kind = ""
		Type = ""
		Attrs = ""
	endif
endsub

sub FullLookUp((Ident$), (Context$), Kind$, Type$, Attrs$)
	'' Performs a symbol lookup as it is actually done in the interpreter.
	'' Namely, in exception to the global/local distinction:
	'' -- globally-scoped constants are visible everywhere.
	'' -- subprograms and functions are visible everywhere.

	LookUp Ident, Context, Kind, Type, Attrs

	if Kind = "" and Context <> "" then
		LookUp Ident, "", Kind, Type, Attrs
		if Kind <> "" then
			if not ((Kind = K_Variable and A_Const in Attrs) _
			or Kind = K_Function or Kind = K_Subprogram) then
				Kind = ""
				Type = ""
				Attrs = ""
			endif
		endif
	endif
endsub

sub Redefine((Ident$), (Context$), (Kind$), (Type$), (Attrs$))
	'' pre: it is guaranteed that the symbol is already defined

	shared IdName(), IdCtxt(), IdKind(), IdType(), IdAttr()

	Idx% = 0
	Find Ident, Context, Idx
	IdName(Idx) = Ident
	IdCtxt(Idx) = Context
	IdKind(Idx) = Kind
	IdType(Idx) = Type
	IdAttr(Idx) = Attrs
endsub

sub HandleForwardReferences
	shared RfName(), RfCtxt(), RfKind(), RfType(), RfLine(), RfCount

	for I = 1 to RfCount
		ActualKind$ = ""
		ActualType$ = ""
		ActualAttrs$ = ""
		FullLookUp RfName(I), RfCtxt(I), ActualKind, ActualType, _
		  ActualAttrs
		
		if ActualKind <> "" then
			CheckConsistent RfName(I), RfLine(I), RfKind(I), _
			  RfType(I), ActualKind, ActualType, ActualAttrs, true
		else
			'' Note that only global symbols can be imported, with
			'' the important exception of a local label name 
			'' referred to in a RESUME statement.

			LookUp RfName(I), "", ActualKind, ActualType, _
			  ActualAttrs
			if ActualKind = "" then
				AddSymbol RfName(I), "", RfKind(I), _
				  RfType(I), A_External
			endif
		endif
	next
endsub

sub CollapseSharedVariables
	'' A shared variable is just an alias for a global variable.
	'' So it's redundant to record them specially in the symbol table.

	shared IdName(), IdCtxt(), IdKind(), IdType(), IdAttr(), IdCount

	'' For each shared variable definition, swap its shared attribute to
	'' the definition of the correspoding global var, and delete it.

	'' Note that the while loop is needed because the table may change.
	I% = 1
	while I <= IdCount
		'' Any global variable marked as shared will have been done so
		'' earlier in this process, hence the check for a nonempty 
		'' context.

		if A_Shared in IdAttr(I) and IdCtxt(I) <> "" then
			MakeGlobalShared IdName(I), IdCtxt(I), IdType(I), _
			  IdAttr(I)
			'' 'Remove' this shared var record from the output:
			IdName(I) = ""
		endif
		I = I + 1
	wend
endsub

sub MakeGlobalShared((Id$), (Ctx$), (Type$), (Attr$))
	shared IdKind(), IdType(), IdAttr()

	GIdx% = 0
	Find Id, "", GIdx
	if GIdx = 0 then
		'' In this case, assume external variable is being shared.
		'' Create an external variable definition.

		AddSymbol Id, "", K_Variable, Type, Attr + A_External
	else
		if not K_Variable in IdKind(GIdx) then
			SharedError Id, Ctx, "no corresponding global variable"
		endif
		GA$ = IdAttr(GIdx)
		if A_Array in Attr _
		and not (A_Array in GA or A_External in GA) then
			SharedError Id, Ctx, "global variable is not an array" 
		endif
		if A_Array in GA and not A_Array in Attr then
			SharedError Id, Ctx, _
			  "global variable is an array, () required"
		endif
		if A_ExplicitlyTyped in Attr and A_ExplicitlyTyped in GA _
		and Type <> IdType(GIdx) then
			SharedError Id, Ctx, _
			   "type mismatch with global variable"
		endif

		'' Mark the _global_ variable as shared.

		if not A_Shared in GA then
			IdAttr(GIdx) = IdAttr(GIdx) + A_Shared
		endif

		'' If the shared variable is external, can be more specific 
		'' about what it must be, based on the local var.

		if A_External in GA then
			IdKind(GIdx) = K_Variable
			if (not A_ExplicitlyTyped in GA) _ 
			and A_ExplicitlyTyped in Attr then
				IdType(GIdx) = Type
				IdAttr(GIdx) = IdAttr(GIdx) + A_ExplicitlyTyped
			endif
			if A_Array in Attr then
				IdAttr(GIdx) = IdAttr(GIdx) + A_Array
			endif
		endif
	endif
endsub

sub SharedError((Sym$), (Ctx$), (Msg$))
	SymbolError Sym + " shared in " + Ctx + ": " + Msg
endsub

sub DisplaySymbolTable
	shared IdName(), IdCtxt(), IdKind(), IdType(), IdAttr(), IdCount

	open "o", OutFID, ""
	
	for I = 1 to IdCount
		'' Don't include 'removed' shared variable records.

		if IdName(I) <> "" then
			write OutFID, IdName(I), IdCtxt(I), IdKind(I), IdType(I), IdAttr(I)
		endif
	next 
endsub

'' Main program:

open "i", InFID, argv(1)

LxReadFile InFID
HandleForwardReferences
CollapseSharedVariables
close InFID

DisplaySymbolTable

END
