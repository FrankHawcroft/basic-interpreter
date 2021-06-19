'' Errors.bas - defines error codes along with messages.
'' Used to generate the error table for the interpreter;
'' can also be used as a module to provide constants for
'' known error codes.

if executed then
	gosub L_Error
	gosub I__Error_Definitions
	
	Sym$ = ""
	Kind$ = ""
	Module$ = ""
	Msg$ = ""
	
	PrintHdr "Beginning of error code definitions"
	for I = 1 to MaxErrorCode
		if ED(I) <> "" then
			SplitErr I, Sym, Kind, Module, Msg
			''DefErr E_FILEBADOP TrE(1, "ER_M_IO")
			print "#define " + Sym + " " + Kind + "(" + mid(str(I), 2) + ", " + Module + ")"
		endif
	next
	PrintHdr "End of error code definitions"
	
	PrintHdr "Beginning of error message definitions"
	print chr(9) + "NULL" + ","
	for I = 1 to MaxErrorCode
		if ED(I) <> "" then
			SplitErr I, Sym, Kind, Module, Msg
			print chr(9) + chr(34) + Msg + chr(34) + ","
		else
			print chr(9) + "NULL" + ","
		endif
	next
	PrintHdr "End of error message definitions"
endif
END

L_Error:

I__Error_Ctr% = 1

const E_FileError% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_FILELOCKED% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_FILENONEXISTENT% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_FILEPROTECTED% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_FILEDEVICE% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_READPASTEOF% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_BADFILESIZE% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_RECNUMTOOBIG% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_INPUTFORMAT% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_ER_F_MORE_FILES% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_OUTSIDEDOMAIN% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_BADDIMENSION% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_NOMEMORY% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_OVERFLOWERR% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_BADSUBSTRING% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_NULLSTRING% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_ZERODIVISOR% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_BADEXPONENT% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_BADSUBSCRIPT% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_BADFILENUMBER% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_BADBUFFERSIZE% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_FILENUMBERINUSE% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_BADFILEMODE% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_BADFIELD% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_ER_OUT_OF_DATA% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_ER_BAD_SCREEN_ID% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_ER_SCREEN_HAS_WINDOW% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_ER_SCREEN% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_ER_BAD_WINDOW_ID% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1 
const E_ER_BAD_SIZE% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_ER_WINDOW% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_ER_NO_OUTPUT_WINDOW% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_ER_AREA% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_ER_UNSAFE% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_ER_AUDIO_OPEN_FAILED% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_ER_AUDIO_CHANNEL_DENIED% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_ER_AUDIO_NO_SAMPLE_MEMORY% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_ER_AUDIO_BAD_PARAMETER% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_ER_BAD_MENU_NAME% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_ER_NO_MENU% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1

I__Error_Ctr = 100

const E_Success% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_BADCOMMANDLINEARGS% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_CANTOPENCODEFILE% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_CANTOPENPROFILE% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_NOTIMPLEMENTED% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_BADCONSTANT% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_BADNAMEORTYPE% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_LONGLINE% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_REDEFINE% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_UNDEFINEDSUB% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_UNDEFINEDLABEL% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_UNDEFINEDVARORFUNC% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_MISMATCHEDPARENS% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_BADSYNTAX% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_BADARGCOUNT% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_BADARGTYPE% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_DIMENSIONMISMATCH% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_REDIMENSION% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_MODIFYCONST% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_SCALAREXPECTED% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_ARRAYEXPECTED% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_BADHANDLER% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_SUBWITHOUTENDSUB% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_ENDSUBWITHOUTSUB% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_NESTEDSUBS% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_SHAREDOUTSIDESUB% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_LOCALOUTSIDESUB% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_IFWITHOUTENDIF% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_ELSEIFWITHOUTIF% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_ENDIFWITHOUTIF% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_WHILEWITHOUTWEND% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_WENDWITHOUTWHILE% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_FORWITHOUTNEXT% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_NEXTWITHOUTFOR% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_REPEATWITHOUTUNTIL% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_UNTILWITHOUTREPEAT% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_SELECTWITHOUTENDSELECT% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_CASEOUTSIDESELECT% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_OTHERWISENOTLASTCLAUSE% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_TWOOTHERWISECLAUSES% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_ENDSELECTWITHOUTSELECT% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_RETURNWITHOUTGOSUB% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_RESUMEOUTSIDEHANDLER% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_RESUMEINTOHANDLER% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_EXECUTIONABORTED% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_PROGRAMBUFFERFULL% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_BADRETURNTYPE% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_ER_NOT_BOOL% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_FORGETINHANDLER% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_ER_MODULE_PATH_LONG% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_ER_STACK_OVERFLOW% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_ER_STACK_UNDERFLOW% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_ER_UNKNOWN_EVENT_TYPE% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_ER_EXPECTED_VARIABLE% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_ER_HALTED% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_ER_TOO_MANY_SYNTAX_ERRORS% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_ER_PARAMETER_NOT_OPTIONAL% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_ER_BAD_NEXT_VARIABLE% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_ER_ERASE_IN_SUB% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_ER_TOO_MANY_INSTANCES% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1

I__Error_Ctr = 200

const E_INTERNALEVENT% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1

I__Error_Ctr = 254

const E_Internal% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1
const E_Undefined% = I__Error_Ctr | I__Error_Ctr = I__Error_Ctr + 1

const MaxErrorCode% = I__Error_Ctr - 1 '' which should be 255 ...

RETURN '' from L_Error

I__Error_Definitions:

'' Internal/Trappable/DeadEnd distinction:

const InE$ = "MakeInternalError"
const TrE$ = "MakeTrappableError"
const DeE$ = "MakeDeadEndError"

dim ED$(MaxErrorCode)
NumErrs% = 0

sub DefErr((Code%), (Sym$), (Kind$), (Module$), (Msg$))
	shared ED$(), NumErrs%
	
	if ED(Code) <> "" then
		print "Duplicate error code: ", Code
		error 999
	endif
	ED(Code) = Sym + "|" + Kind + "|" + Module + "|" + Msg + "|"
endsub

sub SplitErr((Code%), Sym$, Kind$, Module$, Msg$)
	shared ED$()

	PE$ = ED(Code)
	GetField PE, Sym
	GetField PE, Kind
	GetField PE, Module
	GetField PE, Msg
endsub

sub GetField(S$, Txt$)
	PipePos% = instr(1, S, "|")
	Txt = mid(S, 1, PipePos - 1)
	S = mid(S, PipePos + 1, 32767)
endsub

sub PrintHdr((H$))
	print "/* ---- " + H + " generated by errors.bas on " + date + " at " + time + " ---- */"
endsub

'' Trappable errors:
DefErr E_FileError, "FILEBADOP", TrE, "ER_M_IO", "file error"
DefErr E_FILELOCKED, "FILELOCKED", TrE, "ER_M_IO", "file locked"
DefErr E_FILENONEXISTENT, "FILENONEXISTENT", TrE, "ER_M_IO", "nonexistent file"
DefErr E_FILEPROTECTED, "FILEPROTECTED", TrE, "ER_M_IO", "file is protected from access"
DefErr E_FILEDEVICE, "FILEDEVICE", TrE, "ER_M_IO", "device error"
DefErr E_READPASTEOF, "READPASTEOF", TrE, "ER_M_IO", "attempt to read past end of file"
DefErr E_BADFILESIZE, "BADFILESIZE", TrE, "ER_M_IO", "file size not multiple of record size"
DefErr E_RECNUMTOOBIG, "RECNUMTOOBIG", TrE, "ER_M_IO", "record number too large or negative"
DefErr E_INPUTFORMAT, "INPUTFORMAT", TrE, "ER_M_IO", "bad data format"
DefErr E_ER_F_MORE_FILES, "ER_F_MORE_FILES", TrE, "ER_M_IO", "not all files in directory were visited"
DefErr E_OUTSIDEDOMAIN, "OUTSIDEDOMAIN", TrE, "ER_M_RUNTIME", "value outside operator or function domain (possibly missing default clause)"
DefErr E_BADDIMENSION, "BADDIMENSION", TrE, "ER_M_RUNTIME", "illegal array dimension"
DefErr E_NOMEMORY, "NOMEMORY", TrE, "ER_M_HEAP", "out of heap space"
DefErr E_OVERFLOWERR, "OVERFLOWERR", TrE, "ER_M_RUNTIME", "numerical overflow"
DefErr E_BADSUBSTRING, "BADSUBSTRING", TrE, "ER_M_RUNTIME", "illegal substring"
DefErr E_NULLSTRING, "NULLSTRING", TrE, "ER_M_RUNTIME", "string is empty"
DefErr E_ZERODIVISOR, "ZERODIVISOR", TrE, "ER_M_RUNTIME", "division by zero"
DefErr E_BADEXPONENT, "BADEXPONENT", TrE, "ER_M_RUNTIME", "illegal exponent"
DefErr E_BADSUBSCRIPT, "BADSUBSCRIPT", TrE, "ER_M_RUNTIME", "subscript out of bounds"
DefErr E_BADFILENUMBER, "BADFILENUMBER", TrE, "ER_M_IO", "bad file number"
DefErr E_BADBUFFERSIZE, "BADBUFFERSIZE", TrE, "ER_M_IO", "bad buffer size"
DefErr E_FILENUMBERINUSE, "FILENUMBERINUSE", TrE, "ER_M_IO", "file number in use"
DefErr E_BADFILEMODE, "BADFILEMODE", TrE, "ER_M_IO", "bad file mode"
DefErr E_BADFIELD, "BADFIELD", TrE, "ER_M_IO", "bad field"
DefErr E_ER_OUT_OF_DATA, "ER_OUT_OF_DATA", TrE, "ER_M_RUNTIME", "out of DATA"
DefErr E_ER_BAD_SCREEN_ID, "ER_BAD_SCREEN_ID", TrE, "ER_M_RUNTIME", "bad screen number, or already in use"
DefErr E_ER_SCREEN_HAS_WINDOW, "ER_SCREEN_HAS_WINDOW", TrE, "ER_M_RUNTIME", "screen has window(s) open"
DefErr E_ER_SCREEN, "ER_SCREEN", TrE, "ER_M_RUNTIME", "unable to open screen - is the mode available?"
DefErr E_ER_BAD_WINDOW_ID, "ER_BAD_WINDOW_ID", TrE, "ER_M_RUNTIME", "bad window number, or already in use"
DefErr E_ER_BAD_SIZE, "ER_BAD_SIZE", TrE, "ER_M_RUNTIME", "bad size"
DefErr E_ER_WINDOW, "ER_WINDOW", TrE, "ER_M_RUNTIME", "couldn't open window"
DefErr E_ER_NO_OUTPUT_WINDOW, "ER_NO_OUTPUT_WINDOW", TrE, "ER_M_RUNTIME", "no output window"
DefErr E_ER_AREA, "ER_AREA", TrE, "ER_M_RUNTIME", "AREA is too large or has not been defined"
DefErr E_ER_UNSAFE, "ER_UNSAFE", TrE, "ER_M_RUNTIME", "statement or function can only be used in --unsafe mode"
DefErr E_ER_AUDIO_OPEN_FAILED, "ER_AUDIO_OPEN_FAILED", TrE, "ER_M_IO", "failed to open audio channel"
DefErr E_ER_AUDIO_CHANNEL_DENIED, "ER_AUDIO_CHANNEL_DENIED", TrE, "ER_M_IO", "audio channel denied"
DefErr E_ER_AUDIO_NO_SAMPLE_MEMORY, "ER_AUDIO_NO_SAMPLE_MEMORY", TrE, "ER_M_IO", "failed to allocate memory for waveform"
DefErr E_ER_AUDIO_BAD_PARAMETER, "ER_AUDIO_BAD_PARAMETER", TrE, "ER_M_IO", "bad sound or waveform parameter"
DefErr E_ER_BAD_MENU_NAME, "ER_BAD_MENU_NAME", TrE, "ER_M_RUNTIME", "bad menu name"
DefErr E_ER_NO_MENU, "ER_NO_MENU", TrE, "ER_M_RUNTIME", "nonexistent menu"

'' The 'success' (i.e. non-error) code, which is treated as an internal error if ever displayed:
DefErr E_Success, "SUCCESS", InE, "ER_M_UNKNOWN", "internal error"

'' Dead-end errors:
DefErr E_BADCOMMANDLINEARGS, "BADCOMMANDLINEARGS", DeE, "ER_M_MAIN", "bad command-line arguments"
DefErr E_CANTOPENCODEFILE, "CANTOPENCODEFILE", DeE, "ER_M_MAIN", "can't open program file"
DefErr E_CANTOPENPROFILE, "CANTOPENPROFILE", DeE, "ER_M_MAIN", "can't open profile output"
DefErr E_NOTIMPLEMENTED, "NOTIMPLEMENTED", DeE, "ER_M_RUNTIME", "unimplemented feature"
DefErr E_BADCONSTANT, "BADCONSTANT", DeE, "ER_M_LEXER", "illegal literal"
DefErr E_BADNAMEORTYPE, "BADNAMEORTYPE", DeE, "ER_M_LEXER", "illegal name"
DefErr E_LONGLINE, "LONGLINE", DeE, "ER_M_LEXER", "line too long"
DefErr E_REDEFINE, "REDEFINE", DeE, "ER_M_SYMTAB", "can't redefine existing variable, function, subprogram, or label"
DefErr E_UNDEFINEDSUB, "UNDEFINEDSUB", DeE, "ER_M_CONTROL", "undefined subprogram"
DefErr E_UNDEFINEDLABEL, "UNDEFINEDLABEL", DeE, "ER_M_CONTROL", "undefined label"
DefErr E_UNDEFINEDVARORFUNC, "UNDEFINEDVARORFUNC", DeE, "ER_M_SYMTAB", "undefined variable or function"
DefErr E_MISMATCHEDPARENS, "MISMATCHEDPARENS", DeE, "ER_M_PARSER", "syntax error (mismatched parentheses)"
DefErr E_BADSYNTAX, "BADSYNTAX", DeE, "ER_M_PARSER", "syntax error"
DefErr E_BADARGCOUNT, "BADARGCOUNT", DeE, "ER_M_SEMANTICS", "bad argument or operand count"
DefErr E_BADARGTYPE, "BADARGTYPE", DeE, "ER_M_SEMANTICS", "argument or operand type mismatch"
DefErr E_DIMENSIONMISMATCH, "DIMENSIONMISMATCH", DeE, "ER_M_SEMANTICS", "array dimension mismatch"
DefErr E_REDIMENSION, "REDIMENSION", DeE, "ER_M_RUNTIME", "attempt to redimension array"
DefErr E_MODIFYCONST, "MODIFYCONST", DeE, "ER_M_SEMANTICS", "assignment to constant"
DefErr E_SCALAREXPECTED, "SCALAREXPECTED", DeE, "ER_M_SEMANTICS", "type mismatch - a scalar is required"
DefErr E_ARRAYEXPECTED, "ARRAYEXPECTED", DeE, "ER_M_SEMANTICS", "type mismatch - an array is required"
DefErr E_BADHANDLER, "BADHANDLER", DeE, "ER_M_EVENT", "handler must not have arguments"
DefErr E_SUBWITHOUTENDSUB, "SUBWITHOUTENDSUB", DeE, "ER_M_CONTROL", "SUB without ENDSUB"
DefErr E_ENDSUBWITHOUTSUB, "ENDSUBWITHOUTSUB", DeE, "ER_M_CONTROL", "ENDSUB or EXITSUB without SUB"
DefErr E_NESTEDSUBS, "NESTEDSUBS", DeE, "ER_M_CONTROL", "nested SUB or DEF"
DefErr E_SHAREDOUTSIDESUB, "SHAREDOUTSIDESUB", DeE, "ER_M_CONTROL", "SHARED outside subprogram"
DefErr E_LOCALOUTSIDESUB, "LOCALOUTSIDESUB", DeE, "ER_M_CONTROL", "LOCAL outside subprogram"
DefErr E_IFWITHOUTENDIF, "IFWITHOUTENDIF", DeE, "ER_M_CONTROL", "IF without ENDIF"
DefErr E_ELSEIFWITHOUTIF, "ELSEIFWITHOUTIF", DeE, "ER_M_CONTROL", "ELSE or ELSEIF without IF"
DefErr E_ENDIFWITHOUTIF, "ENDIFWITHOUTIF", DeE, "ER_M_CONTROL", "ENDIF without IF"
DefErr E_WHILEWITHOUTWEND, "WHILEWITHOUTWEND", DeE, "ER_M_CONTROL", "WHILE without WEND"
DefErr E_WENDWITHOUTWHILE, "WENDWITHOUTWHILE", DeE, "ER_M_CONTROL", "WEND without WHILE"
DefErr E_FORWITHOUTNEXT, "FORWITHOUTNEXT", DeE, "ER_M_CONTROL", "FOR without NEXT"
DefErr E_NEXTWITHOUTFOR, "NEXTWITHOUTFOR", DeE, "ER_M_CONTROL", "NEXT without FOR"
DefErr E_REPEATWITHOUTUNTIL, "REPEATWITHOUTUNTIL", DeE, "ER_M_CONTROL", "REPEAT without UNTIL or FOREVER"
DefErr E_UNTILWITHOUTREPEAT, "UNTILWITHOUTREPEAT", DeE, "ER_M_CONTROL", "UNTIL or FOREVER without REPEAT"
DefErr E_SELECTWITHOUTENDSELECT, "SELECTWITHOUTENDSELECT", DeE, "ER_M_CONTROL", "SELECT without ENDSELECT"
DefErr E_CASEOUTSIDESELECT, "CASEOUTSIDESELECT", DeE, "ER_M_CONTROL", "CASE or OTHERWISE outside a SELECT block"
DefErr E_OTHERWISENOTLASTCLAUSE, "OTHERWISENOTLASTCLAUSE", DeE, "ER_M_CONTROL", "OTHERWISE not last clause in SELECT block"
DefErr E_TWOOTHERWISECLAUSES, "TWOOTHERWISECLAUSES", DeE, "ER_M_CONTROL", "two OTHERWISE clauses in SELECT block"
DefErr E_ENDSELECTWITHOUTSELECT, "ENDSELECTWITHOUTSELECT", DeE, "ER_M_CONTROL", "ENDSELECT without SELECT"
DefErr E_RETURNWITHOUTGOSUB, "RETURNWITHOUTGOSUB", DeE, "ER_M_CONTROL", "RETURN without GOSUB"
DefErr E_RESUMEOUTSIDEHANDLER, "RESUMEOUTSIDEHANDLER", DeE, "ER_M_EVENT", "no event to RESUME after"
DefErr E_RESUMEINTOHANDLER, "RESUMEINTOHANDLER", DeE, "ER_M_EVENT", "attempt to RESUME into handler"
DefErr E_EXECUTIONABORTED, "EXECUTIONABORTED", DeE, "ER_M_EVENT", "execution halted by signal break (Ctrl-C)"
DefErr E_PROGRAMBUFFERFULL, "PROGRAMBUFFERFULL", DeE, "ER_M_MAIN", "program too large for buffer"
DefErr E_BADRETURNTYPE, "BADRETURNTYPE", DeE, "ER_M_SEMANTICS", "function return value doesn't have declared type"
DefErr E_ER_NOT_BOOL, "ER_NOT_BOOL", DeE, "ER_M_SEMANTICS", "Boolean value required"
DefErr E_FORGETINHANDLER, "FORGETINHANDLER", DeE, "ER_M_EVENT", "attempt to FORGET inside event handler"
DefErr E_ER_MODULE_PATH_LONG, "ER_MODULE_PATH_LONG",  DeE, "ER_M_RUNTIME", "module search path(s) too long"
DefErr E_ER_STACK_OVERFLOW, "ER_STACK_OVERFLOW", DeE, "ER_M_STACK", "stack overflow" '' TODO should be trappable
DefErr E_ER_STACK_UNDERFLOW, "ER_STACK_UNDERFLOW", DeE, "ER_M_STACK", "stack underflow"
DefErr E_ER_UNKNOWN_EVENT_TYPE, "ER_UNKNOWN_EVENT_TYPE", DeE, "ER_M_EVENT", "unknown event type"
DefErr E_ER_EXPECTED_VARIABLE, "ER_EXPECTED_VARIABLE", DeE, "ER_M_SEMANTICS", "a variable is expected here"
DefErr E_ER_HALTED, "ER_HALTED", DeE, "ER_M_RUNTIME", "END statement executed (you shouldn't see this message!)"
DefErr E_ER_TOO_MANY_SYNTAX_ERRORS, "ER_TOO_MANY_SYNTAX_ERRORS", DeE, "ER_M_MAIN", "too many syntax errors - stopping syntax check"
DefErr E_ER_PARAMETER_NOT_OPTIONAL, "ER_PARAMETER_NOT_OPTIONAL", DeE, "ER_M_SEMANTICS", "bad argument or operand count - parameter is required"
DefErr E_ER_BAD_NEXT_VARIABLE, "ER_BAD_NEXT_VARIABLE", DeE, "ER_M_SEMANTICS", "NEXT variable doesn't match FOR"
DefErr E_ER_ERASE_IN_SUB, "ER_ERASE_IN_SUB", DeE, "ER_M_SEMANTICS", "attempt to erase argument or shared array in subprogram"
DefErr E_ER_TOO_MANY_INSTANCES, "ER_TOO_MANY_INSTANCES", DeE, "ER_M_MAIN", "too many interpreter instances"

'' Internal errors - specific:
DefErr E_INTERNALEVENT, "INTERNALEVENT", InE, "ER_M_RUNTIME", "event error"

'' Internal errors - general:
DefErr E_Undefined, "ER_UNDEFINED", InE, "ER_M_UNKNOWN", "unknown error"
DefErr E_Internal, "ER_INTERNAL", InE, "ER_M_RUNTIME", "internal error"

RETURN '' from I__Error_Definitions
