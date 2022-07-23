/****** statements.c ******/

/*
	$VER: statements.c 0.16A (9.10.2015)

	Names and parameters of built-in commands, and some other functions relating thereto.
*/

#include "interpreter.h"
#include "process.h"
#include "builtin.h"
#include "heap.h"
#include "hashtable.h"

/* Match 'any number' of actual parameters - */
#define UNLIMITED MAX_TOKENS

static struct Parameter m_ArgForArea[1] = {
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 2, FALSE}};
	
static struct Parameter m_ArgForAreaFill[1] = {
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 1, FALSE}};
	
static struct Parameter m_ArgForCase[1] = {
	{LITERAL, TR_ANY, NULL, NO_NAME, UNLIMITED, FALSE}};

static struct Parameter m_ArgForChDir[1] = {
	{LITERAL, TR_STRING_ONLY, NULL, NO_NAME, 1, FALSE}};

static struct Parameter m_ArgForCircle[1] = {
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 3, FALSE}}; /* TODO x, y, r - also aspect ratio, colour,... */
	
static struct Parameter m_ArgForClear[1] = {
	{LITERAL, TR_INT_TO_LONG, NULL, NO_NAME, 2, FALSE}};

static struct Parameter m_ArgForClose[1] = {
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, UNLIMITED, FALSE}};

static struct Parameter m_ArgForColour[1] = {
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 2, FALSE}};

static Scalar m_DefDim1;
static struct Parameter m_ArgsForDim[2] = {
	{ARRAY, TR_ANY, NULL, NO_NAME, 1, FALSE},
	{LITERAL, TR_SUBSCRIPT, &m_DefDim1, NO_NAME, MAX_DIMENSIONS, FALSE}};

static struct Parameter m_ArgForIfAndElseIf[1] = {
	{LITERAL, TR_LOGICAL, NULL, NO_NAME, 1, FALSE}};

static struct Parameter m_ArgForErase[1] = {
	{ARRAY, TR_ANY, NULL, NO_NAME, UNLIMITED, FALSE}};
	
static Scalar m_DefError1;
static struct Parameter m_ArgForError[1] = {
	{LITERAL, TR_NUM_TO_INT, &m_DefError1, NO_NAME, 1, FALSE}};

static struct Parameter m_ArgsForField[2] = {
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 1, FALSE},
	{SCALAR_VAR, TR_STRING_ONLY, NULL, NO_NAME, UNLIMITED, FALSE}};

static struct Parameter m_ArgsForFiles[3] = {
	{LITERAL, TR_STRING_ONLY, NULL, NO_NAME, 1, FALSE},
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 1, FALSE},
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 1, FALSE}};

static struct Parameter m_ArgsForFInput[2] = {
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 1, FALSE},
	{SCALAR_VAR, TR_ANY, NULL, NO_NAME, UNLIMITED, FALSE}};

static struct Parameter m_ArgsForFLineInput[2] = {
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 1, FALSE},
	{SCALAR_VAR, TR_STRING_ONLY, NULL, NO_NAME, UNLIMITED, FALSE}};

static struct Parameter m_ArgsForFor[4] = {
	{SCALAR_VAR, TR_NUMERIC, NULL, NO_NAME, 1, FALSE},
	{LITERAL, TR_PROMOTE, NULL, NO_NAME, 1, FALSE},
	{LITERAL, TR_PROMOTE, NULL, NO_NAME, 1, FALSE},
	{LITERAL, TR_PROMOTE, NULL, NO_NAME, 1, FALSE}};

static struct Parameter m_ArgsForFPrint[2] = {
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 1, FALSE},
	{LITERAL, TR_ANY, NULL, NO_NAME, UNLIMITED, FALSE}};

static struct Parameter m_ArgsForGetAndPut[2] = {
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 1, FALSE},
	{LITERAL, TR_INT_TO_LONG, NULL, NO_NAME, 1, FALSE}};

static struct Parameter m_ArgForGoToAndGoSub[1] = {
	{LABEL, TR_IRRELEVANT, NULL, NO_NAME, 1, FALSE}};

static struct Parameter m_ArgsForIfGoTo[2] = {
	{LITERAL, TR_LOGICAL, NULL, NO_NAME, 1, FALSE},
	{LABEL, TR_IRRELEVANT, NULL, NO_NAME, 1, FALSE}};
	
static struct Parameter m_ArgsForIfThenLet[3] = {
	{LITERAL, TR_LOGICAL, NULL, NO_NAME, 1, FALSE},
	{SCALAR_VAR, TR_ANY, NULL, NO_NAME, 1, FALSE},
	{LITERAL, TR_ASSIGNMENT, NULL, NO_NAME, 1, FALSE}};

static Scalar m_DefInputAndLineInput1;
static struct Parameter m_ArgsForInput[2] = {
	{LITERAL, TR_STRING_ONLY, &m_DefInputAndLineInput1, NO_NAME, 1, FALSE},
	{SCALAR_VAR, TR_ANY, NULL, NO_NAME, UNLIMITED, FALSE}};

static struct Parameter m_ArgForKill[1] = {
	{LITERAL, TR_STRING_ONLY, NULL, NO_NAME, 1, FALSE}};

static struct Parameter m_ArgsForConstAndLet[2] = {
	{SCALAR_VAR, TR_ANY, NULL, NO_NAME, 1, FALSE},
	{LITERAL, TR_ASSIGNMENT, NULL, NO_NAME, 1, FALSE}};

static struct Parameter m_ArgForLine[1] = {
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 4, FALSE}};

static struct Parameter m_ArgsForLineInput[2] = {
	{LITERAL, TR_STRING_ONLY, &m_DefInputAndLineInput1, NO_NAME, 1, FALSE},
	{SCALAR_VAR, TR_STRING_ONLY, NULL, NO_NAME, 1, FALSE}};

static struct Parameter m_ArgForLoad[1] = {
	{LITERAL, TR_STRING_ONLY, NULL, NO_NAME, 1, FALSE}};

static struct Parameter m_ArgsForMenu[3] = {
	{LITERAL, TR_STRING_ONLY, NULL, NO_NAME, 1, FALSE},
	{LITERAL, TR_STRING_TO_CHAR, NULL, NO_NAME, 1, FALSE},
	{LITERAL, TR_NUM_TO_LONG, NULL, NO_NAME, 1, FALSE}};

static struct Parameter m_ArgForMerge[1] = {
	{LITERAL, TR_STRING_ONLY, NULL, NO_NAME, 1, FALSE}};

static struct Parameter m_ArgsForName[2] = {
	{LITERAL, TR_STRING_ONLY, NULL, NO_NAME, 1, FALSE},
	{LITERAL, TR_STRING_ONLY, NULL, NO_NAME, 1, FALSE}};
	
static struct Parameter m_ArgForNextVar[1] = {
	{SCALAR_VAR, TR_NUMERIC, NULL, NO_NAME, UNLIMITED, FALSE}};

static struct Parameter m_ArgsForOnGoTo[2] = {
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 1, FALSE},
	{LABEL, TR_IRRELEVANT, NULL, NO_NAME, UNLIMITED, FALSE}};

static struct Parameter m_ArgsForOpen[4] = {
	{LITERAL, TR_STRING_TO_CHAR, NULL, NO_NAME, 1, FALSE},
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 1, FALSE},
	{LITERAL, TR_STRING_ONLY, NULL, NO_NAME, 1, FALSE},
	{LITERAL, TR_INT_TO_LONG, NULL, NO_NAME, 1, FALSE}}; 

static struct Parameter m_ArgForOptionBase[1] = {
	{LITERAL, TR_SUBSCRIPT, NULL, NO_NAME, 1, FALSE}};

static struct Parameter m_ArgForPaint[1] = {
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 3, FALSE}};

static struct Parameter m_ArgsForPalette[2] = {
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 1, FALSE},
	{LITERAL, TR_NUM_TO_SINGLE, NULL, NO_NAME, 3, FALSE}};

static struct Parameter m_ArgsForPattern[2] = {
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 1, FALSE},
	{ARRAY, TR_INT_ONLY, NULL, NO_NAME, 1, FALSE}};

static struct Parameter m_ArgsForPokeAndPokeW[2] = {
	{LITERAL, TR_INT_TO_LONG, NULL, NO_NAME, 1, FALSE},
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 1, FALSE}};
	
static struct Parameter m_ArgsForPokeL[2] = {
	{LITERAL, TR_INT_TO_LONG, NULL, NO_NAME, 1, FALSE},
	{LITERAL, TR_INT_TO_LONG, NULL, NO_NAME, 1, FALSE}};
	
static struct Parameter m_ArgForPReset[1] = {
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 2, FALSE}};
	
static struct Parameter m_ArgForPrint[1] = {
	{LITERAL, TR_ANY, NULL, NO_NAME, UNLIMITED, FALSE}};

static struct Parameter m_ArgsForPSet[2] = {
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 2, FALSE},
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 1, FALSE}};

static struct Parameter m_ArgForRandomize[1] = {
	{LITERAL, TR_NUM_TO_LONG, NULL, NO_NAME, 1, FALSE}};

static struct Parameter m_ArgForRead[1] = {
	{SCALAR_VAR, TR_ANY, NULL, NO_NAME, UNLIMITED, FALSE}};

static Scalar m_DefScreen2;
static struct Parameter m_ArgsForScreen[] = {
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 1, FALSE},
	{LITERAL, TR_STRING_ONLY, &m_DefScreen2, NO_NAME, 1, FALSE},
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 3, FALSE},
	{LITERAL, TR_INT_TO_LONG, NULL, NO_NAME, 1, FALSE}};
	
static struct Parameter m_ArgForScreenClose[] = {
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 1, FALSE}};
	
static struct Parameter m_ArgsForScreenGetAndPut[] = {
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 4, FALSE},
	{ARRAY, TR_NUMERIC, NULL, NO_NAME, 1, FALSE}
};

static struct Parameter m_ArgForScroll[1] = {
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 6, FALSE}};

static struct Parameter m_ArgForSelect[1] = {
	{LITERAL, TR_ANY, NULL, NO_NAME, 1, FALSE}};

static Scalar m_DefSound3;
static struct Parameter m_ArgsForSound[5] = {
	{LITERAL, TR_NUM_TO_SINGLE, NULL, NO_NAME, 1, FALSE},
	{LITERAL, TR_NUM_TO_SINGLE, NULL, NO_NAME, 1, FALSE},
	{LITERAL, TR_NUM_TO_INT, &m_DefSound3, NO_NAME, 1, FALSE},
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 1, FALSE},
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 1, FALSE}};

static struct Parameter m_ArgsForSwap[2] = {
	{SCALAR_VAR, TR_ANY, NULL, NO_NAME, 1, FALSE},
	{SCALAR_VAR, TR_SAME, NULL, NO_NAME, 1, FALSE}};

static struct Parameter m_ArgForSystem[1] = {
	{LITERAL, TR_LOGICAL, NULL, NO_NAME, 1, FALSE}};
	
static struct Parameter m_ArgForUntilAndWhile[1] = {
	{LITERAL, TR_LOGICAL, NULL, NO_NAME, 1, FALSE}};

static struct Parameter m_ArgForWait[1] = {
	{LITERAL, TR_NUM_TO_SINGLE, NULL, NO_NAME, 1, FALSE}};

static struct Parameter m_ArgsForWave[] = {
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 1, FALSE},
	{ARRAY, TR_NUMERIC, NULL, NO_NAME, 1, FALSE}};

static struct Parameter m_ArgForWaveSin[] = {
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 1, FALSE}};
	
static Scalar m_DefWindow2;
static struct Parameter m_ArgsForWindow[] = {
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 1, FALSE},
	{LITERAL, TR_STRING_ONLY, &m_DefWindow2, NO_NAME, 1, FALSE},
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 4, FALSE},
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 1, FALSE},
	{LITERAL, TR_STRING_ONLY, NULL, NO_NAME, 1, FALSE},
	{LITERAL, TR_INT_TO_LONG, NULL, NO_NAME, 1, FALSE}};
	
static struct Parameter m_ArgForWLocate[1] = {
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 2, FALSE}};
	
static struct Parameter m_ArgForWindowCloseOutputActivateToFront[1] = {
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 1, FALSE}};
	
static struct Parameter m_ArgForWPrint[] = {
	{LITERAL, TR_ANY_TO_STRING, NULL, NO_NAME, UNLIMITED, FALSE}};
	
static struct Parameter m_ArgsForWrite[] = {
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 1, FALSE},
	{LITERAL, TR_ANY, NULL, NO_NAME, UNLIMITED, FALSE}};

#ifdef DEBUG
static struct Parameter m_ArgForXStack[] = {
	{LITERAL, TR_NUM_TO_INT, NULL, NO_NAME, 1, FALSE}};
#endif

static bool m_DefaultsInitialised = FALSE;

/* Used to define built-in statements statically so they can easily be added to 
the symbol table. Default values for arguments must be set up before
adding the statements to the symbol table - see DefineBuiltInStatements(). */
struct BuiltInStatement {
	const char *name; /* Converted to a QString. */
	void (*method)();
	void (*convert)(unsigned, const QString *, BObject *);
	bool (*inactive)(struct Process *, bool);
	struct Parameter *formal;
	short formalCount; /* The number of items in 'formal', NOT the actual parameter count
						which must be supplied when used. (See semantics.c) */
};

static const struct BuiltInStatement m_StmtDefinitions[] = {		
	{KW_AREA, Area_, DefaultConvert, DefaultInactive, m_ArgForArea, 1},
	{"AREAFILL", AreaFill_, DefaultConvert, DefaultInactive, m_ArgForAreaFill, 1},
	{"AREASTEP", AreaStep_, DefaultConvert, DefaultInactive, m_ArgForArea, 1},
	{"BEEP", Beep_, DefaultConvert, DefaultInactive, NULL, 0},
	{"BREAK", Break_, DefaultConvert, DefaultInactive, NULL, 0},
	{"CASE", Case_, DefaultConvert, CaseInactive, m_ArgForCase, 1},
	/* {"CHAIN", Chain_, DefaultConvert, DefaultInactive, }, */
	{"CHDIR", ChDir_, DefaultConvert, DefaultInactive, m_ArgForChDir, 1},
	{"CIRCLE", Circle_, DefaultConvert, DefaultInactive, m_ArgForCircle, 1},
	{"CIRCLESTEP", CircleStep_, DefaultConvert, DefaultInactive, m_ArgForCircle, 1},
	{"CLEAR", Clear_, DefaultConvert, DefaultInactive, m_ArgForClear, 1},
	{KW_CLOSE, Close_, DefaultConvert, DefaultInactive, m_ArgForClose, 1},
	{"COLOR", Colour_, DefaultConvert, DefaultInactive, m_ArgForColour, 1},
	{"COLOUR", Colour_, DefaultConvert, DefaultInactive, m_ArgForColour, 1},
	{KW_CONST, Let_, ConstConvert, DefaultInactive, m_ArgsForConstAndLet, 2},
	{KW_DATA, Data_, EmptyConvert, DataInactive, NULL, TOKENISED_ARGUMENTS},
	{KW_DEF, Def_, EmptyConvert, DefaultInactive, NULL, TOKENISED_ARGUMENTS},
	{"DEFBLN", DefBln_, EmptyConvert, DefaultInactive, NULL, TOKENISED_ARGUMENTS},
	{"DEFCHR", DefChr_, EmptyConvert, DefaultInactive, NULL, TOKENISED_ARGUMENTS},
	{"DEFDBL", DefDbl_, EmptyConvert, DefaultInactive, NULL, TOKENISED_ARGUMENTS},
	{"DEFINT", DefInt_, EmptyConvert, DefaultInactive, NULL, TOKENISED_ARGUMENTS},
	{"DEFLNG", DefLng_, EmptyConvert, DefaultInactive, NULL, TOKENISED_ARGUMENTS},
	{"DEFSNG", DefSng_, EmptyConvert, DefaultInactive, NULL, TOKENISED_ARGUMENTS},
	{"DEFSTR", DefStr_, EmptyConvert, DefaultInactive, NULL, TOKENISED_ARGUMENTS},
	{KW_DIM, Dim_, DimConvert, DefaultInactive, m_ArgsForDim, 2},
	{"DIMSHARED", Dim_, DimSharedConvert, DefaultInactive, m_ArgsForDim, 2},
	{KW_DISABLE, Disable_, EmptyConvert, DefaultInactive, NULL, TOKENISED_ARGUMENTS},
	{KW_ELSE, Else_, DefaultConvert, ElseInactive, NULL, 0},
	{KW_ELSEIF, ElseIf_, DefaultConvert, ElseInactive, m_ArgForIfAndElseIf, 1},
	{KW_EMPTY_STMT, EmptyStatement_, EmptyConvert, EmptyInactive, NULL, TOKENISED_ARGUMENTS},
	{KW_ENABLE, Enable_, EmptyConvert, DefaultInactive, NULL, TOKENISED_ARGUMENTS},
	{KW_END, End_, DefaultConvert, DefaultInactive, NULL, 0},
	{"ENDIF", EndIf_, DefaultConvert, EndIfInactive, NULL, 0},
	{"ENDSELECT", EndSelect_, DefaultConvert, EndSelectInactive, NULL, 0},
	{"ENDSUB", EndSub_, DefaultConvert, EndSubInactive, NULL, 0},
	{"ERASE", Erase_, DefaultConvert, DefaultInactive, m_ArgForErase, 1},
	{"ERROR", Error_, DefaultConvert, DefaultInactive, m_ArgForError, 1},
	{"EXITSUB", ExitSub_, DefaultConvert, SubprogramOnlyInactive, NULL, 0},
	{"FIELD", Field_, DefaultConvert, DefaultInactive, m_ArgsForField, 2},
	{"FILES", Files_, DefaultConvert, DefaultInactive, m_ArgsForFiles, 3},
	{"FINPUT", FInput_, DefaultConvert, DefaultInactive, m_ArgsForFInput, 2},
	{"FLINEINPUT", FLineInput_, DefaultConvert, DefaultInactive, m_ArgsForFLineInput, 2},
	{KW_FOR, For_, AssignConvert, ForInactive, m_ArgsForFor, 4},
	{"FOREVER", Forever_, DefaultConvert, UntilInactive, NULL, 0},
	{"FORGET", Forget_, EmptyConvert, DefaultInactive, NULL, TOKENISED_ARGUMENTS},
	{KW_FPRINT, FPrint_, DefaultConvert, DefaultInactive, m_ArgsForFPrint, 2},
	{"GET", Get_, DefaultConvert, DefaultInactive, m_ArgsForGetAndPut, 2},
	{KW_GOSUB, GoSub_, JumpConvert, DefaultInactive, m_ArgForGoToAndGoSub, 1},
	{KW_GOTO, GoTo_, JumpConvert, DefaultInactive, m_ArgForGoToAndGoSub, 1},
	{KW_IF, If_, DefaultConvert, IfInactive, m_ArgForIfAndElseIf, 1},
	{KW_IFGOTO, IfGoTo_, ConditionalJumpConvert, DefaultInactive, m_ArgsForIfGoTo, 2},
	{KW_IFTHENELSE, IfThenElse_, EmptyConvert, DefaultInactive, NULL, TOKENISED_ARGUMENTS},
	{KW_IFTHENLET, IfThenLet_, DefaultConvert, DefaultInactive, m_ArgsForIfThenLet, 3},
	{KW_INPUT, Input_, DefaultConvert, DefaultInactive, m_ArgsForInput, 2},
	{"KILL", Kill_, DefaultConvert, DefaultInactive, m_ArgForKill, 1},
	{KW_LETQ_LOCAL, Let_, LocalScalarAssignConvert, DefaultInactive, m_ArgsForConstAndLet, 2}, /* Quicker for local scalars. */
	{KW_LETQ_PREDEF, Let_, DefaultConvert, DefaultInactive, m_ArgsForConstAndLet, 2}, /* Quicker for predefined vars. */
	{"LINE", Line_, DefaultConvert, DefaultInactive, m_ArgForLine, 1},
	{"LINEINPUT", LineInput_, DefaultConvert, DefaultInactive, m_ArgsForLineInput, 2},
	{"LOAD", Load_, DefaultConvert, DefaultInactive, m_ArgForLoad, 1},
	{"MENU", Menu_, DefaultConvert, DefaultInactive, m_ArgsForMenu, 3},
	{"MERGE", Merge_, DefaultConvert, DefaultInactive, m_ArgForMerge, 1},
	{KW_NAME, Name_, DefaultConvert, DefaultInactive, m_ArgsForName, 2},
	{KW_NEXT, Next_, DefaultConvert, NextInactive, NULL, 0},
	{KW_NEXTVAR, NextVar_, DefaultConvert, NextInactive, m_ArgForNextVar, 1},
	{KW_ON, On_, EmptyConvert, DefaultInactive, NULL, TOKENISED_ARGUMENTS},
	{KW_ONGOSUB, OnGoSub_, ConditionalJumpConvert, DefaultInactive, m_ArgsForOnGoTo, 2},
	{KW_ONGOTO, OnGoTo_, ConditionalJumpConvert, DefaultInactive, m_ArgsForOnGoTo, 2},
	{KW_OPEN, Open_, DefaultConvert, DefaultInactive, m_ArgsForOpen, 4},
	{"OPTIONBASE", OptionBase_, DefaultConvert, DefaultInactive, m_ArgForOptionBase, 1},
	{"OTHERWISE", Otherwise_, DefaultConvert, CaseInactive, NULL, 0},
	{"PAINT", Paint_, DefaultConvert, DefaultInactive, m_ArgForPaint, 1},
	{"PALETTE", Palette_, DefaultConvert, DefaultInactive, m_ArgsForPalette, 2},
	{"PATTERN", Pattern_, DefaultConvert, DefaultInactive, m_ArgsForPattern, 2},
	{"POKE", Poke_, DefaultConvert, DefaultInactive, m_ArgsForPokeAndPokeW, 2},
	{"POKEL", PokeL_, DefaultConvert, DefaultInactive, m_ArgsForPokeL, 2},
	{"POKEW", PokeW_, DefaultConvert, DefaultInactive, m_ArgsForPokeAndPokeW, 2},
	{"PRESET", PReset_, DefaultConvert, DefaultInactive, m_ArgForPReset, 1},
	{KW_PRINT, Print_, DefaultConvert, DefaultInactive, m_ArgForPrint, 1},
	{"PSET", PSet_, DefaultConvert, DefaultInactive, m_ArgsForPSet, 2},
	{"PUT", Put_, DefaultConvert, DefaultInactive, m_ArgsForGetAndPut, 2},
	{"RANDOMIZE", Randomize_, DefaultConvert, DefaultInactive, m_ArgForRandomize, 1},
	{"READ", Read_, DefaultConvert, DefaultInactive, m_ArgForRead, 1},
	{"REPEAT", Repeat_, DefaultConvert, RepeatInactive, NULL, 0},
	{"RESTORE", Restore_, EmptyConvert, DefaultInactive, NULL, TOKENISED_ARGUMENTS},
	{"RESUME", Resume_, EmptyConvert, DefaultInactive, NULL, TOKENISED_ARGUMENTS},
	{KW_RETURN, Return_, DefaultConvert, DefaultInactive, NULL, 0},
	{KW_RETURNTO, ReturnTo_, JumpConvert, DefaultInactive, m_ArgForGoToAndGoSub, 1},
	{"RUN", Run_, DefaultConvert, DefaultInactive, NULL, 0},
	{KW_SCREEN, Screen_, DefaultConvert, DefaultInactive, m_ArgsForScreen, 4},
	{"SCREENCLOSE", ScreenClose_, DefaultConvert, DefaultInactive, m_ArgForScreenClose, 1},
	{"SCREENGET", ScreenGet_, DefaultConvert, DefaultInactive, m_ArgsForScreenGetAndPut, 2},
	{"SCREENPUT", ScreenPut_, DefaultConvert, DefaultInactive, m_ArgsForScreenGetAndPut, 2},
	{"SCROLL", Scroll_, DefaultConvert, DefaultInactive, m_ArgForScroll, 1},
	{"SELECT", Select_, DefaultConvert, SelectInactive, m_ArgForSelect, 1},
	{"SHARED", Shared_, EmptyConvert, SubprogramOnlyInactive, NULL, TOKENISED_ARGUMENTS},
	{"SLEEP", Sleep_, DefaultConvert, DefaultInactive, NULL, 0},
	{"SOUND", Sound_, DefaultConvert, DefaultInactive, m_ArgsForSound, 5},
	{"SOUNDRESUME", SoundResume_, DefaultConvert, DefaultInactive, NULL, 0},
	{"SOUNDWAIT", SoundWait_, DefaultConvert, DefaultInactive, NULL, 0},
	{KW_STOP, Stop_, DefaultConvert, DefaultInactive, NULL, 0},
	{"SUB", Sub_, EmptyConvert, DefaultInactive, NULL, TOKENISED_ARGUMENTS},
	{KW_SUSPEND, Suspend_, EmptyConvert, DefaultInactive, NULL, TOKENISED_ARGUMENTS},
	{"SWAP", Swap_, DefaultConvert, DefaultInactive, m_ArgsForSwap, 2},
	{"SYSTEM", System_, DefaultConvert, DefaultInactive, m_ArgForSystem, 1},
	{"TROFF", TrOff_, DefaultConvert, DefaultInactive, NULL, 0},
	{"TRON", TrOn_, DefaultConvert, DefaultInactive, NULL, 0},
	{"UNTIL", Until_, DefaultConvert, UntilInactive, m_ArgForUntilAndWhile, 1},
	{"WAIT", Wait_, DefaultConvert, DefaultInactive, m_ArgForWait, 1},
	{"WAVE", Wave_, DefaultConvert, DefaultInactive, m_ArgsForWave, 2},
	{"WAVESIN", WaveSin_, DefaultConvert, DefaultInactive, m_ArgForWaveSin, 1},
	{"WCLS", WClS_, DefaultConvert, DefaultInactive, NULL, 0},
	{"WEND", WEnd_, DefaultConvert, WEndInactive, NULL, 0},
	{"WHILE", While_, DefaultConvert, WhileInactive, m_ArgForUntilAndWhile, 1},
	{"WINDOW", Window_, DefaultConvert, DefaultInactive, m_ArgsForWindow, 6},
	{"WINDOWCLOSE", WindowClose_, DefaultConvert, DefaultInactive, m_ArgForWindowCloseOutputActivateToFront, 1},
	{"WINDOWOUTPUT", WindowOutput_, DefaultConvert, DefaultInactive, m_ArgForWindowCloseOutputActivateToFront, 1},
	{"WLOCATE", WLocate_, DefaultConvert, DefaultInactive, m_ArgForWLocate, 1},
	{"WPRINT", WPrint_, DefaultConvert, DefaultInactive, m_ArgForWPrint, 1},
	{"WRITE", Write_, DefaultConvert, DefaultInactive, m_ArgsForWrite, 2},

#ifdef DEBUG
	{"XCACHE", XCache_, DefaultConvert, DefaultInactive, NULL, 0},
	{"XDOC", XDoc_, DefaultConvert, DefaultInactive, NULL, 0},
	{"XFREE", XFree_, DefaultConvert, DefaultInactive, NULL, 0},
	{"XOBJ", XObj_, EmptyConvert, DefaultInactive, NULL, TOKENISED_ARGUMENTS},
	{"XSTACK", XStack_, DefaultConvert, DefaultInactive, m_ArgForXStack, 1}, 
#endif

	/* LET is added last - see the comment in process.c/DefineBuiltIns */
	{KW_LET, Let_, AssignConvert, DefaultInactive, m_ArgsForConstAndLet, 2},

	{NULL, NULL, NULL, NULL, NULL, 0} /* sentinel */
};

static void InitStatement(struct Statement *stmt)
{
	stmt->method.builtIn = NULL;
	stmt->convert = DefaultConvert;
	stmt->inactive = DefaultInactive;
	stmt->formal = NULL;
	stmt->formalCount = 0;
	stmt->userDefined = FALSE;
	stmt->staticSub = FALSE;
}

bool StatementIsEmpty(const struct Statement *command)
{
	return command == NULL || (!IsSubprogram(command) && command->method.macro == EmptyStatement_);
}

extern void ForwardDefineAllSubprograms(void);

/* Statements aren't allowed to be overloaded with anything else, and are always visible at SCOPE_GLOBAL. */
Error GetStatement(const QString *statementName, const struct Statement **stmt)
{
	BObject *stmtDefn = LookUp(statementName, SCOPE_GLOBAL);
	if(stmtDefn == NULL) {
		ForwardDefineAllSubprograms();
		stmtDefn = LookUp(statementName, SCOPE_GLOBAL);
	}
	if(stmtDefn == NULL || stmtDefn->category != STATEMENT) {
		*stmt = NULL;
		return PositionError(UNDEFINEDSUB, Proc()->currentStatementStart, QsGetData(statementName));
	}
	else {
		*stmt = stmtDefn->value.statement;
		return SUCCESS;
	}
}

void DisposeStatement(struct Statement *stmt)
{
	if(IsSubprogram(stmt)) {
		if(stmt->predefinedParameter != NULL)
			Dispose(stmt->predefinedParameter);
		if(stmt->localStatics != NULL)
			HtDispose(stmt->localStatics);
		if(stmt->formal != NULL)
			Dispose(stmt->formal);
	}
	Dispose(stmt);
}

static void DefineBuiltInStatement(const struct BuiltInStatement *command)
{
	QString name;
	struct Statement *newStmt = New(sizeof(struct Statement));
	
	if(!m_DefaultsInitialised) {
		/* 1. Generic defaults - */
		m_ArgForAreaFill[0].defaultValue = g_ZeroInt;
		m_ArgForClear[0].defaultValue = g_ZeroLongInt;
		m_ArgForClose[0].defaultValue = g_MinimumInt;
		m_ArgForColour[0].defaultValue = g_NegOneInt;
		m_ArgsForFiles[0].defaultValue = g_CurDirString;
		m_ArgsForFiles[1].defaultValue = g_MaximumInt;
		m_ArgsForFiles[2].defaultValue = g_NegOneInt;
		m_ArgsForFor[3].defaultValue = g_OneInt;
		m_ArgsForFPrint[1].defaultValue = g_NullString;
		m_ArgsForGetAndPut[1].defaultValue = g_MinimumInt;
		m_ArgsForMenu[0].defaultValue = g_NullString;
		m_ArgsForMenu[1].defaultValue = g_NulChar;
		m_ArgsForMenu[2].defaultValue = g_ZeroLongInt;
		m_ArgsForOpen[3].defaultValue = g_ZeroLongInt;
		m_ArgsForPattern[0].defaultValue = g_NegOneInt;
		m_ArgForPrint[0].defaultValue = g_NullString;
		m_ArgsForPSet[1].defaultValue = g_NegOneInt;
		m_ArgForRandomize[0].defaultValue = g_ZeroLongInt;
		m_ArgsForScreen[2].defaultValue = g_NegOneInt;
		m_ArgsForScreen[3].defaultValue = g_NegOneLongInt;
		m_ArgsForSound[3].defaultValue = g_NegOneInt;
		m_ArgsForSound[4].defaultValue = g_ZeroInt;
		m_ArgForSystem[0].defaultValue = g_FalseBoolean;
		m_ArgsForWindow[2].defaultValue = g_NegOneInt;
		m_ArgsForWindow[3].defaultValue = g_NegOneInt;
		m_ArgsForWindow[4].defaultValue = g_NullString;
		m_ArgsForWindow[5].defaultValue = g_ZeroLongInt;
		m_ArgForWPrint[0].defaultValue = g_NullString;
#ifdef DEBUG
		m_ArgForXStack[0].defaultValue = g_MaximumInt;
#endif
		
		/* 2. Specialised defaults - */
		SetFromLong(&m_DefDim1, 10, T_INT);
		SetFromLong(&m_DefError1, (long)ER_UNDEFINED, T_INT);
		InitScalarAsString(&m_DefInputAndLineInput1);
		QsInitStaticNTS(&m_DefInputAndLineInput1.value.string, "?");
		InitScalarAsString(&m_DefScreen2);
		QsInitStaticNTS(&m_DefScreen2.value.string, "New Screen");
		InitScalarAsString(&m_DefWindow2);
		QsInitStaticNTS(&m_DefWindow2.value.string, "New Window");
		SetFromLong(&m_DefSound3, 127, T_INT);
		
		m_DefaultsInitialised = TRUE;
	}

	InitStatement(newStmt);
	
	if(command->formalCount == TOKENISED_ARGUMENTS)
		newStmt->method.macro = (void (*)(const QString *, unsigned))command->method;
	else
		newStmt->method.builtIn = (void (*)(BObject *, unsigned))command->method;
	newStmt->convert = command->convert;
	newStmt->inactive = command->inactive;
	newStmt->formal = command->formal;
	newStmt->formalCount = command->formalCount;
	newStmt->userDefined = FALSE;
	
	QsInitStaticNTS(&name, command->name);
	
	RequireSuccess(DefineSymbol(&name, newStmt, STATEMENT, SCOPE_BUILTIN));
}

void DefineBuiltInStatements(void)
{
	const struct BuiltInStatement *command;
	for(command = &m_StmtDefinitions[0]; command->method != NULL; command++)
		DefineBuiltInStatement(command);
}

bool AttemptToDefineBuiltInStatement(const QString *candidate)
{
	const struct BuiltInStatement *command;
	for(command = &m_StmtDefinitions[0]; command->method != NULL; command++) {
		QString name;
		QsInitStaticNTS(&name, command->name);
		if(QsEqNoCase(&name, candidate)) {
			DefineBuiltInStatement(command);
			return TRUE;
		}
	}
	return FALSE;
}

/* A dummy command which is executed for blank or comment-only statements.
For a very slight performance improvement, is a macro, and so does not evaluate 
and check its (nonexistent) parameters. */
void EmptyStatement_(const QString *toks, unsigned nToks)
{
	assert(nToks <= 1 && (nToks == 0 || IsTerminator(&toks[0])));
}

/* Translate a token from textual to internal form, without handling creation of
variables and other special side effects required for particular statements. */
void DefaultConvert(unsigned index, const QString *token, BObject *result)
{
	ConvertToObject(token, result, SCOPE_CURRENT);
}

/* Conversion for statements which don't, i.e. macros. */
void EmptyConvert(unsigned index, const QString *token, BObject *result)
{
}
