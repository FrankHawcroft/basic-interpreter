/****** sugar.c ******/

/*
	$VER: sugar.c 0.16A (5.14.2015)

	Deals with syntactic sugar - idiosyncrasies of BASIC which are annoying to parse,
	and can easily be transformed into more regular equivalents. This is done at the token
	sequence level, to simplify parsing and expression evaluation.
*/

#include <ctype.h>
#include "interpreter.h"
#include "builtin.h"

/* Internally rewritten keywords are suffixed with '~'. */
const char KW_PLUS[] = "+";
const char KW_MINUS[] = "-";
const char KW_UNAMBIGUOUS_UNARY_PLUS[] = "+u";
const char KW_UNAMBIGUOUS_UNARY_MINUS[] = "-u";
const char KW_LPAREN[] = "(";
const char KW_RPAREN[] = ")";
const char KW_COMMA[] = ",";
const char KW_EQUALS[] = "=";
const char KW_AREA[] = "AREA";
const char KW_CLOSE[] = "CLOSE";
const char KW_CONST[] = "CONST";
const char KW_DATA[] = "DATA";
const char KW_DEF[] = "DEF";
const char KW_DIM[] = "DIM";
const char KW_DISABLE[] = "DISABLE";
const char KW_ELSE[] = "ELSE";
const char KW_ELSEIF[] = "ELSEIF";
const char KW_EMPTY_STMT[] = "e~";
const char KW_ENABLE[] = "ENABLE";
const char KW_END[] = "END";
const char KW_FOR[] = "FOR";
const char KW_FPRINT[] = "FPRINT";
const char KW_GOSUB[] = "GOSUB";
const char KW_GOTO[] = "GOTO";
const char KW_IF[] = "IF";
const char KW_IFGOTO[] = "IFGOTO~";
const char KW_IFTHENELSE[] = "IFTHENELSE~";
const char KW_IFTHENLET[] = "IFTHENLET";
const char KW_INPUT[] = "INPUT";
const char KW_INSTR[] = "INSTR";
const char KW_LEN[] = "LEN";
const char KW_LET[] = "LET";
const char KW_LETQ_LOCAL[] = "LETQL~"; /* For cached statements - faster interning. */
const char KW_LETQ_PREDEF[] = "LETQP~"; /* For cached statements - faster interning. */
const char KW_MID[] = "MID";
const char KW_NAME[] = "NAME";
const char KW_NEXT[] = "NEXT";
const char KW_NEXTVAR[] = "NEXTVAR~";
const char KW_ON[] = "ON";
const char KW_ONGOSUB[] = "ONGOSUB~";
const char KW_ONGOTO[] = "ONGOTO~";
const char KW_OPEN[] = "OPEN";
const char KW_PRINT[] = "PRINT";
const char KW_RETURN[] = "RETURN";
const char KW_RETURNTO[] = "RETURNTO~";
const char KW_SCREEN[] = "SCREEN";
const char KW_STOP[] = "STOP";
const char KW_SUSPEND[] = "SUSPEND";

const QString g_LParen = {(char *)KW_LPAREN, 1};
const QString g_RParen = {(char *)KW_RPAREN, 1};
const QString g_DataKeyword = {(char *)KW_DATA, 4};
const QString g_ElseKeyword = {(char *)KW_ELSE, 4};
const QString g_EndKeyword = {(char *)KW_END, 3};
const QString g_GoToKeyword = {(char *)KW_GOTO, 4};

const QString g_Pipe = {"|", 1};
const QString g_Semicolon = {";", 1};
const QString g_AsKeyword = {"AS", 2};
const QString g_CallKeyword = {"CALL", 4};
const QString g_EndSubKeyword = {"ENDSUB", 6};
const QString g_StaticKeyword = {"STATIC", 6};
const QString g_SubKeyword = {"SUB", 3};
const QString g_ThenKeyword = {"THEN", 4};
const QString g_WhereKeyword = {"WHERE", 5};
const QString g_ScalarYieldingFunction = {"v~", 2};
const QString g_Missing = {"m~", 2};

static const QString m_Minus = {(char *)KW_MINUS, 1};
static const QString m_Plus = {(char *)KW_PLUS, 1};
static const QString m_UnambiguousUnaryPlus = {(char *)KW_UNAMBIGUOUS_UNARY_PLUS, 2};
static const QString m_UnambiguousUnaryMinus = {(char *)KW_UNAMBIGUOUS_UNARY_MINUS, 2};
static const QString m_Equals = {(char *)KW_EQUALS, 1};
static const QString m_Comma = {(char *)KW_COMMA, 1};
static const QString m_AreaKeyword = {(char *)KW_AREA, 4};
static const QString m_BaseKeyword = {"BASE", 4};
static const QString m_CloseKeyword = {(char *)KW_CLOSE, 5};
static const QString m_ConstKeyword = {(char *)KW_CONST, 5};
static const QString m_DefKeyword = {(char *)KW_DEF, 3};
static const QString m_DimKeyword = {(char *)KW_DIM, 3};
static const QString m_DimSharedKeyword = {"DIMSHARED", 9};
static const QString m_DisableKeyword = {(char *)KW_DISABLE, 7};
static const QString m_ElseIfKeyword = {(char *)KW_ELSEIF, 6};
static const QString m_EmptyStatement = {(char *)KW_EMPTY_STMT, 2};
static const QString m_EnableKeyword = {(char *)KW_ENABLE, 6};
static const QString m_ExitKeyword = {"EXIT", 4};
static const QString m_FillKeyword = {"FILL", 4};
static const QString m_ForKeyword = {(char *)KW_FOR, 3};
static const QString m_FPrintKeyword = {(char *)KW_FPRINT, 6};
static const QString m_FReadKeyword = {"FREAD", 5};
static const QString m_GoSubKeyword = {(char *)KW_GOSUB, 5};
static const QString m_IfKeyword = {(char *)KW_IF, 2};
static const QString m_IfGoToKeyword = {(char *)KW_IFGOTO, 7}; /* includes ~ */
static const QString m_IfThenElseKeyword = {(char *)KW_IFTHENELSE, 11}; /* includes ~ */
static const QString m_IfThenLetKeyword = {(char *)KW_IFTHENLET, 9};
static const QString m_InputKeyword = {(char *)KW_INPUT, 5};
static const QString m_InstrKeyword = {(char *)KW_INSTR, 5};
static const QString m_Instr2Keyword = {"INSTR2", 6};
static const QString m_LenKeyword = {(char *)KW_LEN, 3};
static const QString m_LetKeyword = {(char *)KW_LET, 3};
static const QString m_LetMidKeyword = {"LETMID", 6};
static const QString m_LSetKeyword = {"LSET", 4};
static const QString m_MidKeyword = {(char *)KW_MID, 3};
static const QString m_Mid2Keyword = {"MID2", 4};
static const QString m_NameKeyword = {(char *)KW_NAME, 4};
static const QString m_NextKeyword = {(char *)KW_NEXT, 4};
static const QString m_NextVarKeyword = {(char *)KW_NEXTVAR, 8}; /* includes ~ */
static const QString m_OffKeyword = {"OFF", 3};
static const QString m_OnKeyword = {(char *)KW_ON, 2};
static const QString m_OnGoSubKeyword = {(char *)KW_ONGOSUB, 8}; /* includes ~ */
static const QString m_OnGoToKeyword = {(char *)KW_ONGOTO, 7}; /* includes ~ */
static const QString m_OpenKeyword = {(char *)KW_OPEN, 4};
static const QString m_OptionKeyword = {"OPTION", 6};
static const QString m_OutputKeyword = {"OUTPUT", 6};
static const QString m_PrintKeyword = {(char *)KW_PRINT, 5};
static const QString m_ResumeKeyword = {"RESUME", 6};
static const QString m_ReturnKeyword = {(char *)KW_RETURN, 6};
static const QString m_ReturnToKeyword = {(char *)KW_RETURNTO, 9}; /* includes ~ */
static const QString m_RSetKeyword = {"RSET", 4};
static const QString m_ScreenKeyword = {(char *)KW_SCREEN, 6};
static const QString m_SharedKeyword = {"SHARED", 6};
static const QString m_SoundKeyword = {"SOUND", 5};
static const QString m_StepKeyword = {"STEP", 4};
static const QString m_StopKeyword = {(char *)KW_STOP, 4};
static const QString m_SuspendKeyword = {(char *)KW_SUSPEND, 7};
static const QString m_ToKeyword = {"TO", 2};
static const QString m_WaitKeyword = {"WAIT", 4};
static const QString m_WindowKeyword = {"WINDOW", 6};
static const QString m_WindowInfoKeyword = {"WINDOWINFO", 10};
static const QString m_QuotedTab = {"\"\t\"", 3};
static const QString m_QuotedLinefeed = {"\"\n\"", 3};
static const QString m_QuestionMark = {"?", 1};
static const QString m_Zero = {"0", 1};

bool IsParameterSeparatingKeyword(const QString *token)
{
	return QsEqNoCase(token, &m_ToKeyword)
		|| QsEqNoCase(token, &m_StepKeyword)
		|| QsEqNoCase(token, &g_AsKeyword)
		|| QsEqNoCase(token, &g_GoToKeyword);
}

static bool UsesParameterSeparatingKeywords(const QString *stmt)
{
	return QsEqNoCase(stmt, &m_ForKeyword)
		|| QsEqNoCase(stmt, &m_IfKeyword)
		|| QsEqNoCase(stmt, &m_ElseIfKeyword)
		|| QsEqNoCase(stmt, &m_IfGoToKeyword)
		|| QsEqNoCase(stmt, &m_NameKeyword);
}

static bool IsAppliedName(const QString *token)
{
	return isalpha(QsGetFirst(token)) && !IsParameterSeparatingKeyword(token);
}

static bool IsPrintParameterSeparator(char token)
{
	return token == ',' || token == ';';
}

static bool TerminatesStatementParameter(char token)
{
	return token == ';' || token == '|';
}

static bool IsPrint(const QString *stmt)
{
	return QsEqNoCase(stmt, &m_PrintKeyword) || QsEqNoCase(stmt, &m_FPrintKeyword);
}

static bool IsArrayCreator(const QString *stmt)
{
	return QsEqNoCase(stmt, &m_DimKeyword) || QsEqNoCase(stmt, &m_DimSharedKeyword);
}

static bool NonMacroHasParenthesisedParameterList(const QString *stmt)
{
	return IsArrayCreator(stmt) || QsEqNoCase(stmt, &g_CallKeyword);
}

static bool IsSimpleWord(const QString *token)
{
	return isalpha(QsGetFirst(token)) && isalpha(QsGetLast(token));
}

static bool IsTwoWordForm(const QString *stmt, const QString *following)
{
	static const QString *canBeSeparated[][2] = {
		{ &m_AreaKeyword, &m_FillKeyword },
		{ &m_AreaKeyword, &m_StepKeyword },
		{ &m_DimKeyword, &m_SharedKeyword },
		{ &g_EndKeyword, &m_IfKeyword },
		{ &g_EndKeyword, &g_SubKeyword },
		{ &m_ExitKeyword, &g_SubKeyword },
		{ &m_LetKeyword, &m_MidKeyword },
		{ &m_OptionKeyword, &m_BaseKeyword },
		{ &m_ScreenKeyword, &m_CloseKeyword },
		{ &m_SoundKeyword, &m_ResumeKeyword },
		{ &m_SoundKeyword, &m_WaitKeyword },
		{ &m_WindowKeyword, &m_CloseKeyword },
		{ &m_WindowKeyword, &m_OutputKeyword },
		{ NULL, NULL }
	};
	int i;
	
	if(!IsSimpleWord(stmt) || !IsSimpleWord(following))
		return FALSE;
	
	/*const QString *item = bsearch(stmt, canBeSeparated, 13, sizeof(canBeSeparated[0]), (int (*)(const void *, const void *))&QsCompare);*/
	
	for(i = 0; canBeSeparated[i][0] != NULL
		&& !(QsEqNoCase(canBeSeparated[i][0], stmt) && QsEqNoCase(canBeSeparated[i][1], following)); i++)
		;
	return canBeSeparated[i][0] != NULL;
	
	/*return ((QsEqNoCase(stmt, &g_EndKeyword) || QsEqNoCase(stmt, &m_ExitKeyword))
	    && (QsEqNoCase(following, &m_IfKeyword) || QsEqNoCase(following, &g_SubKeyword)))
	 || (QsEqNoCase(stmt, &m_OptionKeyword) && QsEqNoCase(following, &m_BaseKeyword))
	 || (QsEqNoCase(stmt, &m_DimKeyword) && QsEqNoCase(following, &m_SharedKeyword))
	 || (QsEqNoCase(stmt, &m_LetKeyword) && QsEqNoCase(following, &m_MidKeyword))
	 || (QsEqNoCase(stmt, &m_SoundKeyword)
			&& (QsEqNoCase(following, &m_WaitKeyword) || QsEqNoCase(following, &m_ResumeKeyword))); */
	 /* TODO LINE INPUT, LINE STEP, AREA STEP, etc. */
}

static void ShiftTokens(struct TokenSequence *ts, int index, int gap)
{
	int i;

	assert(gap > 0); /* use DeleteToken instead */
	assert(index + gap >= 0);
	
	ExpandTokenSequence(ts, ts->length + gap);

	/* Ensure tokens at top are clear before overwriting - paranoia ... */
	for(i = ts->length; i < ts->length + gap; i++)
		QsInitNull(&ts->rest[i]);

	/* Now shuffle: */
	for(i = ts->length + gap - 1; i > index + gap; i--)
		QsCopy(&ts->rest[i], &ts->rest[i - gap]);
	ts->length += gap;
}

/* ... (with no detected statement) --> LET ...
   <EOS> --> e~
*/
static void SetDefaultStatement(QString *stmt, unsigned short nTokens)
{
	if(QsIsNull(stmt))
		QsCopy(stmt, nTokens >= 2 ? &m_LetKeyword : &m_EmptyStatement);
}

/* NEXT i[, j ...] --> NEXTVAR~ ...
   ? ... --> PRINT ...
   RETURN label --> RETURNTO~ label
*/
static void StandardiseSimpleStatementSynonym(QString *stmt, unsigned short nTokens)
{
	if(nTokens >= 2 && QsEqNoCase(stmt, &m_NextKeyword))
		QsCopy(stmt, &m_NextVarKeyword);
	else if(QsEqNoCase(stmt, &m_QuestionMark))
		QsCopy(stmt, &m_PrintKeyword);
	else if(nTokens >= 2 && QsEqNoCase(stmt, &m_ReturnKeyword))
		QsCopy(stmt, &m_ReturnToKeyword);
}

/* PRINT <expr1>, <expr2> --> PRINT <expr1>; <tab>; <expr2>
	etc.
*/
static void ExpandPrintParameters(struct TokenSequence *ts)
{
	if(!IsPrint(&ts->statementName))
		return;
	
	/* Make implied LF at end explicit - */
	if(ts->length <= 1 || !IsPrintParameterSeparator(QsGetFirst(&ts->rest[ts->length - 2]))) {
		ExpandTokenSequence(ts, ts->length + 2);
		ts->length += 2;
		QsCopyChar(&ts->rest[ts->length - 3], ';');
		QsCopy(&ts->rest[ts->length - 2], &m_QuotedLinefeed);
		QsCopyChar(&ts->rest[ts->length - 1], '|');
	}
	
	/* The comma separator produces a tab - */
	{
		int nestLevel = 0;
		unsigned short i;
		bool firstComma = TRUE;
		
		for(i = 0; i != ts->length; i++) {
			Nest(&ts->rest[i], &nestLevel);
			
			if(nestLevel == 0 && QsEqNoCase(&ts->rest[i], &m_Comma)) {
				if(!(QsEqNoCase(&ts->statementName, &m_FPrintKeyword) && firstComma)) {
					ShiftTokens(ts, i, 2);
					QsCopy(&ts->rest[i], &g_Semicolon); /* overwrites ',' */
					QsCopy(&ts->rest[i + 1], &m_QuotedTab);
					QsCopy(&ts->rest[i + 2], &g_Semicolon);
				}
				firstComma = FALSE;
			}
		}
	}
}

static void ReplaceEqualsUsedAsSeparator(QString *tok, unsigned short limit, const QString *replacement)
{
	int nestLevel = 0;
	unsigned short i;
	
	for(i = 0; i != limit; i++) {
		Nest(&tok[i], &nestLevel);
			/* Checking the sanity of the nest level is overkill in the context of this module. */

		if(nestLevel == 0 && QsGetFirst(&tok[i]) == '=') {
			QsCopy(&tok[i], replacement);
			return;
		}
	}
}

/* IF <expr> THEN <stmt> [ELSE <stmt>] --> IFTHENELSE~ ...
   IF <expr> GOTO <label> --> IFGOTO~ ...
   IF <expr> THEN <var> = <expr> --> IFTHENLET~ <expr>; <var>; <expr>
*/
static void MakeShortIfStatementExplicit(struct TokenSequence *ts)
{
	if(ts->length > 3
	&& QsEqNoCase(&ts->statementName, &m_IfKeyword)
	&& !QsEqNoCase(&ts->rest[ts->length - 2], &g_ThenKeyword)) { /* THEN at end of line in a block IF is left alone */
		unsigned short i;
	
		/* Scan backwards so that ELSE is detected before 'IF cond THEN x = ...' or 'IF cond GOTO label' */
		for(i = ts->length - 2; i != 0; i--) {
			if(QsEqNoCase(&ts->rest[i], &g_ElseKeyword)) {
				QsCopy(&ts->statementName, &m_IfThenElseKeyword);
				return;
			}
			else if(QsEqNoCase(&ts->rest[i], &g_ThenKeyword)) {
				const BObject *following = LookUp(&ts->rest[i + 1], SCOPE_CURRENT);
				if(following != NULL && IsVariable(following)) {
					/* TODO detect explicit LET as well */
					QsCopy(&ts->statementName, &m_IfThenLetKeyword);
					QsCopy(&ts->rest[i], &g_Semicolon); /* replace THEN */
					ReplaceEqualsUsedAsSeparator(&ts->rest[i + 1], ts->length - 2 - i, &g_Semicolon);
				}
				else
					QsCopy(&ts->statementName, &m_IfThenElseKeyword);
				return;
			}
			else if(QsEqNoCase(&ts->rest[i], &g_GoToKeyword)) {
				QsCopy(&ts->statementName, &m_IfGoToKeyword);
				return;
			}
		}
	}
}

/* OPEN "name" FOR <mode> AS #n [LEN=size] --> OPEN "mode", n, "name"[, size] */
static void TranslateOldStyleOpen(struct TokenSequence *ts)
{
	if(ts->length >= 6
	&& QsEqNoCase(&ts->statementName, &m_OpenKeyword)
	&& QsEqNoCase(&ts->rest[1], &m_ForKeyword)
	&& QsEqNoCase(&ts->rest[3], &g_AsKeyword)) {
		QString name, mode, number, bufSize;
		
		QsCopy(&name, &ts->rest[0]);
		QsCopy(&mode, &ts->rest[2]);
		QsCopy(&number, &ts->rest[4]);
		if(ts->length > 8 && QsEqNoCase(&ts->rest[5], &m_LenKeyword))
			QsCopy(&bufSize, &ts->rest[7]);
		else
			QsInitNull(&bufSize);
		
		QsCopyChar(&ts->rest[0], '\"');
		QsAppendChar(&ts->rest[0], QsGetCharAt(&mode, 0)); /* Suffices for the allowed modes. */
		QsAppendChar(&ts->rest[0], '\"');
		QsCopy(&ts->rest[1], &m_Comma);
		QsCopy(&ts->rest[2], &number);
		QsCopy(&ts->rest[3], &m_Comma);
		QsCopy(&ts->rest[4], &name);
		if(!QsIsNull(&bufSize)) {
			QsCopy(&ts->rest[5], &m_Comma);
			QsCopy(&ts->rest[6], &bufSize);
			QsCopy(&ts->rest[7], &g_Pipe);
			ts->length = 8;
		}
		else {
			QsCopy(&ts->rest[5], &g_Pipe);
			ts->length = 6;
		}
	}
}

/* LET x[(...)] = ... --> LET x[(...)]; ...
	etc.
*/
static void StandardiseEqualsInAssignment(struct TokenSequence *ts)
{
	if(ts->length >= 3) {
		if(QsEqNoCase(&ts->statementName, &m_LetKeyword)
		|| QsEqNoCase(&ts->statementName, &m_ConstKeyword)
		|| QsEqNoCase(&ts->statementName, &m_ForKeyword)
		|| QsEqNoCase(&ts->statementName, &m_LSetKeyword)
		|| QsEqNoCase(&ts->statementName, &m_RSetKeyword)
		|| QsEqNoCase(&ts->statementName, &m_LetMidKeyword))
			ReplaceEqualsUsedAsSeparator(ts->rest, ts->length, &g_Semicolon);
	}
}

/* DEF x[(...)] = <expr> --> DEF x[(...)] AS <expr> */
static void TranslateOldStyleFunctionDefinition(struct TokenSequence *ts, bool functionDefinition)
{
	if(functionDefinition && ts->length >= 5) {
		unsigned short i;
		bool newStyle = FALSE;
		
		for(i = 0; i != ts->length && !newStyle; i++)
			newStyle = QsEqNoCase(&ts->rest[i], &g_WhereKeyword) || QsEqNoCase(&ts->rest[i], &g_AsKeyword);
		
		if(!newStyle)
			ReplaceEqualsUsedAsSeparator(ts->rest, ts->length, &g_AsKeyword);
	}
}

/* DIM a(...) --> DIM a; ... 
   CALL s(...) --> CALL s; ...	'' first transformation for CALL */
static void RemoveWrappingParentheses(struct TokenSequence *ts)
{
	if(ts->length >= 3
	&& QsGetFirst(&ts->rest[1]) == '('
	&& QsGetFirst(&ts->rest[ts->length - 2]) == ')'
	&& NonMacroHasParenthesisedParameterList(&ts->statementName)
	&& IsAppliedName(&ts->rest[0])) {
		QsCopy(&ts->rest[1], &g_Semicolon);
		DeleteToken(ts, ts->length - 2);
	}
}

/* CALL s; [...] --> s ...	'' second transformation for CALL */
static void TranslateOldStyleSubprogramCall(struct TokenSequence *ts)
{
	if(ts->length > 1 && QsEqNoCase(&ts->statementName, &g_CallKeyword)) {
		QsCopy(&ts->statementName, &ts->rest[0]);
		DeleteToken(ts, 0);
		if(ts->length > 2)
			DeleteToken(ts, 0);
	}
}

/* IF <expr> THEN --> IF <expr> 

Only for the block form - not the IF ... THEN ... ELSE on one line form, which must be detected earlier! */
static void RemoveRedundantTrailer(struct TokenSequence *ts)
{
	if(ts->length > 1
	&& (QsEqNoCase(&ts->statementName, &m_IfKeyword)
		|| QsEqNoCase(&ts->statementName, &m_ElseIfKeyword))
	&& QsEqNoCase(&ts->rest[ts->length - 2], &g_ThenKeyword))
		DeleteToken(ts, ts->length - 2);
}

/* END IF --> ENDIF
	etc.
*/
static void ConcatenateTwoWordForm(struct TokenSequence *ts)
{
	if(ts->length > 1 && IsTwoWordForm(&ts->statementName, &ts->rest[0])) {
		QsAppend(&ts->statementName, &ts->rest[0]);
		DeleteToken(ts, 0);
	}
}

/* <eventspec> {ON|OFF|STOP} --> {ENABLE|DISABLE|SUSPEND} <eventspec> */
static void TranslateEventTrappingControl(struct TokenSequence *ts)
{
	if(ts->length == 2 && ValidEventName(&ts->statementName)) {
		if(QsEqNoCase(&ts->rest[0], &m_OnKeyword)) {
			QsCopy(&ts->rest[0], &ts->statementName);
			QsCopy(&ts->statementName, &m_EnableKeyword);
		}
		else if(QsEqNoCase(&ts->rest[0], &m_OffKeyword)) {
			QsCopy(&ts->rest[0], &ts->statementName);
			QsCopy(&ts->statementName, &m_DisableKeyword);
		}
		else if(QsEqNoCase(&ts->rest[0], &m_StopKeyword)) {
			QsCopy(&ts->rest[0], &ts->statementName);
			QsCopy(&ts->statementName, &m_SuspendKeyword);
		}
	}
}

/* ON <expr> GOTO <label1>[, <label2> ...] --> ONGOTO~ <expr>, <label1>[, <label2> ...]
ON <expr> GOSUB <label1>[, <label2> ...] --> ONGOSUB~ <expr>, <label1>[, <label2> ...]
ON <eventspec> GOTO 0 --> DISABLE <eventspec> */
static void TranslateComputedJump(struct TokenSequence *ts)
{
	if(ts->length >= 2 && QsEqNoCase(&ts->statementName, &m_OnKeyword)) {
		if(!ValidEventName(&ts->rest[0])) {
			unsigned short i;
			for(i = 0; i != ts->length; i++) {
				if(QsEqNoCase(&ts->rest[i], &g_GoToKeyword)) {
					QsCopy(&ts->statementName, &m_OnGoToKeyword);
					QsCopy(&ts->rest[i], &m_Comma);
				}
				else if(QsEqNoCase(&ts->rest[i], &m_GoSubKeyword)) {
					QsCopy(&ts->statementName, &m_OnGoSubKeyword);
					QsCopy(&ts->rest[i], &m_Comma);
				}
			}
		}
		else if(QsEqNoCase(&ts->rest[1], &g_GoToKeyword) && QsEqual(&ts->rest[2], &m_Zero)) {
			QsCopy(&ts->statementName, &m_DisableKeyword);
			DeleteToken(ts, 1); DeleteToken(ts, 1);
		}
	}
}

/* + <expr> --> +u <expr>
   - <expr> --> -u <expr>

Only unary (vs binary) '+' and '-' are dealt with. Other overloadings
(e.g. '+' as string concatenation or numeric addition) are easily dealt
with when type information is available. Transforming unary + and - here
saves complexity in the symbol table and in expression parsing, and can
be done simply based on lexical context. */
static void ResolveOverloadedOperator(QString *token, const QString *leftContext)
{
	char first = QsGetFirst(token);
	if(((first == '-' || first == '+') && QsGetLength(token) == 1)
	&& !IsNumeric(leftContext) && !IsQuotedLiteral(leftContext) && !QsEqNoCase(leftContext, &g_RParen)
	&& !IsAppliedName(leftContext))
		QsCopy(token, QsEqNoCase(token, &m_Plus) ? &m_UnambiguousUnaryPlus : &m_UnambiguousUnaryMinus);
}

/* ( <expr> ) --> v~( <expr> )

This simplifies parsing. */
static void InsertDummyFunctionInvocation(struct TokenSequence *ts, int index, bool sub)
{
	if(!sub
	&& QsGetFirst(&ts->rest[index]) == '('
	&& (index == 0 || !IsAppliedName(&ts->rest[index - 1]))) {
		EnsureExistsIfBuiltIn(&g_ScalarYieldingFunction);
		
		ShiftTokens(ts, index, 1);
		QsCopy(&ts->rest[index], &g_ScalarYieldingFunction);
		QsCopy(&ts->rest[index + 1], &g_LParen);
	}
}

/* <expr1>, <expr2> --> <expr1>; <expr2>		'' for statement parameters only, not sub-expressions
   NAME <expr1> AS <expr2> --> NAME <expr1>; <expr2>
	etc.
*/
static void StandardiseSeparator(QString *token, int nestLevel, bool considerKeywords, bool macro)
{
	if(nestLevel == 0
	&& ((!macro && QsGetFirst(token) == ',') || (considerKeywords && IsParameterSeparatingKeyword(token))))
		QsCopy(token, &g_Semicolon);
}

/* INPUT(...) --> FREAD(...)
   WINDOW(...) --> WINDOWINFO(...)
   WINDOWINFO(<expr1>) --> WINDOWINFO1(<expr1>) '' TODO
   INSTR(<expr1>, <expr2>) --> INSTR2(<expr1>, <expr2>)
   MID(<expr1>, <expr2>) --> MID2(<expr1>, <expr2>)
   MID$(...) --> MID(...) etc.
*/
static void RenameFunction(QString *token, bool functionDefinition, unsigned short idx, unsigned short limit)
{
	if(idx + 1 >= limit || (idx == 0 && functionDefinition)
	|| QsGetFirst(token + 1) != '(' || !isalpha(QsGetFirst(token)))
		return;
	
	if(QsGetLast(token) == '$') {
		/* Traditional string function syntax - e.g. LEFT$(s$, 2)
			Remove the '$' to avoid special-case checks later. */
		const BObject *defn;
		QString typeless;
		
		QsGetSubstring(&typeless, token, 0, QsGetLength(token) - 1);
		defn = LookUp(&typeless, SCOPE_GLOBAL);
		if(defn != NULL && defn->category == FUNCTION
		&& (defn->value.function->type & (T_STRING | T_CHAR))) {
			QsDispose(token);
			QsCopy(token, &typeless);
		}
		QsDispose(&typeless);
	}
	
	if(QsEqNoCase(token, &m_InputKeyword))
		QsCopy(token, &m_FReadKeyword);
	else if(QsEqNoCase(token, &m_WindowKeyword))
		QsCopy(token, &m_WindowInfoKeyword);
	else if(QsEqNoCase(token, &m_InstrKeyword) || QsEqNoCase(token, &m_MidKeyword)
		/* || QsEqNoCase(token, &m_WindowInfoKeyword) */) {
		int relNesting = 0, paramCount = 0;
		QString *scan;
		
		for(scan = token + 1, ++idx; relNesting >= 0 && idx < limit; scan++, idx++) {
			Nest(scan, &relNesting);
			paramCount += relNesting == 1 && (QsGetFirst(scan) == '(' || QsGetFirst(scan) == ',');	
		}
		
		if(paramCount == 2)
			QsCopy(token, QsEqNoCase(token, &m_InstrKeyword) ? &m_Instr2Keyword : &m_Mid2Keyword);
	}
}

/* It's necessary to insert special placeholders for missing parameters because
parameter separators (semicolons) are removed when converting infix to prefix. 
The actual default values could be inserted, but that would entail converting to string
form and back again, and this approach has the virtue of simplicity. */
static void InsertPlaceholder(struct TokenSequence *ts, int index, int *numActualParams)
{
	bool endsParameter = TerminatesStatementParameter(QsGetFirst(&ts->rest[index]));

	*numActualParams += endsParameter;
	if(endsParameter 
	  && (index == 0 || (index > 0 && QsGetFirst(&ts->rest[index - 1]) == ';'))) {
		ShiftTokens(ts, index - 1, 1);
		QsCopy(&ts->rest[index], &g_Missing);
	}
}

static void AppendPlaceholders(struct TokenSequence *ts, int numActualParams, int minActuals)
{
	while(numActualParams < minActuals) {
		ExpandTokenSequence(ts, ts->length + 2);
		if(ts->length > 1 && QsGetFirst(&ts->rest[ts->length - 2]) != ';') {
			QsCopy(&ts->rest[ts->length - 1], &g_Semicolon);
			++ts->length;
		}
		QsCopy(&ts->rest[ts->length - 1], &g_Missing);
		QsCopy(&ts->rest[ts->length], &g_Pipe);
		++ts->length;
		numActualParams++;
	}
}

/* Cheats ... semantic infection, but better than the alternative complicated ways of dealing with default values.*/
static int MinActuals(const BObject *cmd)
{
	int n = 0;
	
	if(cmd != NULL && cmd->category == STATEMENT && cmd->value.statement->formalCount > 0) {
		const struct Parameter *p;
		for(p = cmd->value.statement->formal; p < &cmd->value.statement->formal[cmd->value.statement->formalCount]; p++)
			n += p->defaultValue == NULL && p->maxCount < MAX_TOKENS ? p->maxCount : 1;
	}
	
	return n;
}

static bool MacroInvocation(const BObject *cmd)
{
	return cmd != NULL && cmd->category == STATEMENT && IsMacro(cmd->value.statement);
}

/*static void ProcessNestedStatements(struct TokenSequence *ts)
{
	if(ts->length > 4 && QsEqNoCase(&ts->statementName, &m_IfThenElseKeyword)) {
			unsigned thenPosition = 0, elsePosition = 0, scan;
	Error error = SUCCESS;
	
	for(scan = 0; scan != nToks; scan++) {
		if(thenPosition == 0 && QsEqNoCase(&toks[scan], &g_ThenKeyword))
			thenPosition = scan;
		else if(elsePosition == 0 && QsEqNoCase(&toks[scan], &g_ElseKeyword))
			elsePosition = scan;
	}
	
	if(thenPosition == 0 || (elsePosition != 0 && elsePosition < thenPosition))
		error = BADSYNTAX;
	}
}*/

void MakeSavoury(struct TokenSequence *ts)
{
	bool def = QsEqNoCase(&ts->statementName, &m_DefKeyword);
	
	/* Trying to finalise the statement name is a good thing to do first,
		although it will need to be adjusted later on in some situations - */
		
	SetDefaultStatement(&ts->statementName, ts->length);

	StandardiseSimpleStatementSynonym(&ts->statementName, ts->length);

	/* TODO InsertMissingPrintSeparators -- so that loose syntax like 'PRINT a "xyz" 123' works,
		though probably want this to be disallowed by default -- only allowed in (TODO also)
		--archaic mode */
		
	ExpandPrintParameters(ts);
		
	TranslateOldStyleOpen(ts);
	
	TranslateOldStyleFunctionDefinition(ts, def);
	
	/* Must happen before RemoveRedundantTrailer - */
	MakeShortIfStatementExplicit(ts);
	
	RemoveRedundantTrailer(ts);
	
	ConcatenateTwoWordForm(ts);
	
	StandardiseEqualsInAssignment(ts);
	
	/* Must happen before TranslateOldStyleSubprogramCall - */
	RemoveWrappingParentheses(ts);
	
	TranslateOldStyleSubprogramCall(ts);
		
	/* TODO file statements and functions with '#': PRINT#1 etc. */
	
	TranslateEventTrappingControl(ts);
	
	TranslateComputedJump(ts);
	
	EnsureExistsIfBuiltIn(&ts->statementName);
	
	{
		int nestLevel = 0;
		unsigned short i;
		const BObject *cmd = LookUp(&ts->statementName, SCOPE_GLOBAL);
		bool usesKeywordSeparators = ts->length > 1 && UsesParameterSeparatingKeywords(&ts->statementName);
		bool macro = MacroInvocation(cmd);
		bool sub = QsEqNoCase(&ts->statementName, &g_SubKeyword);
		
		/* Grab the statement if found - saves an additional look up by the caller. */
		if(cmd != NULL && cmd->category == STATEMENT)
			ts->command = cmd->value.statement;
		
		for(i = 0; i != ts->length; i++) {
			{
				QString *tok = &ts->rest[i];
				
				RenameFunction(tok, def, i, ts->length);
				
				ResolveOverloadedOperator(tok, i == 0 ? &m_Comma : tok - 1);
				
				Nest(tok, &nestLevel);
					/* Checking the sanity of the nest level is overkill in the context of this module. */
			
				EnsureExistsIfBuiltIn(tok);
				
				StandardiseSeparator(tok, nestLevel, usesKeywordSeparators, macro);
			}
		
			/* This may invalidate tokens, so do it last - */
			InsertDummyFunctionInvocation(ts, i, sub);
		}

		/* Standardise the parameter terminator - */
		if(ts->length != 0 && IsTerminator(&ts->rest[ts->length - 1]))
			QsCopy(&ts->rest[ts->length - 1], &g_Pipe);
		
		/* Add placeholders for defaults. */
		if(!macro) {
			int numActuals = 0;
			
			if(ts->length > 1)
				for(i = 0; i < ts->length; i++)
					InsertPlaceholder(ts, i, &numActuals);

			AppendPlaceholders(ts, numActuals, MinActuals(cmd));
		}
	}
}
