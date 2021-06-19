/****** lexer.c ******/

/*
	$VER: lexer.c 0.16 (7.18.2011)

	Tokenisation and lexical analysis.
*/

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include "interpreter.h"
#include "process.h"
#include "heap.h"

#if defined(VBCC) || defined(__clang__)
extern int strnicmp(const char *, const char *, size_t);
#endif

static const QString m_Rem = {"REM", 3};

static bool IsLabelFirst(char c) { return isalpha(c) != 0; }

static bool IsNameChar(char c) { return isalnum(c) || c == '_' || c == '.'; }

static bool IsDecimalNumConstFirst(char c) { return isdigit(c) || c == '.'; }

static bool IsUnambiguousNumConstFirst(char c) { return IsDecimalNumConstFirst(c) || c == '#'; }

static bool IsAmbiguousPrefix(char c) { return c == '&' || c == '+' || c == '-'; }

static bool IsNumConstFirst(char c) { return IsUnambiguousNumConstFirst(c) || IsAmbiguousPrefix(c); }

static bool IsSimpleOperator(char c) { return c == '=' || c == '(' || c == ')' || c == '*' || c == '/' 
										|| c == '<' || c == '>' || c == '^' || c == '\\' 
										|| IsAmbiguousPrefix(c); }

static bool IsSimpleDelimiter(char c) { return c == ',' || c == ';' || IsSimpleTerminator(c); } /* i.e. delimits parameters */

bool IsSimpleTerminator(char c) { return c == '\n' || c == '\'' || c == NUL || c == '|' || c == ':'; }

bool IsWhiteSpace(char c) { return c == ' ' || c == '\t'; }

bool IsTerminator(const QString *t)
{
	return (QsGetLength(t) == 1 && IsSimpleTerminator(QsGetFirst(t))) || QsEqNoCase(t, &m_Rem);
}

bool IsSeparator(const QString *t)
{
	return QsGetFirst(t) == ';' || QsGetFirst(t) == ',';
}

static bool StartsWithRem(const char *s) { return strnicmp(s, QsGetData(&m_Rem), 3) == 0; }

bool IsLParen(const QString *t)
{
	return QsEqNoCase(t, &g_LParen);
}

bool IsRParen(const QString *t)
{
	return QsEqNoCase(t, &g_RParen);
}

bool IsLiteral(const QString *t)
{
	return IsQuotedLiteral(t) || IsNumeric(t);
}

/* This is not a thorough test - assumes the token has been correctly scanned. */
bool IsNumeric(const QString *t)
{
	assert(!QsIsNull(t));
	
	char c = QsGetFirst(t);
	return IsNumConstFirst(c) 
		&& !(IsAmbiguousPrefix(c) && (QsGetLength(t) == 1 || QsGetCharAt(t, 1) == 'u'));
}

bool IsName(const QString *t)
{
	return isalpha(QsGetFirst(t)) != 0;
}

/* Assumes the token has been scanned - checks for specific 'reserved' tokens. */
bool IsValidName(const QString *t)
{
	assert(!QsIsNull(t));

	return IsName(t) && !QsEqNoCase(t, &g_Missing) && !QsEqNoCase(t, &g_ScalarYieldingFunction);
}

bool IsValidLabelOrLineNumber(const QString *t)
{
	return IsValidName(t) || isdigit(QsGetFirst(t));
}

static bool IsCommentMarker(const QString *t)
{
	return QsGetFirst(t) == '\'' || (QsGetLength(t) > 1 && IsTerminator(t));
}

static bool CanPrecedeNumericalConstant(const QString *t)
{
	return QsIsNull(t)
		|| IsSimpleDelimiter(QsGetFirst(t))
		|| IsParameterSeparatingKeyword(t)
		|| (QsGetFirst(t) != '=' && ResolveOperator(t) != NULL);
}

static bool AttemptToScanAsNumeric(char c, char next, const QString *leftContext)
{
	if(c == '+' || c == '-')
		return IsDecimalNumConstFirst(next) && CanPrecedeNumericalConstant(leftContext);
	else if(c == '&') /* Generally, spaces must be used around & - &h, &o, &d where d in 0..9, can all introduce a constant.
					  Safer to discourage not having spaces. */
		return isalnum(next) != 0;
	else
		return IsUnambiguousNumConstFirst(c);
}

/* Advances past blanks, tabs. */
static void SkipWhiteSpace(const char **position)
{
	const char *p = *position;
	char c = *p;
	
	while(IsWhiteSpace(c))
		c = *++p;
	*position = p;
}

/* Advances _one_past_ a newline, or _onto_ the buffer-terminating NUL. */
static void SkipToNextLine(const char **position)
{
	do
		++*position;
	while((*position)[-1] != NUL && (*position)[-1] != '\n');

	if((*position)[-1] == NUL)
		--*position;
}

/* Skip any comment following the continuation character _, and any lines following that contain only continuations and/or comments.  */
static Error SkipContinuation(const char **position)
{
	while(**position == '_') {
		++*position; /* Move off _ */
		SkipWhiteSpace(position);

		if(**position == '\'' || (StartsWithRem(*position) && !IsNameChar((*position)[3]))) {
			SkipToNextLine(position);
			SkipWhiteSpace(position);
		}
		else if(**position == '\n') {
			++*position;
			SkipWhiteSpace(position);
		}
		else if(**position != NUL)
			return BADSYNTAX;
			/* Nothing else is allowed to follow the _.
				 Doesn't move off the NUL. This is always significant and 
				 lexer shouldn't go past the end of the buffer. */
	}
	return SUCCESS;
}
 
/* All these 'Get' functions advance the position to one past the end of the token.
They don't alter the token passed if an optional item isn't found. */

static void GetOptionalLineNumberToken(const char **position, QString *t)
{
	if(isdigit(**position)) {
		const char *rememberStart = *position;

		do
			++*position;
		while(isdigit(**position));

		/* If followed by a colon, it's a label rather than a line number. */
		if(IsWhiteSpace(**position) || (IsSimpleTerminator(**position) && **position != ':'))
			QsInitStaticPtrs(t, rememberStart, *position - 1);
				/* Don't include the terminating space etc. */
		else /* Not a line number, restore. */
			*position = rememberStart;
	}
}

static void GetOptionalLabelToken(const char **position, QString *t)
{
	if(IsLabelFirst(**position)) {
		const char *rememberStart = *position;

		do
			++*position;
		while(IsNameChar(**position));

		/* Might be a statement or variable name: */
		if(**position == ':' && !StartsWithRem(rememberStart)) {
			/* This is a valid label name. */
			QsInitStaticPtrs(t, rememberStart, *position - 1);
				/* Don't include the ':' ... */
			++*position; /* ... but don't re-scan it as a terminator. */
		}
		else /* Not a label, restore. */
			*position = rememberStart;
	}
}

/* Unfortunately, this function means the lexer is not independent of runtime state,
because it looks up the token so that where 'a' is a known variable, a statement like
	a(i) = <tokens> 
will be treated as an assignment (LET) statement. During the syntax checking pass, 
this detection won't work and 'a' will be treated as the statement name, but this
doesn't cause erroneous syntax errors because the syntax-checking algorithm
is sufficiently forgiving.

Statements of the form
	x = <tokens>
are always treated as assignment statements based on a look ahead for '='.*/
static void GetOptionalStatementToken(const char **position, QString *t)
{
	if(**position == '?') {
		QsInitStatic(t, *position, 1);
		++*position;
	}
	else if(isalpha(**position)) {
		BObject *defn;
		const char *rememberStart = *position;
		bool hasTypeSpecifier, assignmentOperator, leftParen;

		do
			++*position;
		while(IsNameChar(**position));
		
		QsInitStaticPtrs(t, rememberStart, *position - 1);
			/* Don't need to remember the space or whatever after the token. */
			
		hasTypeSpecifier = IsTypeSpecifier(**position);

		/* Scan forward looking for '=', which also indicates assignment. */
		SkipWhiteSpace(position);
		assignmentOperator = **position == '=';
		leftParen = **position == '(';
		
		EnsureExistsIfBuiltIn(t);
		
		/* If it comes to checking the symtab, only variables are exempt. 
		If a function, label, operator, etc. is looked up, assume a  mistyped stmt name, 
		omitted : after an intended label, etc., and treat the token as a statement anyway. */
		if(hasTypeSpecifier
		|| assignmentOperator
		|| QsEqNoCase(t, &m_Rem)
		|| ((defn = LookUp(t, Proc()->callNestLevel)) != NULL && IsVariable(defn))
		|| (leftParen && (defn == NULL || defn->category != STATEMENT))) {
			/* Not a statement name. Restore position and token. */
				
			QsInitNull(t);
			*position = rememberStart;
		}
	}
}

static void GetIdentifierToken(const char **position, QString *t)
{
	const char *start = *position;

	do
		++*position;
	while(IsNameChar(**position));
	
	/* Variable and function names can optionally be suffixed with a 
		type specifier. */

	if(IsTypeSpecifier(**position))
		++*position;

	QsInitStaticPtrs(t, start, *position - 1);
}

enum NumericConstantParseState {
	NUMCONSTSTART,		/* Starting. */
	FINISHEDBUTBACKTRACK,	/* Finished, went one past end, so backtrack one char to get to end of constant. */
	FINISHEDNORMAL,		/* Finished, don't backtrack. */
	ISDECIMALCONST,		/* Getting decimal constant. nnnn... */
	OCTALORHEXFOLLOWS,	/* Encountered & specifier. */
	ISOCTALCONST,		/* Getting octal constant. {& or &O}nn... */
	ISHEXCONST,		/* Getting hex constant. {Ox or &H}nn... */
	FPFRACTPART,		/* Getting fractional part of FP constant, .nnn... */
	FPEXPFOUND,		/* Encountered E[+|-] in FP constant. */
	FPPOWER,		/* Getting power in FP constant. */
	FIRSTWASZERO		/* First digit was 0. */
};

/* Numeric constants are the most variform tokens in BASIC, and therefore
the most complicated to scan.

A numeric constant can have the following parts:

	[prefix][whole-part][fractional-part][exponent][type-specifier]
	
The prefix can include a sign, and careful handling is required to
disambiguate this from the '+' and '-' operators. The '&' prefix (used
to introduce octal or hexadecimal constants) must also be distinguished
from the '&' (string concatenation) operator.
	TODO this function allows '0x', and '#', and possibly other dodgy constructs - 
change to use a different approach - not a state machine that doesn't quite work ... */
static Error GetNumericToken(const char **position, QString *t)
{
	const char *start = *position;
	Error errorFound = SUCCESS;
	enum NumericConstantParseState state = NUMCONSTSTART;

	do {
		switch(toupper(**position)) {
		case '0': 
			/* Could introduce a hex constant 0xNNN... */
			if(state == OCTALORHEXFOLLOWS)
				state = ISOCTALCONST;
				/* Assume &nnnn... to be octal const. */
			else if(state == NUMCONSTSTART)
				state = FIRSTWASZERO;
			else if(state == FPEXPFOUND)
				state = FPPOWER;
			break;
		case '1': case '2': case '3': case '4': case '5': case '6': case '7':
			/* Valid oct, hex and dec digits. */
			if(state == OCTALORHEXFOLLOWS)
				state = ISOCTALCONST;
				/* Assume &nnnn... to be octal const. */
			else if(state == NUMCONSTSTART || state == FIRSTWASZERO)
				state = ISDECIMALCONST;
				/* Assume decimal const nnnnn... */
			else if(state == FPEXPFOUND)
				state = FPPOWER;
			break;
		case '8': case '9':
			/* Valid dec, hex but not oct digits. */
			if(state == ISOCTALCONST || state == OCTALORHEXFOLLOWS)
				errorFound = BADCONSTANT;
			else if(state == NUMCONSTSTART)
				state = ISDECIMALCONST;
			else if(state == FPEXPFOUND)
				state = FPPOWER;
			break;
		case 'A': case 'B': case 'C': case 'F':
			/* Valid hex digits only. Otherwise, treat as the start of the next token. */
			if(state != ISHEXCONST)
				state = FINISHEDBUTBACKTRACK;
			break;
		case 'E': case 'D':	
			/* Hex digit, also introduce single or double floating point powers. */
			if(state == ISDECIMALCONST || state == FPFRACTPART)
				state = FPEXPFOUND;
			else if(state != ISHEXCONST)
				state = FINISHEDBUTBACKTRACK;
			break;
		case '.':
			if(state == NUMCONSTSTART || state == ISDECIMALCONST || state == FIRSTWASZERO)
				state = FPFRACTPART;
			else
				errorFound = BADCONSTANT;
			break;
		case '+': case '-':
			/* The constant might be ending - could be an operator. */
			if(state == NUMCONSTSTART)
				state = ISDECIMALCONST; /* Oct and hex constants can't have signs. */
			else if(state == FPEXPFOUND)
				state = FPPOWER;
			else
				state = FINISHEDBUTBACKTRACK;
			break;
		case 'O':
			if(state == OCTALORHEXFOLLOWS)
				/* Octal constant. */
				state = ISOCTALCONST;
			else
				state = FINISHEDBUTBACKTRACK;
			break;
		case 'H':
			if(state == OCTALORHEXFOLLOWS)
				/* Hex constant. */
				state = ISHEXCONST;
			else
				state = FINISHEDBUTBACKTRACK;
			break;
		case 'X':
			if(state == FIRSTWASZERO)
				state = ISHEXCONST;
			else
				state = FINISHEDBUTBACKTRACK;
			break;
		case '&': 
			/* Used to introduce &H, &O, &..., also longint type specifier. */
			if(state == NUMCONSTSTART)
				state = OCTALORHEXFOLLOWS;
			else if(state == ISDECIMALCONST 
			     || state == ISOCTALCONST 
			     || state == ISHEXCONST
			     || state == FIRSTWASZERO)
				state = FINISHEDNORMAL;
			else
				errorFound = BADCONSTANT;
			break;
		case '%':
			/* The integer type specifier. If collecting a F.P. constant, this is not allowed. */
			if(state == ISDECIMALCONST 
			|| state == ISOCTALCONST 
			|| state == ISHEXCONST
			|| state == FIRSTWASZERO)
				state = FINISHEDNORMAL;
			else
				errorFound = BADCONSTANT;
			break;
		case '!':
			/* The F.P. type specifier. Not allowed if collecting a hex or oct constant. */
			if(state == ISDECIMALCONST 
			|| state == FPFRACTPART
			|| state == FPPOWER
			|| state == FIRSTWASZERO)
				state = FINISHEDNORMAL;
			else
				errorFound = BADCONSTANT;
			break;
		case '#':
			/* The double precision type specifier. Not allowed if collecting a hex or oct constant.
				Also introduces a file number specifier, which is treated as an integer for
				lexical purposes. */

			if(state == NUMCONSTSTART)
				state = ISDECIMALCONST;
			else if(state == ISDECIMALCONST 
			|| state == FPFRACTPART
			|| state == FPPOWER
			|| state == FIRSTWASZERO)
				state = FINISHEDNORMAL;
			else
				errorFound = BADCONSTANT;
			break;
		default:
			/* if(state == OCTALORHEXFOLLOWS)
				errorFound = BADCONSTANT;
			else */
			/* Not a numeric constant.
				'&' as an operator when used 'unambiguously' is thus allowed. 
				If the code above is uncommented, it won't be. */
			state = FINISHEDBUTBACKTRACK;
			break;
		}
		++*position;
	}
	while(state != FINISHEDBUTBACKTRACK && state != FINISHEDNORMAL && errorFound == SUCCESS);

	if(errorFound == SUCCESS) {
		if(state == FINISHEDBUTBACKTRACK)
			--*position;
			
		QsInitStaticPtrs(t, start, *position - 1);
	}
	
	return errorFound;
}

static void GetSimpleOperatorToken(const char **position, QString *t)
{
	const char *start = *position;

	/* Multi-character operators: */
	if((start[0] == '>' && start[1] == '=')
	|| (start[0] == '<' && (start[1] == '=' || start[1] == '>')))
		++*position;
	QsInitStaticPtrs(t, start, *position);
	++*position;
}

static void GetSimpleDelimiterToken(const char **position, QString *t)
{
	QsInitStaticPtrs(t, *position, *position);
	++*position;
}

static Error GetQuotedLiteralToken(const char **position, QString *t)
{
	const char *start = *position;
	
	/* Advance to end of string. A NUL or newline in the string is illegal.
	Quotes cannot be put in strings either - there are no escape sequences in BASIC. */

	do
		++*position;
	while(!IsQuote(**position) && **position != '\n' && **position != NUL);

	if(**position != '\n' && **position != NUL) {
		QsInitStaticPtrs(t, start, *position); /* Include "" in string. */
		++*position;
		return SUCCESS;
	}
	else
		return BADCONSTANT;
}

/* Get the next token from 'position' into the string 'token'. 'leftContxt'
should be passed the previous token scanned and is used to disambiguate
certain operators (unary + and -, &) from the start of constants.
	Doesn't copy data, just sets a 'static' string to point into the buffer.
Skips white space at beginning if necessary, handles continuations, etc. Updates 
'position' so it points to the character following the token.
	Returns true if scanning should continue; false if a statement terminator 
has been scanned, or an error occurred, in which case it will be set in *error. */
static bool GetToken(const char **position, const QString *leftContext, QString *token, Error *error)
{
	bool shouldContinue = TRUE;

	*error = SUCCESS;
	
	/* Skip leading whitespace: */

	SkipWhiteSpace(position);
	
	/* Skip to continuation, if marker present: */

	if(**position == '_' && (*error = SkipContinuation(position)) != SUCCESS)
		shouldContinue = FALSE;

	/* Decide on token type and tokenise: */

	if(shouldContinue) {
		if(isalpha(**position)) {
			GetIdentifierToken(position, token);

			/* Check to see if it's REM: */
			shouldContinue = !QsEqNoCase(token, &m_Rem);
		}
		else if(AttemptToScanAsNumeric(**position, *(*position + 1), leftContext))
			/* '-', '+' and '&' have a lexical conflict between their use as operators
				and as prefixes for numeric constants. */
			*error = GetNumericToken(position, token);
		else if(IsSimpleOperator(**position))
			GetSimpleOperatorToken(position, token);
		else if(IsSimpleDelimiter(**position)) {
			/* Includes statement-terminating delimiters. */
				
			GetSimpleDelimiterToken(position, token);
			shouldContinue = !IsTerminator(token);
		}
		else if(IsQuote(**position))
			*error = GetQuotedLiteralToken(position, token);
		else /* assume a constant was mistyped */
			*error = BADCONSTANT;
	}

	return shouldContinue && *error == SUCCESS;
}

void CreateTokenSequence(struct TokenSequence *tokSeq, unsigned short initialCapacity)
{
	tokSeq->start = tokSeq->next = NULL;
	
	tokSeq->length = 0;
	tokSeq->capacity = initialCapacity;
	
	if(initialCapacity != 0) {
		unsigned short i;
		
		tokSeq->rest = New(sizeof(QString) * initialCapacity);
		for(i = 0; i != initialCapacity; i++)
			QsInitNull(&tokSeq->rest[i]);
	}
	else
		tokSeq->rest = NULL;
	
	QsInitNull(&tokSeq->lineNumber);
	QsInitNull(&tokSeq->label);
	QsInitNull(&tokSeq->statementName);
	
	tokSeq->ops = 0;
	tokSeq->command = NULL;
	tokSeq->preconverted = NULL;
}

void ClearTokenSequence(struct TokenSequence *tokSeq)
{
	assert(tokSeq != NULL);
	
	if(tokSeq->rest != NULL) {
		unsigned short i;
		for(i = 0; i < tokSeq->length; i++)
			QsDispose(&tokSeq->rest[i]);
	}
	QsDispose(&tokSeq->lineNumber);
	QsDispose(&tokSeq->label);
	QsDispose(&tokSeq->statementName);
	tokSeq->length = 0;
	
	tokSeq->ops = 0;
	tokSeq->command = NULL;
	tokSeq->preconverted = NULL;
}

void DisposeTokenSequence(struct TokenSequence *tokSeq)
{
	assert(tokSeq != NULL);
	
	if(tokSeq->preconverted != NULL) {
		unsigned short i;
		for(i = 0; i < tokSeq->length; i++)
			RemoveObject(&tokSeq->preconverted[i], FALSE);
		Dispose(tokSeq->preconverted);
	}
	ClearTokenSequence(tokSeq);
	if(tokSeq->rest != NULL) {
		Dispose(tokSeq->rest);
		tokSeq->rest = NULL;
		tokSeq->capacity = 0;
	}
	tokSeq->start = tokSeq->next = NULL;
}

static void ShallowCopy(struct TokenSequence *to, const struct TokenSequence *from)
{
	assert(to != NULL && from != NULL);
	/*assert(from->preconverted == NULL);*/
	
	to->start = from->start;
	to->next = from->next;
	to->ops = from->ops;
	to->command = from->command;

	QsCopy(&to->lineNumber, &from->lineNumber);
	QsCopy(&to->label, &from->label);
	QsCopy(&to->statementName, &from->statementName);
	
	to->length = from->length;
	to->preconverted = NULL;
	if(from->preconverted != NULL)
		to->preconverted = from->preconverted;
	else if(to->length != 0) {
		unsigned i;
		for(i = 0; i < to->length; i++)
			QsCopy(&to->rest[i], &from->rest[i]);
	}
}

struct TokenSequence *Duplicate(const struct TokenSequence *from)
{
	struct TokenSequence *copy = TolerantNew(sizeof(struct TokenSequence));

	assert(from != NULL);

	/* Don't end the program due to failing to alloc. */
	if(copy == NULL)
		return NULL;

	copy->rest = NULL;
	if(from->length != 0 && from->preconverted == NULL) {
		copy->rest = TolerantNew(sizeof(QString) * from->length);
		if(copy->rest == NULL) {
			Dispose(copy);
			return NULL;
		}
	}
	
	ShallowCopy(copy, from);
	copy->capacity = from->preconverted != NULL ? 0 : from->capacity;
	
	return copy;
}

void ExpandTokenSequence(struct TokenSequence *tokSeq, unsigned short newSize)
{
	assert(tokSeq != NULL);
	assert(newSize >= tokSeq->length);
	
	if(newSize > tokSeq->capacity) {
		struct TokenSequence expanded;
		unsigned short minimumGrowth = 3 * tokSeq->capacity / 2;

		CreateTokenSequence(&expanded, newSize > minimumGrowth ? newSize : minimumGrowth);
		
		/*fprintf(stderr, "[TokSeq] Expanded token sequence capacity %hd -> %hd\n",
			tokSeq->capacity, expanded.capacity);*/

		ShallowCopy(&expanded, tokSeq);
		DisposeTokenSequence(tokSeq);
		*tokSeq = expanded;
	}
}

void DeleteToken(struct TokenSequence *tokSeq, unsigned short index)
{
	unsigned short i;
	
	assert(tokSeq->length > 1); /* Must keep terminator at least. */
	
	for(i = index; i + 1 < tokSeq->length; i++) {
		QsDispose(&tokSeq->rest[i]);
		QsCopy(&tokSeq->rest[i], &tokSeq->rest[i + 1]);
	}
	QsDispose(&tokSeq->rest[tokSeq->length - 1]);
	--tokSeq->length;
}
		
/* Tokenisation at the statement level - from a sequence of characters produces a 
sequence of lexically-valid tokens for a simple statement.

In: *position -- the start of the statement.
	*tokSeq -- an initialised structure to store the tokens.
	skipOnError -- controls error handling behaviour. If TRUE, the
		lexer will skip to the next line if an error is encountered.
		Otherwise, *position will remain at the point the error
		occurred.

Out: *position -- updated. If no lexical error is found, *position will 
		be at the start of the next stmt, or one character beyond the buffer-
		terminating NUL if this was the last stmt of the program. If an error 
		occurred, will be at a location determined by skipOnError (see above).
	*tokSeq -- contains the scanned tokens - only those prior to where any
		error was detected will be valid. 
		
Returns: an error, or SUCCESS. */

Error Tokenise(const char **position, struct TokenSequence *tokSeq, bool skipOnError)
{
	QString dummyToken, *leftContext;
	const char *stmtStart = tokSeq->start = *position;
	Error error = SUCCESS;	
	
	/*CreateTokenSequence(tokSeq, 6);*/ /* [let] x = a <op> b \n */
	
	/* TODO should support continuations between line num, label, stmt. Not a big deal, I think. */
	SkipWhiteSpace(position);
	GetOptionalLineNumberToken(position, &tokSeq->lineNumber);
	
	SkipWhiteSpace(position);
	GetOptionalLabelToken(position, &tokSeq->label);

	SkipWhiteSpace(position);
	GetOptionalStatementToken(position, &tokSeq->statementName);

	QsInitNull(&dummyToken);
	leftContext = &dummyToken;
	while(tokSeq->length < MAX_TOKENS
	   && GetToken(position, leftContext, &tokSeq->rest[tokSeq->length++], &error)) {
		ExpandTokenSequence(tokSeq, tokSeq->length + 1);
		leftContext = &tokSeq->rest[tokSeq->length - 1];
	}
	
	if(error != SUCCESS) {
		--tokSeq->length;
		error = PositionError(error, stmtStart, *position);
	}
	else if(tokSeq->length >= MAX_TOKENS
	 && !IsTerminator(&tokSeq->rest[tokSeq->length - 1])
	 && GetToken(position, &dummyToken, &dummyToken, &error))
		error = LONGLINE;

	if((skipOnError && error != SUCCESS)
	|| (tokSeq->length <= MAX_TOKENS && tokSeq->length != 0
	  && IsCommentMarker(&tokSeq->rest[tokSeq->length - 1])))
		SkipToNextLine(position);

	if(error == SUCCESS)
		tokSeq->next = *position;

	return error;
}

#ifdef DEBUG

void PrintTokSeq(const struct TokenSequence *tokSeq)
{
	int ctr;

	if(!QsIsNull(&tokSeq->lineNumber))		/* Line number (optional) */
		QsWrite(&tokSeq->lineNumber, stderr);
	else
		fprintf(stderr, "[]");
	fputc(' ', stderr);
	if(!QsIsNull(&tokSeq->label))		/* Label (optional) */
		QsWrite(&tokSeq->label, stderr);
	else
		fprintf(stderr, "[]");
	fputc(' ', stderr);
	if(!QsIsNull(&tokSeq->statementName))	/* Statement (optional) */
		QsWrite(&tokSeq->statementName, stderr);
	else
		fprintf(stderr, "[]");

	for(ctr = 0; ctr < tokSeq->length; ctr++) {
		const QString *curTok = &tokSeq->rest[ctr];
		
		fputc(' ', stderr);
	
		/* Display the token nicely: */

		if(QsGetLength(curTok) > 0) {
			char tokFirst = QsGetFirst(curTok);

			if(isalpha(tokFirst) || IsNumConstFirst(tokFirst)
			|| IsSimpleOperator(tokFirst) || IsQuote(tokFirst))
				QsWrite(curTok, stderr);
			else if(IsSimpleDelimiter(tokFirst)) {
				if(QsGetLength(curTok) == 1) {
					if(tokFirst == ';' || tokFirst == ',' || tokFirst == '|' || tokFirst == '\'' || tokFirst == ':')
						fputc(tokFirst, stderr);
					else if(tokFirst == '\n')
						fprintf(stderr, "<newline>");
					else if(tokFirst == NUL)
						fprintf(stderr, "<nul>");
					else
						fprintf(stderr, "<!!weird delim!!>");
				}
				else
					fprintf(stderr, "<!!weird long delim!!>");
			}
			else
				fprintf(stderr, "<!!weird char %c!!>", tokFirst);
		}
		else
			fprintf(stderr, "<!!length <= 0!!>");
	}
	fprintf(stderr, ".\n");
}

#endif /* DEBUG */
