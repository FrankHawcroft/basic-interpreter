/****** label.c ******/

/*
	$VER: label.c 0.16 (7.12.14)
	
	Support for alphanumeric line labels ('Foo: bar') and traditional numeric line numbers
	('10 GOTO 20'). Line numbers are treated just like labels for most purposes - the only
	significant difference is that they are always scoped globally, whereas labels can be
	local to a subprogram.
*/

#include <ctype.h>
#include <string.h>
#include "interpreter.h"
#include "process.h"
#include "buffer.h"

#if defined(VBCC) || defined(__clang__)
extern int strnicmp(const char *, const char *, size_t);
#endif

bool AddLabel(const QString *name, const char *labelledPosition, short callNestLevel)
{
	const BObject *existing = LookUp(name, callNestLevel);

	assert(name != NULL && !QsIsNull(name));
	assert(labelledPosition != NULL);
	
	/* TODO variables, functions, subs, and labels all live in the same namespace ... 
		Generally I think labels and subs should have their own namespaces, and suspect this is the 
		case in most or all other BASICs. */
	if(existing == NULL)
		return DefineSymbol(name, (void *)(labelledPosition), LABEL, callNestLevel) == SUCCESS;
	else
		return existing->category == LABEL && labelledPosition == existing->value.labelPos;
}

const char *ScanStatementPrefix(const char *code, QString *lineNumToken, QString *labelToken, QString *stmtToken)
{
	bool candidate = FALSE, whitespaceOnlyPrefix = FALSE;
	const char *sub = QsGetData(&g_SubKeyword), *scan;
	
	QsInitNull(lineNumToken); QsInitNull(labelToken); QsInitNull(stmtToken);
	
	/* Tokenising the entire statement is slow, so try to avoid it.
		Only lines containing '[END]SUB', a label, or line number are of interest;
		lines with continuations must be scanned due to potentially tricky situations
		such as numbers at the beginning of following lines which should not be treated as
		line numbers. */
	
	for(scan = code; *scan != NUL && *scan != '\n' && !candidate; scan++) {
		whitespaceOnlyPrefix = scan == code || (whitespaceOnlyPrefix && isspace(scan[-1]));
		candidate = *scan == ':' || *scan == '_'
			|| (isdigit(*scan) && whitespaceOnlyPrefix) || strnicmp(scan, sub, 3) == 0;
	}
	
	if(candidate) {
		struct TokenSequence tokens;
		Error scanError;
		
		CreateTokenSequence(&tokens, 6);
		
		if((scanError = Tokenise(&code, &tokens, FALSE)) == SUCCESS) {
			MakeSavoury(&tokens);
			QsCopy(lineNumToken, &tokens.lineNumber);
			QsCopy(labelToken, &tokens.label);
			QsCopy(stmtToken, &tokens.statementName);
		}
		
		DisposeTokenSequence(&tokens);
		
		return scanError == SUCCESS ? code : NULL;
	}
	else
		return *scan == NUL ? scan : scan + 1;
}

/* Searches for the named label or line number, either within the body of a subprogram, or within the main program
- in which case subprograms will be skipped. */
static const char *ForwardReferencedLabel(const QString *name, bool searchingInSubprogram, const char *from, Error *error)
{
	const char *savedStmt, *found = NULL;
	bool skippingSubprogram = FALSE; /* Only important if !searchingInSubprogram */

	savedStmt = Proc()->currentStatementStart; /* Save current stmt. */

	while(found == NULL && WithinFileBuffer(Proc()->buffer, from)) {
		QString currentLineNum, currentLabel, currentStmt;
		const char *startOfNextStmt;
	
		Proc()->currentStatementStart = from; /* So error messages point to the right line. */
		
		/* Read the labels and statement names (these will be set to null strings if not present): */

		startOfNextStmt = ScanStatementPrefix(from, &currentLineNum, &currentLabel, &currentStmt);

		/* Is this the label we're looking for? */

		if(!skippingSubprogram && (QsEqNoCase(&currentLineNum, name) || QsEqNoCase(&currentLabel, name)))
			found = from;

		/* Check whether the statement is significant, i.e., is a SUB or ENDSUB statement, and adjust the state if necessary: */

		if(QsEqNoCase(&g_SubKeyword, &currentStmt)) {
			if(searchingInSubprogram || skippingSubprogram) {
				if(error != NULL)
					*error = NESTEDSUBS;
				startOfNextStmt = NULL;
			}
			skippingSubprogram = TRUE;
		}
		else if(QsEqNoCase(&g_EndSubKeyword, &currentStmt)) {
			if(!searchingInSubprogram && !skippingSubprogram) {
				if(error != NULL)
					*error = ENDSUBWITHOUTSUB;
				startOfNextStmt = NULL;
			}
			if(searchingInSubprogram) /* stop searching. */
				startOfNextStmt = NULL;
			skippingSubprogram = FALSE;
		}
	
		from = startOfNextStmt; /* Move on to next stmt. */

		QsDispose(&currentLineNum); QsDispose(&currentLabel); QsDispose(&currentStmt);
	}
	
	Proc()->currentStatementStart = savedStmt; /* Restore current stmt. */

	return found;
}

/* Deals with possibly forward-referenced labels as they appear in GOTO, GOSUB, and ON
statements. If the label can be found, its position is returned; otherwise NULL is
returned and if a pointer to an error is supplied, it will be set to an error
indication. */
const char *FindReferencedLabel(const QString *name, Error *error)
{
	const char *position;
	BObject *labelDefn = LookUp(name, Proc()->callNestLevel);

	if(error != NULL)
		*error = SUCCESS;
	
	if(labelDefn != NULL) {
		/* Already know about this label. */
		if(labelDefn->category != LABEL && error != NULL)
			*error = REDEFINE;
		position = labelDefn->category == LABEL ? labelDefn->value.labelPos : NULL;
	}
	else {
		/* Forward reference. It's more likely to be found forward of the current position,
			since not already encountered. */

		const char *sub = StartOfCurrentSubprogram();
		const char *from = sub != NULL ? sub : Proc()->currentPosition;
		
		position = ForwardReferencedLabel(name, sub != NULL, from, error);
		
		if(position == NULL && sub == NULL)
			position = ForwardReferencedLabel(name, FALSE, FileBufferBase(Proc()->buffer), error);
		
		if(position == NULL && error != NULL && *error == SUCCESS)
			*error = UNDEFINEDLABEL;
		
		if(position != NULL)
			AddLabel(name, position, Proc()->callNestLevel);
	}
	
	return position;
}

/* If a label or line number exists in the program which occurs lexically prior to the 
position passed, gets the name of the closest such label/line number to the position.

In: limit, which should point to the _start_ of a statement.
Out: if label found, labelName will contain its name. Otherwise, labelName will be
	set to the null string.

This function is not implemented in a particularly efficient way, and does not deal with subprograms
very elegantly - it will not report the correct label if a subprogram occurs between the start of
the program and the position given. */
void FindLabelPreceding(QString *labelName, const char *limit)
{
	const char *nextStmtStart = FileBufferBase(Proc()->buffer);

	QsInitNull(labelName);
	while(WithinFileBuffer(Proc()->buffer, nextStmtStart) && nextStmtStart <= limit) {
		QString lineNumToken, labelToken, statementToken;
		
		nextStmtStart = ScanStatementPrefix(nextStmtStart, &lineNumToken, &labelToken, &statementToken);

		if(!QsIsNull(&labelToken) || !QsIsNull(&lineNumToken))
			/* Prefer alphanumeric label over a line number, if ambiguity - */
			QsCopy(labelName, !QsIsNull(&labelToken) ? &labelToken : &lineNumToken);
	
		QsDispose(&lineNumToken); QsDispose(&labelToken); QsDispose(&statementToken);
	}
}
