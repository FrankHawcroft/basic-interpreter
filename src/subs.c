/****** subs.c ******/

/*
	$VER: subs.c 0.16 (11.21.2012)

	Statements supporting subprograms.
*/

#include <string.h>
#include <ctype.h>
#include "interpreter.h"
#include "process.h"
#include "builtin.h"
#include "heap.h"
#include "buffer.h"
#include "options.h"
#include "hashtable.h"

void CallSubprogram(const struct Statement *statement, const BObject *actual, unsigned actualCount, bool firstTime)
{
	struct Process *proc = Proc();
	Error error = SUCCESS;
	short an;
	
	assert(IsSubprogram(statement));
	assert(statement->formalCount == (short)actualCount); /* Optional parameters are not supported for subs. */

	++proc->callNestLevel;
	proc->staticSubCallNesting += statement->staticSub;
	PushActivationRecord(statement);

	/* Create local variables and copy values or set references from actuals. */

	if(statement->staticSub)
		for(an = 0; an < statement->formalCount && error == SUCCESS; an++, actual++) {
			error = AssignToStaticParameter(&statement->formal[an], statement->predefinedParameter[an], actual);
			if(statement->predefinedParameter[an] == NULL)
				statement->predefinedParameter[an] = VarPtr(LookUp(&statement->formal[an].name, SCOPE_STATIC));
		}
	else
		for(an = 0; an < statement->formalCount && error == SUCCESS; an++, actual++)
			error = !firstTime || CanDefineVariable(&statement->formal[an].name, proc->callNestLevel)
				? CreateArgumentVariable(&statement->formal[an], actual) : REDEFINE;
	
	if(error == SUCCESS)
		proc->currentPosition = statement->method.sub;
	else {
		 /* Bail out. */
		--proc->callNestLevel;
		proc->staticSubCallNesting -= statement->staticSub;
		DiscardCurrentControlFlow();
		CauseError(error);
	}
}

/* Useful for error reporting. */
static const char *StartOfPreviousLine(const char *position)
{
	const char *scan, *base = FileBufferBase(Proc()->buffer);
	for(scan = position - 1; scan > base && scan[-1] != '\n'; scan--)
		;
	return scan;
}

static bool BufferInUse(void)
{
	return FileBufferBase(Proc()->buffer) != NULL && Proc()->currentStatementStart != NULL;
}

/* All three parameters are 'out'. If a program isn't running yet, they will be set to NULL/-1. */
void UnwindForErrorReport(const char **file, int *line, const char **stmt)
{
	assert(file != NULL && line != NULL && stmt != NULL);

	*file = *stmt = NULL;
	*line = -1;

	if(BufferInUse()) {
		GetLocationInfo(Proc()->buffer, Proc()->currentStatementStart, line, file);

		/* Hide errors in the prelude! */

		if(!Opts()->preludeDebugging)
			while(*line == -1 && StartOfCurrentSubprogram() != NULL) {
				/* This is dangerous as may end up in a recursive
				error-propagating situation, particularly if there are
				active events and handlers. */
				ExitSub_(NULL, 0);

				Proc()->currentStatementStart = StartOfPreviousLine(Proc()->currentPosition);
				GetLocationInfo(Proc()->buffer, Proc()->currentStatementStart, line, file);
			}

		*stmt = Proc()->currentStatementStart;
	}
}

static bool FalsePositive(const char *base, const char *found)
{
	if(isalnum(found[3]))
		return TRUE;
	for( ; found >= base && *found != '\n' && *found != '|' && *found != ':'; found--)
		if(IsQuote(*found) || *found == '\'' || strnicmp(found, " rem ", 5) == 0)
			return TRUE;
	return FALSE;
}

static bool Match(const char *base, const char *found)
{
	return (found >= base + 3 && strnicmp(found - 3, KW_END, 3) == 0)
		|| (found >= base + 4 && strnicmp(found - 4, "end ", 4) == 0);
}

static const char *FindEndSub(const char *startOfSub)
{
	const char *pos = startOfSub, *found = NULL;
	Error error = SUCCESS;

	while(found == NULL && WithinFileBuffer(Proc()->buffer, pos) && error == SUCCESS) {
		QString search;
		QsInternalLength idx;
		
		QsInitStatic(&search, pos, EndOfUsedRegion(Proc()->buffer) - pos);
		idx = QsSearchNoCase(&search, &g_SubKeyword, 0);

		if(idx < 0)
			pos = NULL;
		else if(!Match(startOfSub, pos + idx) || FalsePositive(startOfSub, pos + idx)) {
			pos += idx + 4;
			found = NULL;
		}
		else
			found = pos + idx + 4;
	}

	if(error != SUCCESS)
		CauseError(error);
	else if(found == NULL)
		CauseError(SUBWITHOUTENDSUB);

	return found == NULL ? Proc()->currentPosition : found;
}

static void DisposeDefinition(void *object)
{
	RemoveObject(object, TRUE);
}

void Sub_(const QString *toks, unsigned nToks)
{
	const QString *subprogramName = &toks[0];
	const char *startOfSubprogram = Proc()->currentPosition;
	const BObject *existingDefn;

	if(nToks < 2 || !IsName(subprogramName)) {
		CauseError(BADSYNTAX);
		return;
	}
	
	/* Ensure that subprograms aren't nested ... */

	if(Proc()->callNestLevel != SCOPE_MAIN) {
		CauseError(NESTEDSUBS);
		return;
	}

	/* ... and that the name isn't already in use for something else. */
	
	existingDefn = LookUp(subprogramName, SCOPE_GLOBAL);
	if(existingDefn != NULL
	  && (existingDefn->category != STATEMENT 
	    || !IsSubprogram(existingDefn->value.statement)
	    || existingDefn->value.statement->method.sub != startOfSubprogram)) {
		CauseError(REDEFINE);
		return;
	}
	
	if(existingDefn == NULL) {
		struct Parameter *params = NULL;
		struct Statement *stmt;
		short numParams = 0;
		Error error = SUCCESS;
		bool isStatic = QsEqNoCase(toks + nToks - 2, &g_StaticKeyword);
		
		/* Parse the formal parameters: */
		
		if(QsGetFirst(&toks[1]) == '('
		&& nToks > 4u - isStatic
		&& ((error = CheckNameList(toks + 2, nToks - 4 - isStatic, TRUE)) != SUCCESS
		  || (params = ParseNameList(toks + 2, nToks - 4 - isStatic, &numParams, SCALAR_VAR)) == NULL)) {
			CauseError(error);
			return;
		}

		/* Create the statement: */

		if((stmt = TolerantNew(sizeof(*stmt))) == NULL) {
			Dispose(params);
			CauseError(NOMEMORY);
			return;
		}
		
		stmt->method.sub = startOfSubprogram;
		stmt->convert = DefaultConvert;
		stmt->inactive = DefaultInactive;
		stmt->formal = params;
		stmt->formalCount = numParams;
		stmt->userDefined = TRUE;
		stmt->staticSub = isStatic;
		stmt->localStatics = NULL;
		stmt->predefinedParameter = NULL;

		/* Add it to the symbol table: */

		if((error = DefineSymbol(subprogramName, stmt, STATEMENT, SCOPE_GLOBAL)) != SUCCESS) {
			DisposeStatement(stmt);
			CauseError(error);
			return;
		}
		
		/* Pre-create local environment if static. */
		
		if(isStatic) {	
			stmt->localStatics = HtCreate(5 + numParams / 2 + numParams % 2, DisposeDefinition, NULL);
			if(numParams > 0) {
				stmt->predefinedParameter = New(sizeof(struct Variable *) * numParams);
				memset(stmt->predefinedParameter, 0, sizeof(struct Variable *) * numParams);
			}
		}
	} /* no existing definition of this SUB */

	/* Skip to the statement after the ENDSUB. A lexical scan ahead is
	used rather than pushing an inactive record on the control flow stack,
	partly to avoid side effects which could cause problems when not
	actually executing the subprogram, partly for performance. */

	Proc()->currentPosition = FindEndSub(startOfSubprogram);
}

void Shared_(const QString *toks, unsigned nToks)
{
	short numParams = 0, pi;
	struct Parameter *params = NULL;
	Error error = SUCCESS;
	struct Process *proc = Proc();
	bool previouslyExecuted = GetFromCache(proc, proc->currentStatementStart) != NULL;
	
	if(!previouslyExecuted) {
		if(proc->callNestLevel <= SCOPE_MAIN) {
			CauseError(SHAREDOUTSIDESUB);
			return;
		}
		
		if(nToks > 1 && (error = CheckNameList(toks, nToks - 1, FALSE)) != SUCCESS) {
			CauseError(error);
			return;
		}
	}
	else if(InStaticContext(proc)) /* locals already created, so do nothing */
		return;
	
	if(nToks > 1 && (params = ParseNameList(toks, nToks - 1, &numParams, SHARED_VAR)) == NULL) {
		CauseError(NOMEMORY);
		return;
	}
	
	for(pi = 0; pi < numParams && error == SUCCESS; pi++) {	
		BObject *globalVarDefn = LookUpIgnoringType(&params[pi].name, SCOPE_MAIN),
			*localVarDefn = LookUpIgnoringType(&params[pi].name, proc->callNestLevel);
		const char *errorMsg = NULL;
		
		if(globalVarDefn == NULL || !IsVariable(globalVarDefn)) {
			error = UNDEFINEDVARORFUNC;
			errorMsg = "Not found: %.*s";
		}
		else if(localVarDefn != NULL && (!IsVariable(localVarDefn) || VarData(localVarDefn) != VarData(globalVarDefn))) {
			/* Strange-looking second part of this check is needed because CONSTs are globally visible,
			  but want to allow them to be shared too. */
			error = REDEFINE;
			errorMsg = "Already exists: %.*s";
		}
		else if(params[pi].explicitlyTyped
		  && NonPointer(VarData(globalVarDefn)->type) != TypeUsuallyProducedBy(params[pi].type)) {
			error = BADARGTYPE;
			errorMsg = "Type doesn't match for: %.*s";
		}
		else if(IsArray(globalVarDefn) != ((params[pi].kind & VARIABLE_IS_ARRAY) != 0)) {
			error = (params[pi].kind & VARIABLE_IS_ARRAY) ? ARRAYEXPECTED : SCALAREXPECTED;
			errorMsg = "Kind doesn't match for: %.*s";
		}
		else if(localVarDefn == NULL)
			error = ShareVariable(&params[pi].name, globalVarDefn);
			
		if(error != SUCCESS && errorMsg != NULL)
			SetAdditionalErrorMessage(errorMsg, QsGetData(&params[pi].name), QsGetLength(&params[pi].name));
	}

	if(params != NULL)
		Dispose(params);
	
	if(error != SUCCESS)
		CauseError(error);
}

void ExitSub_(BObject *arg, unsigned count)	 
{
	struct Process *proc = Proc();
	if(proc->callNestLevel > SCOPE_MAIN) {
		bool staticSub = InStaticContext(proc);
		
		DiscardCurrentControlFlow();
		if(!staticSub)
			ClearOutOfContextItems(proc->callNestLevel);
		ReenableEventTraps(proc, --proc->callNestLevel);
		proc->staticSubCallNesting -= staticSub;
		ReturnFromSubprogram();
	}
	else
		CauseError(ENDSUBWITHOUTSUB);
}

void EndSub_(BObject *arg, unsigned count)	 
{
	Error error = CheckForUnbalancedBlocks(TRUE);
	if(error == SUCCESS)
		ExitSub_(arg, count);
	else
		CauseError(error);
}
