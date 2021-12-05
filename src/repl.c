/****** repl.c ******/

/*
	$VER: repl.c 0.16A (5.9.2015)

	The read-evaluate-print loop.
*/

#include <stdio.h>
#include <limits.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "interpreter.h"
#include "builtin.h"
#include "process.h"
#include "heap.h"
#include "buffer.h"
#include "stack.h"
#include "options.h"
#include "platform.h"

/* Initial space allocated for expression evaluation. */
#define EXPR_STACK_SPACE 20 /* (1 + (MAX_TOKENS / 2)) */

#ifdef DEBUG
void PrintVerboseTracingPrefix(char pass)
{
	fprintf(stderr, "%5hu %c %c ",
		(unsigned short)(Proc()->currentStatementStart - FileBufferBase(Proc()->buffer)),
		DefaultInactive(Proc(), FALSE) ? '-' : '+',
		pass);
}
#endif

Error Prepare(struct TokenSequence *tokSeq)
{	
	assert(tokSeq != NULL);

	/* Standardise idiosyncratic syntax to make expression parsing easier. */

	MakeSavoury(tokSeq);

#ifdef DEBUG
	if(Opts()->verbose) {
		PrintVerboseTracingPrefix('2');
		PrintTokSeq(tokSeq);
	}
#endif

	/* At this stage (cf the syntax checking pass), a resolved statement is required.
		MakeSavoury is assumed to have looked it up. */

	if(tokSeq->command == NULL)
		return GetStatement(&tokSeq->statementName, &tokSeq->command); /* i.e. a positioned UNDEFINED error */
	
	if(RequiresSyntaxCheck(tokSeq) && CheckStatementSyntax(tokSeq) != SUCCESS)
		return CheckStatementSyntax(tokSeq);

	/* Convert parameter expressions from infix to prefix form. */

	if(!IsMacro(tokSeq->command) && tokSeq->length > 2) {
		unsigned prefixFormLength = 0;
		QString *prefixFormSpace = InfixToPrefix(&tokSeq->rest[0], tokSeq->length - 1, &prefixFormLength);

		if(prefixFormSpace == NULL)
			return BADSYNTAX;
		
		ReplaceTokens(tokSeq, prefixFormSpace, (unsigned short)prefixFormLength);
		
#ifdef DEBUG
		if(Opts()->verbose) {
			PrintVerboseTracingPrefix('3');
			PrintTokSeq(tokSeq);
		}
#endif
	}
	
	return SUCCESS;
}

/* Apply a label to the current statement, if it's present. */
static Error ApplyLabel(const QString *name, short callNestLevel)
{
	return QsIsNull(name) || AddLabel(name, Proc()->currentStatementStart, callNestLevel) ? SUCCESS : REDEFINE;
}

static Error Compile(const char **position, struct TokenSequence *tokSeq)
{
	Error error = SUCCESS;

	assert(position != NULL && *position != NULL);
	assert(tokSeq != NULL);

	/*fprintf(stderr, "Entered Compile, stmt = %.20s", *position);*/

	/* Tokenise the statement text. Post this call, *position points to the next statement. */

	error = Tokenise(position, tokSeq, FALSE);

#ifdef DEBUG
	if(error == SUCCESS && Opts()->verbose) {
		PrintVerboseTracingPrefix('1');
		PrintTokSeq(tokSeq);
	}
#endif

	/* Fetch command; check syntax and convert arg exprs to prefix (Lisp-like) form. */
	
	if(error == SUCCESS)
		error = Prepare(tokSeq);

	/* Add or check the statement's labels, if present. */

	if(error == SUCCESS)
		error = ApplyLabel(&tokSeq->lineNumber, SCOPE_GLOBAL); 
	if(error == SUCCESS)
		error = ApplyLabel(&tokSeq->label, Proc()->callNestLevel); /* Hmmm, STATIC if in a sub? Pros and cons. */

	return error;
}

enum Ops {
	OP_INACTIVE = 0x1,
	OP_TRACE = 0x2,
	OP_VERBOSE = 0x4,
	OP_PROFILE = 0x8,
	OP_EVAL = 0x10,
	OP_EVALQ = 0x20,
	OP_CONFORM = 0x40,
	OP_CONFORMQ = 0x80,
	OP_MACRO = 0x100,
	OP_SUB = 0x200,
	OP_EXEC = 0x400,
	OP_CACHE = 0x800,
	OP_CLEAR = 0x1000,
	OP_CLEARQ = 0x2000,
	OP_POLL = 0x4000,
	OP_OPTIMISE = 0x8000
};

static unsigned GetOps(const struct TokenSequence *ts, short callNestLevel)
{
	unsigned ops = OP_INACTIVE | OP_POLL;

	ops |= (EligibleForCaching(ts, callNestLevel) ? OP_CACHE : 0);
	ops |= (Opts()->optimise && !IsMacro(ts->command) && (ops & OP_CACHE) ? OP_OPTIMISE : 0);
	ops |= (Proc()->trace ? OP_TRACE : 0);
	ops |= (Opts()->profileDest != NULL ? OP_PROFILE : 0);
#ifdef DEBUG
	ops |= (Opts()->verbose ? OP_VERBOSE : 0);
#endif

	if(IsMacro(ts->command)) ops |= OP_MACRO;
	else if(IsSubprogram(ts->command)) ops |= (OP_EVAL | OP_CONFORM | OP_SUB | OP_CLEAR);
	else ops |= (OP_EVAL | OP_CONFORM | OP_EXEC | OP_CLEAR);
	
	return ops;
}

static void SetOptimisedOps(struct TokenSequence *ts, short callNestLevel)
{
	ts->ops = GetOps(ts, callNestLevel) & ~(OP_CACHE | OP_OPTIMISE);
	
	/*if(Opts()->optimise && !StatementIsEmpty(ts->command) && GuaranteedLive(Proc()))
		ts->ops &= ~OP_INACTIVE;*/
	
	if(Opts()->optimise) {
		if(SemanticallyPredictable(ts))
			ts->ops &= ~OP_CONFORM;
		if(NoDynamicallyAllocatedMemory(ts))
			ts->ops &= ~OP_CLEAR, ts->ops |= OP_CLEARQ;
	}
	
	if(StatementIsEmpty(ts->command))
		ts->ops &= ~OP_POLL; /* TODO option to only poll every n stmts? */
	
	if(ts->length <= 1 && (ts->command->formalCount == 0 || StatementIsEmpty(ts->command)))
		ts->ops &= ~(OP_EVAL | OP_EVALQ | OP_CONFORM | OP_CLEAR | OP_CLEARQ);
		
	if(ts->preconverted != NULL)
		ts->ops &= ~OP_EVAL, ts->ops |= OP_EVALQ;
		
	if((ts->ops & OP_CONFORM) && IsAssignmentStatement(ts->command)) /* TODO can be a bit broader */
		ts->ops &= ~OP_CONFORM, ts->ops |= OP_CONFORMQ;
		
	/*if(ts->preconverted != NULL || !ShouldCachePreconvertedObjects(ts, callNestLevel))
		ts->ops &= ~(OP_CACHE | OP_OPTIMISE);*/
}

/* Prints the statement preceded by its line number. */
static void ShowTraceInfo(const char *statement)
{
	const char *file, *p;
	int line;

	GetLocationInfo(Proc()->buffer, statement, &line, &file);
	if(file != NULL && *file != NUL && file != Proc()->currentFileName)
		printf("file> %s\n", Proc()->currentFileName = file);
	printf("%4d> ", line);
	for(p = statement; !IsSimpleTerminator(*p) || *p == '\''; p++) /* TODO not right for labels */
		putchar(*p);
	putchar('\n');
}

static short EffectiveCallNestLevel(const struct Process *proc) { return InStaticContext(proc) ? SCOPE_STATIC : proc->callNestLevel; }

void Do(struct Process *proc, struct TokenSequence *ts, struct Stack *exprStack)
{
	unsigned ops;
	const char *endOfStmt = NULL;
	const BObject *vdef = NULL;
	PfHighResolutionTimeStamp startTime;
	short initialCallNestLevel = SCOPE_NONEXISTENT;
	
	assert(ts != NULL);
	assert(exprStack != NULL);
	
	ops = ts->ops;
	if(ops == 0) {
		initialCallNestLevel = EffectiveCallNestLevel(proc);
		ops = GetOps(ts, initialCallNestLevel);
	}

	if(ops & OP_PROFILE) {
		PfRecordTime(&startTime);
		endOfStmt = proc->currentPosition - 1;
	}
	
	if(ops & OP_OPTIMISE) {
		Improve(ts);

#ifdef DEBUG
		if(ops & OP_VERBOSE) {
			PrintVerboseTracingPrefix('o');
			PrintTokSeq(ts);
		}
#endif
	}
	
	/* Determine if the statement should actually be executed. */

	if((ops & OP_INACTIVE) && (*ts->command->inactive)(proc, FALSE))
		ops = OP_POLL | (ops & OP_CACHE);

	if(ops & OP_CACHE)
		StorePreconvertedObjects(ts, initialCallNestLevel);

	if(ops & (OP_EVAL | OP_EVALQ | OP_CONFORM | OP_CONFORMQ | OP_SUB | OP_EXEC)) {
		Error error = SUCCESS;

		assert(!(ops & OP_MACRO));
		assert(!IsMacro(ts->command));
		
		/* Evaluate the parameters. */

		if(ops & OP_EVAL)
			Eval(ts->rest, ts->command->convert, 0, exprStack);
		else if(ops & OP_EVALQ)
			EvalPreconverted(ts->preconverted, exprStack);

#ifdef DEBUG
		if(ops & OP_VERBOSE) {
			PrintVerboseTracingPrefix('4');
			DumpExprStk(exprStack);
		}
#endif

		/* Check types of parameters, and attempt to convert them if value parameters - */

		if(ops & OP_CONFORM)
			error = Conform(ts->command->formal, ts->command->formalCount,
				StkHeight(exprStack) > 0 ? (BObject *)exprStack->base : NULL,
				(unsigned)StkHeight(exprStack));
		else if(ops & OP_CONFORMQ)
			error = ConformQuickly(ts->command->formal, (BObject *)exprStack->base, ts->command->formalCount);

#ifdef DEBUG
		if((ops & OP_VERBOSE) && (ops & (OP_CONFORM | OP_CONFORMQ)) && error == SUCCESS) {
			PrintVerboseTracingPrefix('5');
			DumpExprStk(exprStack);
		}
#endif

		if(error == SUCCESS) {
			/* If tracing, print trace output. */

			if(ops & OP_TRACE)
				ShowTraceInfo(proc->currentStatementStart);

			/* Somewhat messy, but in order to look up symbols in the correct context, objects must be preconverted
				before the statement is executed. Just saving the call nest level and using it later doesn't work
				due to the SCOPE_STATIC convention - it isn't possible to 'see' the calling subprogram's scope, once
				in a called sub. */
				
			if(ops & OP_CACHE) {
				StorePreconvertedObjects(ts, initialCallNestLevel);
				if(ts->preconverted == NULL)
					/* May be an assignment (LET) statement, for which variable lookup can be made quicker. */
					vdef = AssignmentTarget(ts, initialCallNestLevel);
			}

			/* Call the statement or subprogram. */

			if(ops & OP_EXEC) {
				assert(!IsSubprogram(ts->command));
				(*ts->command->method.builtIn)((BObject *)exprStack->base, StkHeight(exprStack));				
			}
			else {
				assert((ops & OP_SUB) && IsSubprogram(ts->command));
				CallSubprogram(ts->command, (BObject *)exprStack->base, StkHeight(exprStack), ts->ops == 0);
			}
		}
		else
			CauseError(error);

		/* Delete parameters. */

		if(ops & OP_CLEAR)
			ClearExprStk(exprStack);
		else if(ops & OP_CLEARQ)
			StkClearQuick(exprStack);
	}
	
	if(ops & OP_MACRO) {
		assert(IsMacro(ts->command));
		
		/* Macro - i.e. takes unconverted, unevaluated tokens as parameters. */
		
		if(ops & OP_TRACE)
			ShowTraceInfo(proc->currentStatementStart);

		(*ts->command->method.macro)(ts->rest, ts->length);
	}
	
	/* There's potential for a use-after-free bug if ts is accessed beyond this point -
	statements like CLEAR, TRON or TROFF, or the FRE function,  statement will clear the statement cache, invalidating ts if it was retrieved from the cache. */
	
	if(ops & OP_CACHE) {
		ImproveIfAssignmentStatement(ts, vdef, initialCallNestLevel);
		/* Recalculate ops so as not to capture particular state we don't want (i.e. skipping). 
			Also, ensure recaching doesn't happen - as noted above, this is important for more than just
			performance. */
		SetOptimisedOps(ts, initialCallNestLevel);
		AttemptToCache(ts);
	}
	
	/* Check for events regardless of whether statement was executed or not. */

	if(ops & OP_POLL)
		CheckForEvents(proc);

	/* If profiling, update execution count for this statement. */

	if(ops & OP_PROFILE)
		IncrExecutionCount(&proc->stats, proc->buffer, proc->currentStatementStart, endOfStmt,
			PfGetElapsedTimeSince(&startTime));
		/* proc->currentPosition - 1 is the end of this statement's text */

	/* Move to the next statement. */

	proc->currentStatementStart = proc->currentPosition;
}

static void Immediate(void)
{
	int stdinFailureCount = 0;
	
	while(Proc()->mode == MODE_INTERACTIVE) {
		const char *stmtStart, *position;
		Error error = SUCCESS;
		char buf[MAX_TOKENS + 1];

		Proc()->trace = FALSE; /* If left on, causes problems because immediate mode doesn't update the
									current position. */

		printf("Ok> ");
		fflush(stdout);
		
		if(fgets(buf, MAX_TOKENS + 1, stdin) == NULL) {
			fprintf(stderr, "Unable to read from stdin.\nInterpreter will exit on second failure.\n");
			if(++stdinFailureCount == 2)
				Proc()->mode = MODE_HALTED_FOR_EXIT;
			continue;
		}
		else
			stdinFailureCount = 0;
		
		{
			const char *ending = EndOfUsedRegion(Proc()->buffer);
			position = ending + (ending > FileBufferBase(Proc()->buffer) && ending[-1] != '\n');
			stmtStart = position;
		}

		if((error = (AppendToBuffer(Proc()->buffer, buf) ? SUCCESS : PROGRAMBUFFERFULL)) == SUCCESS) {
			/* This inner loop is needed because of multi-statement lines. */
			
			struct TokenSequence tokSeq;
		
			CreateTokenSequence(&tokSeq, 10);
			
			while(error == SUCCESS
			   && (error = Tokenise(&position, &tokSeq, FALSE)) == SUCCESS
			   && (error = Prepare(&tokSeq)) == SUCCESS) {
				struct Stack localExprStack;
				
				Proc()->pendingError = SUCCESS;
				
				CreateExprStk(&localExprStack, EXPR_STACK_SPACE);
				
				Proc()->currentStatementStart = stmtStart;
				Proc()->currentPosition = position;
				
				Do(Proc(), &tokSeq, &localExprStack);
				
				stmtStart = position = Proc()->currentPosition;
				
				error = Proc()->pendingError;
				
				DisposeExprStk(&localExprStack);
				ClearTokenSequence(&tokSeq);
			}
			
			DisposeTokenSequence(&tokSeq);
		}
		
		if(error != SUCCESS)
			ReportError(error, "<immediate>", 1, stmtStart, Proc()->additionalErrorInfo);
	}
}

extern bool ProgramSyntaxCheckPassed(void);
extern void PrintStackTrace(int maxDepth);

int Loop(void)
{
	struct Process *proc = Proc();
	proc->initialTextExtent = EndOfUsedRegion(proc->buffer);
	
	if(!ProgramSyntaxCheckPassed()) {
		proc->mode = MODE_HALTED_FOR_EXIT;
		proc->returnCode = EXIT_FAILURE;
	}
	else {
		proc->mode = MODE_RUNNING;
		proc->currentStatementStart = proc->currentPosition = FileBufferBase(proc->buffer);
	}
	
	while(proc->mode == MODE_RUNNING) {
		struct TokenSequence tokSeq;
		struct Stack exprStack;
		
		assert(proc->currentStatementStart != NULL);
		
		CreateExprStk(&exprStack, EXPR_STACK_SPACE);
		CreateTokenSequence(&tokSeq, 40);
		
		proc->currentFileName = PrimaryBufferFileName(proc->buffer);
		
		while(proc->mode == MODE_RUNNING && WithinFileBuffer(proc->buffer, proc->currentStatementStart)) {
			struct TokenSequence *cached = GetFromCache(proc, proc->currentStatementStart);

			assert(proc->currentPosition == proc->currentStatementStart);

			if(cached != NULL) {
				proc->currentPosition = cached->next;
				Do(proc, cached, &exprStack);
			}
			else {
				const char *scanPosition = proc->currentStatementStart;
				Error compilationResult = Compile(&scanPosition, &tokSeq);
	
				if(compilationResult == SUCCESS) {
					proc->currentPosition = scanPosition;
					Do(proc, &tokSeq, &exprStack);
				}
				else {
					proc->pendingError = compilationResult;
					proc->mode = MODE_INTERACTIVE;
				}
				
				ClearTokenSequence(&tokSeq);
			}
		}
		
		/*if(!WithinFileBuffer(proc->buffer, proc->currentStatementStart))
			fprintf(stderr, "Exiting due to outside buf: %p\n", proc->currentStatementStart);*/
		
		DisposeTokenSequence(&tokSeq);
		DisposeExprStk(&exprStack);
		
		/* Handle any error which stopped execution: */
		
		if(proc->pendingError != SUCCESS) {
			const char *file = NULL, *stmt = NULL;
			int line = 0;
	
			UnwindForErrorReport(&file, &line, &stmt);
			ReportError(proc->pendingError, file, line, stmt, proc->additionalErrorInfo);
			PrintStackTrace(5);
			
			proc->pendingError = SUCCESS;
			proc->returnCode = Opts()->immediate || proc->mode != MODE_HALTED_FOR_EXIT
				? EXIT_SUCCESS : EXIT_FAILURE;
		}
		
		/* Determine whether to go into immediate (a.k.a. interactive) mode: */
		
		if(proc->mode != MODE_HALTED_FOR_EXIT && Opts()->immediate)
			proc->mode = MODE_INTERACTIVE;
		else
			proc->mode = MODE_HALTED_FOR_EXIT;
		
		assert(proc->callNestLevel >= SCOPE_MAIN);
		
		/* Check for incomplete block control structures if at end of program text: */
		
		{
			Error error;
			
			if(!WithinFileBuffer(proc->buffer, proc->currentStatementStart)
			&& (error = CheckForUnbalancedBlocks(FALSE)) != SUCCESS) {
				const char *file;
				int line;
				
				GetLocationInfo(proc->buffer, proc->currentStatementStart, &line, &file);
				ReportError(error, file, line, proc->currentStatementStart, proc->additionalErrorInfo);
				
				proc->returnCode = Opts()->immediate || proc->mode != MODE_HALTED_FOR_EXIT ? EXIT_SUCCESS : EXIT_FAILURE;
			}
		}
	
		Immediate();
	}
	
	return proc->returnCode;
}
