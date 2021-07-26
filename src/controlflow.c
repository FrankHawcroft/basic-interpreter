/****** controlflow.c ******/

/*
	$VER: controlflow.c 0.16A (10.26.2015)

	The control flow stack, and both block and traditional (unstructured) control flow statements.
*/

#include <limits.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include "interpreter.h"
#include "builtin.h"
#include "process.h"
#include "heap.h"
#include "stack.h"
#include "buffer.h"
#include "cache.h"
#ifdef DEBUG
#include "hashtable.h"
#endif

#define STMT_GOSUB 1
#define STMT_FOR 2
#define STMT_IF 4
#define STMT_ELSE 8
#define STMT_SELECT 16
#define STMT_CASE 32 
#define STMT_DEFAULT 64
#define STMT_REPEAT 128
#define STMT_WHILE 256
#define STMT_CALL 512

#define STMT_MASK 1023

#define TAKEN_BRANCH 1024
#define UNTAKEN_BRANCH 2048
#define BLOCK_PLACEHOLDER 4096

enum ControlFlow {
	NO_CONTROL = 0,
	GOSUB_POSITION = STMT_GOSUB | TAKEN_BRANCH,
	FOR_INT = STMT_FOR | TAKEN_BRANCH | 8192,
	FOR_LONG = STMT_FOR | TAKEN_BRANCH | 16384,
	FOR_SINGLE = STMT_FOR | TAKEN_BRANCH | 32768,
	FOR_DOUBLE = STMT_FOR | TAKEN_BRANCH | 65536,
	FOR_UNTAKEN = STMT_FOR | UNTAKEN_BRANCH,
	IF_BLOCK = STMT_IF | TAKEN_BRANCH, /* or executing an ELSEIF clause */
	ELSE_BLOCK = STMT_IF | STMT_ELSE | TAKEN_BRANCH,			
	IF_UNTAKEN = STMT_IF | UNTAKEN_BRANCH, /* or an ELSEIF */
	IF_TAKEN_SKIP_ELSE = STMT_IF | STMT_ELSE | UNTAKEN_BRANCH,
	SELECT_ENTERED = STMT_SELECT,
	CASE_BLOCK = STMT_SELECT | STMT_CASE | TAKEN_BRANCH,			
	CASE_TAKEN_SKIP_REST = STMT_SELECT | UNTAKEN_BRANCH,
	DEFAULT_BLOCK = STMT_SELECT | STMT_DEFAULT | TAKEN_BRANCH,			
	REPEAT_BLOCK = STMT_REPEAT | TAKEN_BRANCH,
	WHILE_BLOCK = STMT_WHILE | TAKEN_BRANCH,
	WHILE_UNTAKEN = STMT_WHILE | UNTAKEN_BRANCH,
	CALL_POSITION = STMT_CALL | TAKEN_BRANCH,
	NESTED_UNTAKEN_FOR = STMT_FOR | UNTAKEN_BRANCH | BLOCK_PLACEHOLDER,
	NESTED_UNTAKEN_IF = STMT_IF | UNTAKEN_BRANCH | BLOCK_PLACEHOLDER,			
	NESTED_UNTAKEN_WHILE = STMT_WHILE | UNTAKEN_BRANCH | BLOCK_PLACEHOLDER,
	NESTED_UNTAKEN_REPEAT = STMT_REPEAT | UNTAKEN_BRANCH | BLOCK_PLACEHOLDER,
	NESTED_UNTAKEN_SELECT = STMT_SELECT | UNTAKEN_BRANCH | BLOCK_PLACEHOLDER
};

/* Counter, end value and step size always have the same type. */
struct ForLoopControl {
	union Pointer counter;
	union NumericalValue endValue;
	union NumericalValue stepSize;
};

/* Control flow stack record. */
struct StackNode {
	enum ControlFlow kind;
	const char *retAddr; /* Used in loops, GOSUB, CALL, and for untaken branch caching. */
	union {
		struct ForLoopControl forLoop;
		const struct Statement *subprogram; /* Used only if CALL_POSITION. */
		Scalar selectValue; /* Used only if SELECT_ENTERED. */
	} extension;
};

#define DEFAULT_STACK_SIZE (50 + 1) /* 50 per AmigaBASIC; + 1 for sentinel */

void CreateControlFlowStack(unsigned short height)
{
	assert(Proc()->controlFlowStack == NULL);
	
	Proc()->controlFlowStack = New(sizeof(struct Stack));
	StkInit(Proc()->controlFlowStack);
	StkCreate(Proc()->controlFlowStack, sizeof(struct StackNode), height == 0 ? DEFAULT_STACK_SIZE : height);
	
	{
		struct StackNode sentinel;
		sentinel.kind = NO_CONTROL;
		sentinel.retAddr = NULL;
		StkPush(Proc()->controlFlowStack, &sentinel);
		Proc()->currentContext = sentinel.kind;
	}
}

static void DisposeNode(void *item)
{
	struct StackNode *node = item;
	if(node->kind == SELECT_ENTERED)
		DisposeScalar(&node->extension.selectValue);
}

void DisposeControlFlowStack(void)
{
	if(Proc()->controlFlowStack != NULL)
	{
		StkClear(Proc()->controlFlowStack, DisposeNode);
		StkDispose(Proc()->controlFlowStack);
		Dispose(Proc()->controlFlowStack);
		Proc()->controlFlowStack = NULL;
		Proc()->currentContext = NO_CONTROL;
	}
}

static int ControlFlowStackHeight(void)
{
	return StkHeight(Proc()->controlFlowStack);
}

#define PeekAt(proc, offset) ((struct StackNode *)StkPeek(proc->controlFlowStack, offset))

static struct StackNode *Peek(int offset)
{
	return PeekAt(Proc(), offset);
}

static struct StackNode *PeekTop(void)
{
	return Peek(0);
}

static enum ControlFlow CurrentContext(void)
{
	return (enum ControlFlow)Proc()->currentContext; /* ControlFlowStackHeight() > 0 ? PeekTop()->kind : NO_CONTROL*/;
}

static struct ForLoopControl *PeekForLoopControl(void)
{
	return ControlFlowStackHeight() > 1 ? &(PeekTop()->extension.forLoop) : NULL;
}

static const Scalar *PeekSelectValue(void)
{
	assert(ControlFlowStackHeight() <= 1 || (CurrentContext() & STMT_SELECT));

	return ControlFlowStackHeight() > 1 ? &(PeekTop()->extension.selectValue) : NULL;
}

static void RemoveTop(void)
{
	struct Process *proc = Proc();
	StkDiscard(proc->controlFlowStack, 1, DisposeNode);
	proc->currentContext = PeekTop()->kind;
}

static bool InSelectionStatement(void)
{
	enum ControlFlow nodeKind = CurrentContext();
	return (nodeKind & STMT_MASK) == STMT_IF || (nodeKind & STMT_MASK) == STMT_SELECT;
}

static void PushContext(const struct StackNode *node)
{
	struct Process *proc = Proc();
	if(StkFull(proc->controlFlowStack))
		CauseError(ER_STACK_OVERFLOW);
	else {
		proc->currentContext = node->kind;
		if(node->kind == SELECT_ENTERED) {
			struct StackNode newNode = *node;
			/* Need to explicitly duplicate it because could be a string - */
			CopyScalar(&newNode.extension.selectValue, &node->extension.selectValue);
			StkPush(proc->controlFlowStack, &newNode);
		}
		else
			StkPush(proc->controlFlowStack, node);
	}
}

static void Ret(void)
{
	Proc()->currentPosition = PeekTop()->retAddr;
}

void ClearControlFlowStack(void)
{
	StkClear(Proc()->controlFlowStack, DisposeNode);
}

long StackSpaceNeverUsed(void)
{
	return (StkLimit(Proc()->controlFlowStack) - StkHighWaterMark(Proc()->controlFlowStack)) * sizeof(struct StackNode);
}

INLINE const struct Statement *SubprogramContext(const struct Process *proc)
{
	int offset;
	for(offset = 0; offset < StkHeight(proc->controlFlowStack) && PeekAt(proc, offset)->kind != CALL_POSITION; offset++)
		;
	return offset == StkHeight(proc->controlFlowStack) ? NULL : PeekAt(proc, offset)->extension.subprogram;
}

const char *StartOfCurrentSubprogram(void)
{
	const struct Statement *context = SubprogramContext(Proc());
	return context != NULL ? context->method.sub : NULL;
}

bool InStaticContext(const struct Process *proc)
{
	const struct Statement *context
		= proc->staticSubCallNesting > SCOPE_MAIN && proc->functionCallNesting == SCOPE_MAIN
			? SubprogramContext(proc) : NULL;
	return context != NULL && context->staticSub;
}

struct HashTable *CurrentStaticContext(const struct Process *proc)
{
	const struct Statement *context = proc->staticSubCallNesting > SCOPE_MAIN ? SubprogramContext(proc) : NULL;
	return context != NULL && context->staticSub ? context->localStatics : NULL;
}

Error CheckForUnbalancedBlocks(bool inSubprogram)
{
	const char *position = NULL;
	int offset;
	Error errorFound = SUCCESS;
	bool aborted = FALSE;

	for(offset = 0; offset < ControlFlowStackHeight() && !aborted && errorFound == SUCCESS; offset++) {
		const struct StackNode *sn = Peek(offset);
		enum ControlFlow kind = sn->kind;

		position = sn->retAddr;

		if((kind & STMT_MASK) == STMT_FOR)
			errorFound = FORWITHOUTNEXT;
		else if((kind & STMT_MASK) == STMT_IF)
			errorFound = IFWITHOUTENDIF;
		else if((kind & STMT_MASK) == STMT_SELECT)
			errorFound = SELECTWITHOUTENDSELECT;
		else if((kind & STMT_MASK) == STMT_REPEAT)
			errorFound = REPEATWITHOUTUNTIL;
		else if((kind & STMT_MASK) == STMT_WHILE)
			errorFound = WHILEWITHOUTWEND;
		else if(kind == CALL_POSITION) {
			if(inSubprogram)
				aborted = TRUE;
			else
				errorFound = SUBWITHOUTENDSUB;
		}
	}

	if(errorFound != SUCCESS && position != NULL)
		Proc()->currentStatementStart = position;
	
	return errorFound;
}

void PushActivationRecord(const struct Statement *statement)
{
	struct StackNode returnAddress;
	
	assert(IsSubprogram(statement));
	
	returnAddress.kind = CALL_POSITION;
	returnAddress.retAddr = Proc()->currentPosition; /* Start of next stmt. */
	returnAddress.extension.subprogram = statement;
	PushContext(&returnAddress);
}

void DiscardCurrentControlFlow(void)
{
	enum ControlFlow top;
	while((top = CurrentContext()) != CALL_POSITION && top != NO_CONTROL)
		RemoveTop();
}

void ReturnFromSubprogram(void)
{
	if(CurrentContext() == CALL_POSITION) {
		Ret();
		RemoveTop();
	}
}

static const char *RetrieveUntakenBranchDestination(const char *origin)
{
	return origin != NULL && Proc()->untakenBranchCache != NULL ? RetrieveFromCache(Proc()->untakenBranchCache, origin) : NULL;
}

static void CacheUntakenBranchDestination(const char *origin, const char *destination)
{
	CreateUntakenBranchCache();
	if(origin != NULL && destination != NULL && Proc()->untakenBranchCache != NULL)
		/* && RetrieveUntakenBranchDestination(origin) != destination */
		SetInCache(Proc()->untakenBranchCache, origin, (void *)destination);
}

#ifdef DEBUG

void PrintUntakenBranchCacheInfo(FILE *f, const char *origin)
{
	const char *dest = RetrieveUntakenBranchDestination(origin);

	if(dest != NULL) {
		const char *thisModule;
		int line;

		GetLocationInfo(Proc()->buffer, dest, &line, &thisModule);		
		fprintf(f, " {--> %d} ", line);
	}
}

#endif

static bool IndicatesLoop(int offset) { return (Peek(offset)->kind & (STMT_FOR | STMT_REPEAT | STMT_WHILE)) != 0; }

/* For statement caching. Anything in a loop, and any subprogram called by another subprogram,
is considered a piece of code that may be executed many times. This discriminates against loops
fashioned using GOTO - so be it. */
bool InPotentiallyHotPath(void)
{
	int offset;
	
	if(Proc()->callNestLevel > SCOPE_MAIN + 1 || InStaticContext(Proc()))
		return TRUE;
	
	for(offset = 0; offset < ControlFlowStackHeight(); offset++)
		if(IndicatesLoop(offset))
			return TRUE;
		
	return FALSE;
}

#if 0
/* Whether to cache the converted form of a statement, rather than just tokens. */
bool InLoopDominatedPath()
{
	int offset;
	
	for(offset = 0; offset < ControlFlowStackHeight(); offset++) {
		/* To cache converted objects, the statement must have been executed.
			Otherwise, there may be problems with variables not yet existing. */
		if(Peek(offset)->kind & (UNTAKEN_BRANCH | BLOCK_PLACEHOLDER))
			return FALSE;
		
		/* Do not cache converted objects if subprogram calls seem to predominate.
			Converted objects must be purged on each subprogram exit. */
		if(Peek(offset)->kind == CALL_POSITION)
			return FALSE;
		
		/* Only consider caching converted objects if in a loop. */
		if(IndicatesLoop(offset))
			return TRUE;
	}

	return FALSE;
}

/* Assuming no silly things like jumping to a label inside a block structure. */
bool GuaranteedLive()
{
	enum ControlFlow cf = CurrentContext();
	return cf == NO_CONTROL || cf == CALL_POSITION || cf == GOSUB_POSITION;
}
#endif /* not used */

void GoTo_(BObject *arg, unsigned count)
{
	while(InSelectionStatement())
		RemoveTop();
	Proc()->currentPosition = arg[0].value.labelPos;
}

void GoSub_(BObject *arg, unsigned count)
{
	struct StackNode gosubSN;

	gosubSN.kind = GOSUB_POSITION;
	gosubSN.retAddr = Proc()->currentPosition;
	PushContext(&gosubSN);
	Proc()->currentPosition = arg[0].value.labelPos;
}

void Return_(BObject *arg, unsigned count)	
{
	enum ControlFlow kind;

	while((kind = CurrentContext()) != GOSUB_POSITION
		&& kind != NO_CONTROL && kind != CALL_POSITION)
		RemoveTop();
	
	if(kind == GOSUB_POSITION) {
		Ret();
		RemoveTop();
	}
	else
		CauseError(RETURNWITHOUTGOSUB);
}

void ReturnTo_(BObject *arg, unsigned count)
{
	Return_(NULL, 0);
	GoTo_(arg, count);
}

void OnGoTo_(BObject *arg, unsigned count)
{
	long value = GetLong(&arg[0].value.scalar);
	if(value > 0 && value < (long)count)
		GoTo_(arg + value, 1);
}

void OnGoSub_(BObject *arg, unsigned count)
{
	long value = GetLong(&arg[0].value.scalar);
	if(value > 0 && value < (long)count)
		GoSub_(arg + value, 1);
}

void Clear_(BObject *arg, unsigned count)
{
	long controlSize = arg[0].value.scalar.value.number.l, heapSize = arg[1].value.scalar.value.number.l;
	unsigned short height = controlSize > USHRT_MAX * sizeof(struct StackNode)
		? USHRT_MAX : (unsigned short)((controlSize + sizeof(struct StackNode) - 1) / sizeof(struct StackNode));
			/* Size is provided to CLEAR in bytes. */

	ResetStaticFunctionParams();
	ClearStatementCache();

	if(controlSize > 0) {
		DisposeControlFlowStack();
		CreateControlFlowStack(height); /* Height 0 means use default. */
	}

	if(heapSize > 0) {
		/* TODO ClearOutOfContextItems causes problems with the prelude - need to re-execute it,
			or protect prelude definitions from deletion - or make CLEAR operate
			more like in a traditional BASIC where there's no concept of variable lifetime
			because they just always exist - i.e. set vars back to 0/"" and leave functions and
			subs alone */
		/*ClearOutOfContextItems(SCOPE_GLOBAL, SCOPE_CURRENT);*/
			/* Will also obviously cause problems down the track if in a SUB. */
		CloseAllStreams();
			/* Close all files opened by program. */
		
		/* Since we use a dynamically-sized heap by default, increasing the size is a bit
			meaningless. It would be possible to assert that the heap was at least the
			size provided, if in fixed-size mode, but fixed-size mode is going to be
			pretty rarely used, so haven't bothered. */
	}
}

void Stop_(BObject *arg, unsigned count)	 
{
	Proc()->mode = MODE_INTERACTIVE;
}

void End_(BObject *arg, unsigned count)	 
{
	ClearControlFlowStack();
	CloseAllStreams();
	ClearOutOfContextItems(SCOPE_MAIN + 1, SCOPE_CURRENT); /* TODO main, global non-prelude stuff cleared from symtab */
	
	Proc()->mode = MODE_INTERACTIVE;
}

/*extern void ReleaseResources(struct Options *);*/

void System_(BObject *arg, unsigned count)
{
	int rc = GetBoolean(&arg[0].value.scalar) ? EXIT_FAILURE : EXIT_SUCCESS;
	
	fflush(stdout);
	fflush(stderr);
	
	End_(arg, count);
	
	Proc()->mode = MODE_HALTED_FOR_EXIT;
	Proc()->returnCode = rc;
	
	DisposeProcess();
	DisposeHeap();
	/*ReleaseResources(Opts());*/
	PfFinish();
	exit(rc);
}

/* The f.p. version doesn't check for overflow. */
#define ForLoopIsFinishedFP(newTotal, end, step) (((step) > 0 && (newTotal) > (end)) || ((step) < 0 && (newTotal) < (end)))

#define ForLoopIsFinishedLong(newTotal, oldTotal, end, step) \
	(((step) > 0 && ((newTotal) > (end) || LONG_MAX - (step) < (oldTotal))) \
  || ((step) < 0 && ((newTotal) < (end) || LONG_MIN - (step) > (oldTotal))))
  
static enum ControlFlow ForLoopState(double start, double end, double step, enum ControlFlow pushIfEntered)
{
	return ForLoopIsFinishedFP(start, end, step) ? FOR_UNTAKEN : pushIfEntered;
}

void For_(BObject *arg, unsigned count)
{
	void *counter;
	union NumericalValue *startValue, *endValue, *stepSize;
	struct StackNode node;

	counter = GetPointer(VarData(&arg[0]));
	startValue = &arg[1].value.scalar.value.number;
	endValue = &arg[2].value.scalar.value.number;
	stepSize = &arg[3].value.scalar.value.number;

	node.extension.forLoop.counter.sp = (short *)counter; /* Any pointer will do */
	node.extension.forLoop.endValue = *endValue;
	node.extension.forLoop.stepSize = *stepSize;
	node.retAddr = Proc()->currentPosition;

	switch(NonPointer(VarData(&arg[0])->type)) {
		case T_INT:
			*(short *)counter = startValue->s;
			node.kind = ForLoopState(startValue->s, endValue->s, stepSize->s, FOR_INT);
			break;
		case T_LONG:
			*(long *)counter = startValue->l;
			node.kind = ForLoopState(startValue->l, endValue->l, stepSize->l, FOR_LONG);
			break;
		case T_SINGLE:
			*(float *)counter = startValue->f;
			node.kind = ForLoopState(startValue->f, endValue->f, stepSize->f, FOR_SINGLE);
			break;
		case T_DOUBLE:
			*(double *)counter = startValue->d;
			node.kind = ForLoopState(startValue->d, endValue->d, stepSize->d, FOR_DOUBLE);
			break;
		default:
			assert(FALSE);
	}

	/* No untaken branch caching for FOR loops - they're much more efficient than WHILE loops,
		and the cost of the untaken branch cache access is worth avoiding. */

	PushContext(&node);
}

void Next_(BObject *arg, unsigned count)
{
	struct ForLoopControl *loopControl = PeekForLoopControl();
	Error error = SUCCESS;
	bool shouldFinish = FALSE;

	switch(CurrentContext()) {
		case FOR_INT: {
			long newTotal = (long)*loopControl->counter.sp + loopControl->stepSize.s;
			shouldFinish = ForLoopIsFinishedLong(newTotal, *loopControl->counter.sp,
				loopControl->endValue.s, loopControl->stepSize.s);
			*loopControl->counter.sp = (short)newTotal;
			}
			break;
		case FOR_LONG: {
			long newTotal = *loopControl->counter.lp + loopControl->stepSize.l;
			shouldFinish = ForLoopIsFinishedLong(newTotal, *loopControl->counter.lp,
				loopControl->endValue.l, loopControl->stepSize.l);
			*loopControl->counter.lp = newTotal;
			}
			break;
		case FOR_SINGLE:
			*loopControl->counter.fp += loopControl->stepSize.f;
			shouldFinish = ForLoopIsFinishedFP(*loopControl->counter.fp, loopControl->endValue.f, loopControl->stepSize.f);
			break;
		case FOR_DOUBLE:
			*loopControl->counter.dp += loopControl->stepSize.d;
			shouldFinish = ForLoopIsFinishedFP(*loopControl->counter.dp, loopControl->endValue.d, loopControl->stepSize.d);
			break;
		case FOR_UNTAKEN:
			shouldFinish = TRUE;
			break;
		default:
			error = NEXTWITHOUTFOR;
			break;
	}

	if(error != SUCCESS)
		CauseError(error);
	else if(shouldFinish)
		RemoveTop();
	else
		Ret();
}

void NextVar_(BObject *arg, unsigned count)
{
	struct ForLoopControl *loopControl = PeekForLoopControl();
	Error error = SUCCESS;
	unsigned i;
	
	for(i = 0; i != count && error == SUCCESS; i++)
		if(!(CurrentContext() & STMT_FOR))
			error = NEXTWITHOUTFOR;
		else if(GetPointer(VarData(&arg[i])) != (void *)loopControl->counter.sp)
			error = ER_BAD_NEXT_VARIABLE;
		else
			Next_(NULL, 0);
	
	if(error != SUCCESS)
		CauseError(error);
}

void IfGoTo_(BObject *arg, unsigned count)
{
	if(arg[0].value.scalar.value.boolean)
		GoTo_(arg + 1, count - 1);
}

void If_(BObject *arg, unsigned count)
{
	struct StackNode node;
	const char *skip;
	
	node.kind = arg[0].value.scalar.value.boolean ? IF_BLOCK : IF_UNTAKEN;
	node.retAddr = Proc()->currentStatementStart;
	PushContext(&node);
	
	if(node.kind == IF_UNTAKEN && (skip = RetrieveUntakenBranchDestination(node.retAddr)) != NULL)
		Proc()->currentPosition = skip;
}

void ElseIf_(BObject *arg, unsigned count)
{
	if(CurrentContext() == IF_UNTAKEN) {
		CacheUntakenBranchDestination(PeekTop()->retAddr, Proc()->currentStatementStart);
		RemoveTop();
		If_(arg, count);
	}
	else
		CauseError(ELSEIFWITHOUTIF);
}

void Else_(BObject *arg, unsigned count)
{
	if(CurrentContext() == IF_UNTAKEN) {
		struct StackNode node;
		
		CacheUntakenBranchDestination(PeekTop()->retAddr, Proc()->currentStatementStart);
		RemoveTop();
		
		node.kind = ELSE_BLOCK;
		node.retAddr = Proc()->currentStatementStart;
		PushContext(&node);
	}
	else
		CauseError(ELSEIFWITHOUTIF);
}

void EndIf_(BObject *arg, unsigned count)
{
	enum ControlFlow node = CurrentContext();
	
	if(node == IF_UNTAKEN || node == ELSE_BLOCK)
		CacheUntakenBranchDestination(PeekTop()->retAddr, Proc()->currentStatementStart);
	
	if((node & STMT_MASK) & (STMT_IF | STMT_ELSE))
		RemoveTop();
	else
		CauseError(ENDIFWITHOUTIF);
}

static Error EvalCondition(const QString *expr, unsigned len, struct Stack *evalSpace, bool *fired)
{
	Error error;
	if((error = CheckExpressionSyntax(expr, len, Proc()->currentStatementStart)) == SUCCESS) {
		QString *prefixForm = InfixToPrefix(expr, len, NULL);
		if(prefixForm != NULL) {
			Eval(prefixForm, DefaultConvert, 0, evalSpace);
			Dispose(prefixForm);
		
			*fired = StkHeight(evalSpace) == 1 
				&& (error = DereferenceObject(PeekExprStk(evalSpace, 0))) == SUCCESS
				&& GetBoolean(&(PeekExprStk(evalSpace, 0)->value.scalar));
		}
		else
			error = BADSYNTAX;
	}
	return error;
}

static Error CompileAndExecute(const QString *toks, struct Stack *evalSpace)
{
	Error error = SUCCESS;
	const QString *t;
	struct TokenSequence ts;
	const struct Statement *cmd;
	bool hasExplicitStatement;
	
	CreateTokenSequence(&ts, 10);
	ts.start = Proc()->currentStatementStart;
	
	hasExplicitStatement = GetStatement(&toks[0], &cmd) == SUCCESS;
	if(hasExplicitStatement)
		QsCopy(&ts.statementName, &toks[0]);

	for(t = toks + hasExplicitStatement; !IsTerminator(t) && !QsEqNoCase(t, &g_ElseKeyword); t++) {
		ExpandTokenSequence(&ts, ts.length + 1);
		QsCopy(&ts.rest[ts.length++], t);
	}
	ExpandTokenSequence(&ts, ts.length + 1);
	QsCopy(&ts.rest[ts.length++], &g_Pipe);
	
	if((error = Prepare(&ts)) == SUCCESS)
		Do(Proc(), &ts, evalSpace);

	DisposeTokenSequence(&ts);
	
	return error;
}

void IfThenElse_(const QString *toks, unsigned nToks)
{
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
	else {
		struct Stack exprEvalSpace;
		bool fired = FALSE;
	
		CreateExprStk(&exprEvalSpace, thenPosition);
		if((error = EvalCondition(toks, thenPosition, &exprEvalSpace, &fired)) == SUCCESS) {
			const QString *branch = toks + thenPosition + 1;
			ClearExprStk(&exprEvalSpace);
			if(!fired)
				branch = elsePosition != 0 ? toks + elsePosition + 1 : NULL;
			if(branch != NULL)		
				error = CompileAndExecute(branch, &exprEvalSpace);
		}
		DisposeExprStk(&exprEvalSpace);
	}
	
	if(error != SUCCESS)
		CauseError(error);
}

void IfThenLet_(BObject *arg, unsigned count)
{
	bool fired = arg[0].value.scalar.value.boolean;
	if(fired)
		CopyDereferencingBoth(VarData(&arg[1]), &arg[2].value.scalar);
}

void Merge_(BObject *arg, unsigned count)
{
	const QString *fileName = &arg[0].value.scalar.value.string;
	char *mergedFileName = QsDupAsNTS(fileName);
	char *resolvedLocation = ResolveModuleLocation(mergedFileName);
	Error error = LoadProgram(resolvedLocation, FALSE, FALSE);
	
	Dispose(resolvedLocation);
	Dispose(mergedFileName);
	
	if(error != SUCCESS)
		CauseError(error);
}

void Executed_(Scalar *result, const BObject *arg, unsigned count)
{
	const char *moduleName;
	const char *progName = PrimaryBufferFileName(Proc()->buffer);
	int dummyLine;	

	GetLocationInfo(Proc()->buffer, Proc()->currentStatementStart, &dummyLine, &moduleName);
	SetFromLong(result, 
		moduleName != NULL && progName != NULL && PfFilenameCmp(moduleName, progName) == 0,
		T_BOOL);
}

void Repeat_(BObject *arg, unsigned count)
{
	struct StackNode node;

	node.kind = REPEAT_BLOCK;
	node.retAddr = Proc()->currentPosition;
	PushContext(&node);
}

void Until_(BObject *arg, unsigned count)
{
	if(CurrentContext() == REPEAT_BLOCK) {
		if(arg[0].value.scalar.value.boolean)
			RemoveTop();
		else
			Ret();
	}
	else
		CauseError(UNTILWITHOUTREPEAT);
}

void Forever_(BObject *arg, unsigned count)
{
	if(CurrentContext() == REPEAT_BLOCK)
		Ret();
	else
		CauseError(UNTILWITHOUTREPEAT);
}

void Select_(BObject *arg, unsigned count)
{
	struct StackNode node;

	node.kind = SELECT_ENTERED;
	node.retAddr = NULL;
	node.extension.selectValue = arg[0].value.scalar;
	PushContext(&node);
}

void Case_(BObject *arg, unsigned count)
{
	enum ControlFlow context = CurrentContext();
	
	if(context == SELECT_ENTERED) {
		const Scalar *selectVal = PeekSelectValue();
		const char *skip;
		unsigned i;
		bool matchedOne = FALSE;

		CacheUntakenBranchDestination(PeekTop()->retAddr, Proc()->currentStatementStart);

		for(i = 0; i != count && !matchedOne; i++) {
			Error ignored;
			matchedOne = Compare(selectVal, &arg[i].value.scalar, &ignored) == 0;
		}
			
		if(matchedOne) {
			struct StackNode node;
			
			RemoveTop();
			node.kind = CASE_BLOCK;
			node.retAddr = Proc()->currentStatementStart;
			PushContext(&node);
		}
		else if((skip = RetrieveUntakenBranchDestination(Proc()->currentStatementStart)) != NULL)
			Proc()->currentPosition = skip;
	}
	else if(context == DEFAULT_BLOCK)
		CauseError(OTHERWISENOTLASTCLAUSE);
	else
		CauseError(CASEOUTSIDESELECT);
}

void Otherwise_(BObject *arg, unsigned count)
{
	enum ControlFlow context = CurrentContext();
	struct StackNode node;

	node.retAddr = Proc()->currentStatementStart;
	
	if(context == SELECT_ENTERED) {
		CacheUntakenBranchDestination(PeekTop()->retAddr, node.retAddr);
		
		RemoveTop();
		node.kind = DEFAULT_BLOCK;
		PushContext(&node);
	}
	else if(context == CASE_BLOCK) {
		const char *skip;
		
		RemoveTop();
		node.kind = CASE_TAKEN_SKIP_REST;
		PushContext(&node);
		
		if((skip = RetrieveUntakenBranchDestination(node.retAddr)) != NULL)
			Proc()->currentPosition = skip;
	}
	else if(context == DEFAULT_BLOCK)
		CauseError(TWOOTHERWISECLAUSES);
	else
		CauseError(CASEOUTSIDESELECT);
}

void EndSelect_(BObject *arg, unsigned count)
{
	enum ControlFlow context = CurrentContext();
	
	if((context & STMT_MASK) & STMT_SELECT) {
		RemoveTop();
		if(context & UNTAKEN_BRANCH)
			CacheUntakenBranchDestination(PeekTop()->retAddr, Proc()->currentStatementStart);
	}
	else
		CauseError(ENDSELECTWITHOUTSELECT);
}

void While_(BObject *arg, unsigned count)	
{
	struct StackNode node;
	const char *endOfLoop;

	node.kind = arg[0].value.scalar.value.boolean ? WHILE_BLOCK : WHILE_UNTAKEN;
	node.retAddr = Proc()->currentStatementStart;
		/* Doesn't use currentPosition like FOR does, so the whole WHILE statement is rerun
			each time around the loop. */
				
	endOfLoop = !arg[0].value.scalar.value.boolean
		? RetrieveUntakenBranchDestination(Proc()->currentStatementStart) : NULL;

	if(endOfLoop != NULL)
		/* Unlike untaken branch caching for IF and SELECT, which have more complicated block
			structure, just jump over the entire block, avoiding pushing anything on the stack - */
		Proc()->currentPosition = endOfLoop;
	else
		PushContext(&node);
}

void WEnd_(BObject *arg, unsigned count)	
{
	enum ControlFlow kindPresent = CurrentContext();

	if(kindPresent == WHILE_BLOCK) {
		/* Not very efficient, but simple: jump back and re-execute the whole WHILE statement,
			instead of just re-evaluating the condition. */
		Ret();
		RemoveTop();
	}
	else if(kindPresent == WHILE_UNTAKEN) {
		/* This allows the whole block to be jumped over if the condition is false. */
		CacheUntakenBranchDestination(PeekTop()->retAddr, Proc()->currentPosition);
		RemoveTop();
	}
	else
		CauseError(WENDWITHOUTWHILE);
}

void TrOn_(BObject *arg, unsigned count)
{
	/* Statement cache must be cleared because trace mode does not work with some optimisations. */
	ClearStatementCache();
	Proc()->trace = TRUE;
}

void TrOff_(BObject *arg, unsigned count)	
{
	ClearStatementCache();
	Proc()->trace = FALSE;
}

extern void ResetProgram(void);
extern Error LoadPrelude(void);

void Load_(BObject *arg, unsigned count)
{
	const QString *fileName = &arg[0].value.scalar.value.string;
	char *fileNameAsCString;
	FILE *handle;
	
	if(!QsIsNull(fileName))
		fileNameAsCString = QsDupAsNTS(fileName);
	else
	{
		size_t length = strlen(PrimaryBufferFileName(Proc()->buffer));
		fileNameAsCString = New(length + 1); /* Don't use strdup() as need to Dispose() below. */
		/* Save current program name, as it will be deleted by ResetFileBuffer. */
		strcpy(fileNameAsCString, PrimaryBufferFileName(Proc()->buffer));
	}

	/* Check that the file exists. */

	handle = fopen(fileNameAsCString, "r");
	if(handle == NULL) {
		CauseError(CANTOPENCODEFILE);
		Dispose(fileNameAsCString);
		return;
	}
	fclose(handle);

	/* Clear the program buffer, then load the prelude and program as normal: */

	ResetFileBuffer(Proc()->buffer);

	{
		Error error = LoadPrelude();
		if(error == SUCCESS)
			error = LoadProgram(fileNameAsCString, FALSE, FALSE);
		if(error == SUCCESS)
			ResetProgram();
		if(error != SUCCESS)
			CauseError(error);
	}
	
	Dispose(fileNameAsCString);
	
	Proc()->mode = MODE_INTERACTIVE;
}

void Run_(BObject *arg, unsigned count)
{
	ResetProgram();
	Proc()->mode = MODE_RUNNING;
}

static void PrintNodeInfo(int offset)
{
	const struct StackNode *currentNode;
	const char *description;
	enum ControlFlow nodeKind;
	bool isPlaceKeeper, failedTest, madeJump;

	assert(offset < ControlFlowStackHeight());

	currentNode = Peek(offset);
	nodeKind = currentNode->kind;

	/* Categorise: */
	isPlaceKeeper = nodeKind == NESTED_UNTAKEN_FOR || nodeKind == NESTED_UNTAKEN_IF || nodeKind == NESTED_UNTAKEN_WHILE
		|| nodeKind == NESTED_UNTAKEN_REPEAT || nodeKind == NESTED_UNTAKEN_SELECT;
	failedTest = nodeKind == FOR_UNTAKEN || nodeKind == IF_UNTAKEN || nodeKind == IF_TAKEN_SKIP_ELSE
		|| nodeKind == WHILE_UNTAKEN || nodeKind == CASE_TAKEN_SKIP_REST;
	madeJump = nodeKind == GOSUB_POSITION || nodeKind == CALL_POSITION;

	switch(nodeKind) {
		case GOSUB_POSITION:
			description = "GOSUB";
			break;
		case FOR_INT:
		case FOR_LONG:
		case FOR_SINGLE:
		case FOR_DOUBLE:
		case FOR_UNTAKEN:
		case NESTED_UNTAKEN_FOR:
			description = KW_FOR;
			break;
		case IF_BLOCK:
		case IF_UNTAKEN:
		case IF_TAKEN_SKIP_ELSE:
		case NESTED_UNTAKEN_IF:
			description = "IF";
			break;
		case ELSE_BLOCK:
			description = "ELSE";
			break;
		case SELECT_ENTERED:
		case CASE_BLOCK:
		case CASE_TAKEN_SKIP_REST:
			description = "SELECT";
			break;
		case DEFAULT_BLOCK:
			description = "OTHERWISE clause of SELECT";
			break;
		case REPEAT_BLOCK:
		case NESTED_UNTAKEN_REPEAT:
			description = "REPEAT";
			break;
		case WHILE_BLOCK:
		case WHILE_UNTAKEN:
		case NESTED_UNTAKEN_WHILE:
			description = "WHILE";
			break;
		case CALL_POSITION:
			description = "SUB CALL";
			break;
		default:
			description = "? Unknown item";
			break;
	}

	{
		const char *fmt = isPlaceKeeper ? "<%s" : " %s";
		fprintf(stderr, fmt, description);
	}

	if(nodeKind == FOR_INT
	|| nodeKind == FOR_LONG
	|| nodeKind == FOR_SINGLE
	|| nodeKind == FOR_DOUBLE) {
		const struct ForLoopControl *loopInfo = &currentNode->extension.forLoop;
		double ctr, end, step;

		if(nodeKind == FOR_INT) {
			ctr = *loopInfo->counter.sp;
			end = loopInfo->endValue.s;
			step = loopInfo->stepSize.s;
		}
		else if(nodeKind == FOR_LONG) {
			ctr = *loopInfo->counter.lp;
			end = loopInfo->endValue.l;
			step = loopInfo->stepSize.l;
		}
		else if(nodeKind == FOR_SINGLE) {
			ctr = *loopInfo->counter.fp;
			end = loopInfo->endValue.f;
			step = loopInfo->stepSize.f;
		}
		else /* if(nodeKind == FOR_DOUBLE) */ {
			ctr = *loopInfo->counter.dp;
			end = loopInfo->endValue.d;
			step = loopInfo->stepSize.d;
		}
		fprintf(stderr, ": ctr=%f, end=%f, step=%f", ctr, end, step);
	}
	else if(nodeKind == SELECT_ENTERED) {
		fprintf(stderr, " on ");
		WriteScalar(stdout, &currentNode->extension.selectValue);
	}

	if(isPlaceKeeper)
		fprintf(stderr, ">");
	else if(failedTest)
		fprintf(stderr, " (case not entered)");
	else if(madeJump) {
		const char *fileName;
		int lineNum;

		GetLocationInfo(Proc()->buffer, currentNode->retAddr, &lineNum, &fileName);
		fprintf(stderr, " from line no. %d in file %s", lineNum - 1, fileName);
	}
	fprintf(stderr, "\n");
}

void PrintStackTrace(int maxDepth)
{
	int stackHeight = ControlFlowStackHeight(), offset;

	if(maxDepth > stackHeight)
		maxDepth = stackHeight;

	fprintf(stderr, "---- Stack trace: ----\n");
	fprintf(stderr, "---- top of stack ----\n");
	for(offset = 0; offset < maxDepth; offset++)
		PrintNodeInfo(offset);

	if(maxDepth == stackHeight)
		fprintf(stderr, "---- bottom of stack ----\n");
	else
		fprintf(stderr, "---- %d item(s) below ----\n", stackHeight - maxDepth);
}

#ifdef DEBUG

extern void DumpCache(const struct Cache *cache);

void XFree_(BObject *arg, unsigned count)
{	
	fprintf(stderr, "---- Memory status ----\n");
	fprintf(stderr, "-- Heap --\n");
	PrintHeapStatus();
	fprintf(stderr, "-- Program buffer --\n");
	PrintFileBufferInfo(Proc()->buffer);
	fprintf(stderr, "-- Symbol table --\n");
	PrintSymTabStatus();
	PrintSymTab();
	fprintf(stderr, "-- QString --\n");
	QsPrintMemInfo();
	fprintf(stderr, "-- Statement cache --\n");
	PrintCacheInfo(Proc()->statementCache);
	DumpCache(Proc()->statementCache);
	fprintf(stderr, "-- Untaken branch cache --\n");
	PrintCacheInfo(Proc()->untakenBranchCache);
	DumpCache(Proc()->untakenBranchCache);
	fprintf(stderr, "----\n");
}

void XStack_(BObject *arg, unsigned count)
{
	PrintStackTrace(arg[0].value.scalar.value.number.s);
}

void XCache_(BObject *arg, unsigned count)
{
	fprintf(stderr, "-- Statement cache --\n");
	PrintCacheInfo(Proc()->statementCache);
	DumpCache(Proc()->statementCache);
	fprintf(stderr, "-- Untaken branch cache --\n");
	PrintCacheInfo(Proc()->untakenBranchCache);
	DumpCache(Proc()->untakenBranchCache);
}

void XObj_(const QString *tok, unsigned nTok)
{
	const BObject *defn;
	short nestLevel = Proc()->callNestLevel;
	
	if(nTok >= 3) {
		Scalar nl;
		if(ParseNumber(&tok[2], &nl) == SUCCESS)
			nestLevel = (short)GetLong(&nl);
	}
	
	defn = LookUp(&tok[0], nestLevel);
	if(defn != NULL) {
		DumpObject(defn);
		fputc('\n', stdout);
	}
	else
		fprintf(stderr, "Not found.\n");
}

extern void VisitAllDefinitions(short, HtVisitor);

static bool PrintObjectSignature(unsigned binIndex, const QString *key, const void *val, void *unusedExtraParam)
{
	const BObject *obj = val;

	if (obj->category == FUNCTION || obj->category == STATEMENT) {
		const struct Parameter *param
			= obj->category == FUNCTION ? obj->value.function->parameter : obj->value.statement->formal;
		int nParams = obj->category == FUNCTION ? obj->value.function->numArgs : obj->value.statement->formalCount;
		int n;
		char pLetter = 'a';

		QsWrite(key, stdout);

		if (obj->category == FUNCTION && (obj->value.function->type & T_STRING))
			putc('$', stdout);

		if (obj->category == FUNCTION && nParams != 0)
			putc('(', stdout);

		for (n = 0; n < nParams; n++) {
			const struct Parameter *pn = &param[n];
			unsigned short repeat, repend = pn->maxCount == MAX_TOKENS ? 1 : pn->maxCount;

			for (repeat = 1; repeat <= repend; repeat++) {
				if (n > 0 || obj->category == STATEMENT)
					putc(' ', stdout);

				if (pn->defaultValue != NULL)
					putc('[', stdout);

				if (pn->kind == LABEL)
					fputs("label", stdout);
				else
					putc((pn->kind & VARIABLE_IS_ARRAY) ? toupper(pLetter) : pLetter, stdout);

				if (((pn->kind & IS_VARIABLE) || (pn->kind == LITERAL))
					&& !Contextual(pn->type))
					putc(SpecifierFromType(TypeUsuallyProducedBy(pn->type)), stdout);

				if (pn->kind & VARIABLE_IS_ARRAY)
					fputs("()", stdout);

				++pLetter;

				if (pn->defaultValue != NULL)
					putc(']', stdout);

				if (pn->maxCount == MAX_TOKENS)
					fputs(" ...", stdout);

				if (n + 1 < nParams || repeat + 1 < repend)
					putc(',', stdout);
			}
		}

		if (obj->category == FUNCTION && nParams != 0)
			putc(')', stdout);

		putc('\n', stdout);
	}

	return TRUE;
}

void XDoc_(BObject *arg, unsigned count)
{
	VisitAllDefinitions(SCOPE_BUILTIN, &PrintObjectSignature);
}

#endif /* DEBUG */

static void ResolveLabel(const QString *token, BObject *result)
{
	Error error;
		
	result->category = LABEL;
	result->value.labelPos = FindReferencedLabel(token, &error);
	if(error != SUCCESS)
		SetObjectToError(result, error);
}

/* GOTO <label>
   GOSUB <label>
   RETURN <label> --> RETURNTO~ <label> */
void JumpConvert(unsigned index, const QString *token, BObject *result)
{
	if(index == 0 && IsValidLabelOrLineNumber(token))
		ResolveLabel(token, result);
	else
		ConvertToObject(token, result, SCOPE_CURRENT);
}

/* IF <cond> GOTO <label> --> IFGOTO~ <cond>; <label>
   ON <expr> {GOTO|GOSUB} <label1>[, <label2> ...] --> ON{GOTO|GOSUB|~ <expr>; <label1>[; <label2> ...] */
void ConditionalJumpConvert(unsigned index, const QString *token, BObject *result)
{
	int nesting = 0;
	unsigned i;
	const QString *baseToken = token - (int)index;
	
	for(i = 0; i != index; i++)
		Nest(&baseToken[i], &nesting); /* compiler */
	
	if(index > 0 && nesting == 0 && IsValidLabelOrLineNumber(token))
		ResolveLabel(token, result);
	else
		ConvertToObject(token, result, SCOPE_CURRENT);
}

/*** Inactive statements, a.k.a. untaken branch handling ***/

/* 'Inactive' statement methods are called when not executing code; for example,
in the non-taken branch(es) of an IF...THEN...ELSE statement. They keep track of
nested control structures. */

/* SkippingWhenExecuting returns TRUE if in a non-taken branch of a control flow
statement. This is a generic test based on the control flow stack state - 
for specific statements, more complicated tests are provided below. */
INLINE bool SkippingWhenExecuting(void)
{
	enum ControlFlow nodeKind = CurrentContext();
	return (nodeKind & UNTAKEN_BRANCH) || nodeKind == SELECT_ENTERED;
}

INLINE bool Skipping(bool syntaxCheckingMode)
{
	return SkippingWhenExecuting() || syntaxCheckingMode;
}

static bool PushPlacekeeper(enum ControlFlow kind)
{
	struct StackNode placeKeeper;
	placeKeeper.kind = kind;
	placeKeeper.retAddr = Proc()->currentStatementStart; /* For error reporting and untaken branch caching. */
	PushContext(&placeKeeper);
	return TRUE;
}

static bool PopMatchingPlacekeeper(enum ControlFlow kind)
{
	bool matched = CurrentContext() == kind;
	if(matched)
		RemoveTop();
	return matched;
}

static bool HandleComplexInactiveBehaviour(
	enum ControlFlow tryThisBranch, /* clause might fire */
	enum ControlFlow errorCase, /* indicates bad syntax */
	enum ControlFlow skipThis, /* has already fired ... */
	enum ControlFlow placekeeper) /* ... so push this */
{
	enum ControlFlow kind = CurrentContext();
	if(kind == skipThis) {
		const char *skip;
		
		CacheUntakenBranchDestination(PeekTop()->retAddr, Proc()->currentStatementStart);
		
		if((skip = RetrieveUntakenBranchDestination(Proc()->currentStatementStart)) != NULL)
			Proc()->currentPosition = skip;

		RemoveTop();
		PushPlacekeeper(placekeeper);
		
		return TRUE;
	}
	else 
		return kind != tryThisBranch && kind != errorCase;
}

bool DefaultInactive(bool syntaxCheckingMode) { return Skipping(syntaxCheckingMode); }

bool EmptyInactive(bool syntaxCheckingMode) { return TRUE; }

bool DataInactive(bool syntaxCheckingMode) { return FALSE; }

bool IfInactive(bool syntaxCheckingMode) { return Skipping(syntaxCheckingMode) && PushPlacekeeper(NESTED_UNTAKEN_IF); }

bool ForInactive(bool syntaxCheckingMode) { return Skipping(syntaxCheckingMode) && PushPlacekeeper(NESTED_UNTAKEN_FOR); }

bool WhileInactive(bool syntaxCheckingMode) { return Skipping(syntaxCheckingMode) && PushPlacekeeper(NESTED_UNTAKEN_WHILE); }

bool RepeatInactive(bool syntaxCheckingMode) { return Skipping(syntaxCheckingMode) && PushPlacekeeper(NESTED_UNTAKEN_REPEAT); }

bool SelectInactive(bool syntaxCheckingMode) { return Skipping(syntaxCheckingMode) && PushPlacekeeper(NESTED_UNTAKEN_SELECT); }

bool EndIfInactive(bool syntaxCheckingMode) { return Skipping(syntaxCheckingMode) && PopMatchingPlacekeeper(NESTED_UNTAKEN_IF); }

bool WEndInactive(bool syntaxCheckingMode) { return Skipping(syntaxCheckingMode) && PopMatchingPlacekeeper(NESTED_UNTAKEN_WHILE); }

bool NextInactive(bool syntaxCheckingMode) { return Skipping(syntaxCheckingMode) && PopMatchingPlacekeeper(NESTED_UNTAKEN_FOR); }

bool UntilInactive(bool syntaxCheckingMode) { return Skipping(syntaxCheckingMode) && PopMatchingPlacekeeper(NESTED_UNTAKEN_REPEAT); }

bool EndSelectInactive(bool syntaxCheckingMode) { return Skipping(syntaxCheckingMode) && PopMatchingPlacekeeper(NESTED_UNTAKEN_SELECT); }

bool ElseInactive(bool syntaxCheckingMode)
{
	enum ControlFlow nodeKind = CurrentContext();
	return nodeKind == NESTED_UNTAKEN_IF
		|| HandleComplexInactiveBehaviour(IF_UNTAKEN, ELSE_BLOCK, IF_BLOCK, IF_TAKEN_SKIP_ELSE);
}

bool CaseInactive(bool syntaxCheckingMode)
{
	enum ControlFlow nodeKind = CurrentContext();
	return nodeKind == NESTED_UNTAKEN_SELECT
		|| HandleComplexInactiveBehaviour(SELECT_ENTERED, DEFAULT_BLOCK, CASE_BLOCK, CASE_TAKEN_SKIP_REST);
}

/* Since subs don't nest, always execute END SUB - */
bool EndSubInactive(bool syntaxCheckingMode) { return FALSE; }

/* Used for statements which are restricted to use within a subprogram: EXIT SUB, LOCAL, etc.
If not in a subprogram, the statement's ordinary method will then be called, and cause an error. */
bool SubprogramOnlyInactive(bool syntaxCheckingMode)
{
	return Proc()->callNestLevel > SCOPE_MAIN && Skipping(syntaxCheckingMode);
}
