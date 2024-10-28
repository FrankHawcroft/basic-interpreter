/****** functions.c ******/

/* 
	$VER: functions.c 0.16A(6.11.2015)

	Built-in BASIC function name and parameter definitions and some implementations,
	support for function evaluation including tail recursion detection, and the DEF
	statement for defining new functions.
*/

#include <limits.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>
#include <time.h>
#include "interpreter.h"
#include "process.h"
#include "builtin.h"
#include "options.h"
#include "buffer.h"
#include "platform.h"
#include "heap.h"
#include "stack.h"
#include "basicio.h"
#include "hashtable.h"

/*** Support for both kinds of function - built-in and programatically defined ***/

#define DefineFunction(name, fcn, scope) DefineSymbol(name, fcn, FUNCTION, scope)

static void CallProgramaticallyDefinedFunction(
	const struct Function *function,
	BObject *result,
	BObject *arg,
	struct Stack *workingStack,
	bool *tailCall);

static void CallPrecompiledProgramaticallyDefinedFunction(
	const struct Function *function,
	BObject *result,
	BObject *arg,
	struct Stack *workingStack,
	bool *tailCall,
	struct Process *proc);

void CallFunction(BObject *result, const struct Function *function, BObject *param, unsigned count, struct Stack *stk)
{
	InitObject(result, LITERAL);

	if(IsDefFunction(function)) {
		struct Process *proc = Proc();
		bool tailCall = FALSE;

		if(function->def->compiled)
			CallPrecompiledProgramaticallyDefinedFunction(function, result, param, stk, &tailCall, proc);
		else
			CallProgramaticallyDefinedFunction(function, result, param, stk, &tailCall);

		while(tailCall)
			CallPrecompiledProgramaticallyDefinedFunction(function, result, param, stk, &tailCall, proc);
	}
	else
		(*function->method)(&result->value.scalar, param, count);
}

static void DisposeStringVector(QString *v, short length)
{
	assert(!(length > 0 && v == NULL));

	if(v != NULL) {
		short i;
		for(i = 0; i < length; i++)
			QsDispose(&v[i]);
		Dispose((void *)v);
	}
}

static void DisposeExpr(struct CompiledExpr *expr, bool converted)
{
	if(expr->body.s != NULL && expr->length > 0) {
		if(!converted)
			DisposeStringVector((QString *)expr->body.s, expr->length);
		else {
			short n;
			for(n = 0; n < expr->length; n++)
				DisposeIfScalar((BObject *)&expr->body.obj[n]);
			Dispose((BObject *)expr->body.obj);
		}
	}
}

void DisposeFunction(struct Function *f)
{
	assert(f != NULL);

	if(IsDefFunction(f)) {
		struct Piece *piece, *savedNext;
		for(piece = f->def; piece != NULL; piece = savedNext) {
			savedNext = piece->next;
			DisposeExpr(&piece->condition, f->staticFunction && piece->compiled);
			DisposeExpr(&piece->value, f->staticFunction && piece->compiled);
			Dispose(piece);
		}
	}

	if(f->parameter != NULL)
		/* Assume parameter names aren't dynamically generated,
			so no need to QsDispose each one. */
		Dispose(f->parameter);

	if(f->predefinedParameter != NULL)
		Dispose(f->predefinedParameter);

	Dispose(f);
}

#ifdef DEBUG

static void PrintFnExprTokens(const QString *t, short n)
{
	short i;
	for(i = 0; i < n; i++) {
		QsWrite(t++, stdout);
		putchar(' ');
	}
}

static void PrintFunctionInfo(const struct Function *f)
{
	if(IsDefFunction(f)) {
		struct Piece *piece;
		int i;

		printf("function of %hd parameter(s), result type = %d\n"
			"workspace size = %hu\n", f->numArgs, f->type,
			f->workSpaceSize);

		if(f->numArgs > 0) {
			printf("parameters:");
			for(i = 0; i < f->numArgs; i++) {
				putchar(' ');
				QsWrite(&f->parameter[i].name, stdout);
			}
			putchar('\n');
		}
		puts("pieces:");
		for(piece = f->def; piece != NULL; piece = piece->next) {
			printf("  guard cond: ");
			if(piece->condition.body.s == NULL)
				printf("<default>");
			else if(f->staticFunction && piece->compiled) {
				for(i = 0; i < piece->condition.length; i++)
					DumpObject(&piece->condition.body.obj[i]);
			}
			else
				PrintFnExprTokens(piece->condition.body.s, piece->condition.length);
			putchar('\n');
			printf("  value: ");
			if(f->staticFunction && piece->compiled) {
				for(i = 0; i < piece->value.length; i++)
					DumpObject(&piece->value.body.obj[i]);
			}
			else
				PrintFnExprTokens(piece->value.body.s, piece->value.length);
			putchar('\n');
		}
	}
	else
		printf("built-in function, method ptr = ....%hX\n", PointerDisplayValue(f->method));
}

#endif /* DEBUG */

/*** DEFined functions ***/

/* Functions differ from subprograms in that main-program-scoped variables - not just named CONSTs -
	are visible by default - they don't need to be SHARED. In fact, there's currently no way to
	explicitly specify shared variables in a function, so this 'traditional' BASIC behaviour 
	is necessary to allow efficient dynamic programming using global arrays. */
static void FunctionExpressionConvert(unsigned index, const QString *token, BObject *result)
{
	ConvertToObject(token, result, SCOPE_CURRENT);
	if(GetSimpleType(result) == T_EMPTY) {
		BObject *global = LookUpCheckingType(token, SCOPE_MAIN);
		if(global != NULL) {
			if(IsVariable(global))
				SetSymbolReference(result, global->category | VARIABLE_IS_POINTER, VarPtr(global));
			else
				*result = *global;
		}
		else if(LookUpIgnoringType(token, SCOPE_MAIN) != NULL)
			SetObjectToErrorWithAdditionalMessage(result, BADARGTYPE, "Global variable type differs for: %.*s", token);
		else
			SetAdditionalErrorMessage("Not found: %.*s", QsGetData(token), QsGetLength(token));
	}
}

static void StaticFunctionExpressionConvert(unsigned index, const QString *token, BObject *result)
{
	struct Variable *sVar = NULL;
	if(IsName(token) && Proc()->staticFunctionParams != NULL) {
		QString typelessToken;
		/* Reliable type checking isn't possible for static params, due to being shared. */
		QsInitStatic(&typelessToken, QsGetData(token), QsGetLength(token) - IsTypeSpecifier(QsGetLast(token)));
		sVar = HtLookUp(Proc()->staticFunctionParams, &typelessToken);
	}
	
	if(sVar != NULL)
		SetSymbolReference(result, (sVar->dim.few[0] != -1 ? ARRAY : SCALAR_VAR) | VARIABLE_IS_POINTER, sVar);
	else
		FunctionExpressionConvert(index, token, result);
}

#ifdef DEBUG
extern void PrintVerboseTracingPrefix(char pass);
#endif

static QString *AsPrefix(const QString *infixExpr, short *nTokens, const char *base, Error *error)
{
	QString *prefixForm = NULL;
	
	*error = SUCCESS;
	
	if(*nTokens > 0) {
		unsigned prefixFormLength = 0;

		if((*error = CheckExpressionSyntax(infixExpr, *nTokens, base)) != SUCCESS)
			return NULL;

		if((prefixForm = InfixToPrefix(infixExpr, *nTokens, &prefixFormLength)) == NULL) {
			*error = BADSYNTAX;
			return NULL;
		}
		
		*nTokens = (short)prefixFormLength;
		
		if(Opts()->optimise) {
			struct TokenSequence optimised;
			
			CreateTokenSequence(&optimised, 0);
			optimised.length = optimised.capacity = (unsigned short)prefixFormLength;
			optimised.rest = prefixForm;
			Improve(&optimised);

#ifdef DEBUG		
			if(Opts()->verbose) {
				PrintVerboseTracingPrefix('f');
				PrintTokSeq(&optimised);
			}
#endif
			
			prefixForm = optimised.rest; /* May lead to slight memory leak, but quick. */
			*nTokens = optimised.length;
		}
	}
	
	return prefixForm;
}

static Error Compile(struct Piece *piece, bool convert)
{
	QString *condExpr, *valExpr = NULL;
	Error error = SUCCESS;
	short previousCondExprLength = piece->condition.length, previousValExprLength = piece->value.length;
	
	if(piece->compiled)
		return SUCCESS;
	
	condExpr = AsPrefix(piece->condition.body.s, &piece->condition.length, piece->defStart, &error);
	
	if(error == SUCCESS)
		valExpr = AsPrefix(piece->value.body.s, &piece->value.length, piece->defStart, &error);
	
	if(error == SUCCESS) {
		DisposeStringVector((QString *)piece->condition.body.s, previousCondExprLength);
		DisposeStringVector((QString *)piece->value.body.s, previousValExprLength);
		
		if(convert) {
			short n;
			
			piece->condition.body.obj = piece->condition.length > 0 ? New(sizeof(BObject) * piece->condition.length) : NULL;
			piece->value.body.obj = New(sizeof(BObject) * piece->value.length);
			
			for(n = 0; n < piece->condition.length; n++)
				StaticFunctionExpressionConvert(n, &condExpr[n], (BObject *)&piece->condition.body.obj[n]);	
			for(n = 0; n < piece->value.length; n++)
				StaticFunctionExpressionConvert(n, &valExpr[n], (BObject *)&piece->value.body.obj[n]);
			
			DisposeStringVector(condExpr, piece->condition.length);
			DisposeStringVector(valExpr, piece->value.length);
		}
		else {
			piece->condition.body.s = condExpr;
			piece->value.body.s = valExpr;
		}
		
		piece->compiled = TRUE;
	}
	
	return error;
}

static void TailCall(
	const struct Function *function,
	const struct Piece *piece,
	BObject *result,
	struct Stack *stk,
	struct Process *proc)
{
	Error conversionError;
	int preHeight = StkHeight(stk), argCount;
	BObject *arg;
	
	/* Just evaluate actual parameters, assuming the form (func <expr1> <expr2> ...) */
	
	if(function->staticFunction)
		EvalPreconverted(&piece->value.body.obj[2], stk, piece->value.length);
	else
		Eval(&piece->value.body.s[2], &FunctionExpressionConvert, 0, stk);

	argCount = StkHeight(stk) - preHeight;
	arg = PeekExprStk(stk, argCount - 1);
	
	conversionError = Conform(function->parameter, function->numArgs, arg, argCount);

	if(conversionError != SUCCESS)
		SetObjectToError(result, conversionError);
	else {
		int argIdx;
		
		/* Assign directly to existing parameter variables: */
		
		if(function->staticFunction)
			for(argIdx = 0; argIdx < function->numArgs; argIdx++, arg++) {
				struct Variable *v = function->predefinedParameter[argIdx];
				CopyDereferencingBoth(&v->value, &arg->value.scalar);
			}
		else
			for(argIdx = 0; argIdx < function->numArgs; argIdx++, arg++)
				CopyDereferencingBoth(
					VarData(LookUpIgnoringType(&function->parameter[argIdx].name, proc->callNestLevel)),
					&arg->value.scalar);
	}
	
	CutExprStk(stk, argCount);
}

static void DeleteVar(void *v)
{
	DisposeScalar(&((struct Variable *)v)->value);
	Dispose(v);
}

static BObject *EvalFunctionExpr(const struct CompiledExpr *expr, bool preconverted, struct Stack *stk)
{
	if(preconverted)
		EvalPreconverted(expr->body.obj, stk, expr->length);
	else
		Eval(expr->body.s, &FunctionExpressionConvert, 0, stk);
	return PeekExprStk(stk, 0);
}

static bool Profiling(const struct Process *p)
{
	return p->opts->profileDest != NULL;
}

static void BeginProfileEntry(const struct Process *proc, PfHighResolutionTimeStamp *startTime)
{
	if(Profiling(proc))
		PfRecordTime(startTime);
}

static void CompleteProfileEntry(
	struct Process *proc,
	const struct Piece *piece,
	const PfHighResolutionTimeStamp *startTime)
{
	if(Profiling(proc) && piece != NULL)
		RecordExecution(proc->profile, proc->buffer, piece->defStart, PfGetElapsedTimeSince(startTime));
}

static Error CreateArguments(struct Process *proc, bool tailCall, const struct Function *f, BObject *arg)
{
	Error result = SUCCESS;
	short argIdx;
	bool predefine = f->staticFunction, firstTime = !f->def->compiled;

	if(tailCall)
		return SUCCESS;

	++proc->callNestLevel;
	++proc->functionCallNesting;

	if(predefine && proc->staticFunctionParams == NULL)
		proc->staticFunctionParams = HtCreate(23, &DeleteVar, NULL);

	for(argIdx = 0; result == SUCCESS && argIdx < f->numArgs; argIdx++) {
		const struct Parameter *p = &f->parameter[argIdx];

		if(predefine) {
			/* Ensure var is defined. Static fcn params are shared globally between all fcns.
				Types are not respected(for speed) - the type is overwritten from the parameter definition.
				This recycling means making a non-leaf function STATIC will usually cause problems! */

			struct Variable *v = f->predefinedParameter[argIdx];

			if(v == NULL && (v = HtLookUp(proc->staticFunctionParams, &p->name)) == NULL) {
				if((v = TolerantNew(sizeof(struct Variable))) != NULL) {
					v->dim.few[0] = v->dim.few[1] = -1; /* vvv */
					HtAdd(proc->staticFunctionParams, &p->name, v);
					InitScalar(&v->value, TypeUsuallyProducedBy(p->type), FALSE);					
				}
				else
					result = NOMEMORY;
			}

			if(result == SUCCESS) {
				f->predefinedParameter[argIdx] = v;
				if(v->value.type != arg[argIdx].value.scalar.type) {
					DisposeScalar(&v->value);
					InitScalar(&v->value, TypeUsuallyProducedBy(p->type), FALSE);
				}
				CopyDereferencingBoth(&v->value, &arg[argIdx].value.scalar);
			}
		}
		else /* an ordinary dynamic parameter */
			result = !firstTime || CanDefineVariable(&p->name, proc->callNestLevel)
				? CreateArgumentVariable(p, &arg[argIdx]) : REDEFINE;
	}
	
	return result;
}

static void ClearArguments(struct Process *proc, bool tailCall)
{
	if(!tailCall) {
		ClearOutOfContextItems(proc->callNestLevel, proc->callNestLevel);
		--proc->callNestLevel;
		--proc->functionCallNesting;
	}
}

#define PopObject(stk, obj) StkPop(stk, obj)

/* Call a function, compiling pieces as necessary and doing thorough error checking. */
static void CallProgramaticallyDefinedFunction(
	const struct Function *function, 
	BObject *result,
	BObject *arg,
	struct Stack *workingStack,
	bool *tailCall)
{
	/* Dummy 'formal parameters' - used to convert piece expression results ... */
	static const struct Parameter booleanConversion = { LITERAL, TR_LOGICAL, NULL, NO_NAME, 1, FALSE };
	/* ... and return values. */
	static const struct Parameter returnValueConversion = { LITERAL, TR_ANY, NULL, NO_NAME, 1, FALSE };

	struct Process *proc = Proc();
	struct Piece *piece;
	Error conversionError;
	PfHighResolutionTimeStamp startTime;	
	
	BeginProfileEntry(proc, &startTime);

	*tailCall = FALSE;
	
	/* Create parameter variables in symtab and assign them their actual values. */

	SetObjectToError(result, CreateArguments(proc, *tailCall, function, arg));
			
	/* Compile all the pieces.
		Memory fragmentation alert - but static functions need the parameters defined first, to convert the
		expressions, and the code is tidier without two CreateArgument calls ...*/

	for(piece = function->def; !IndicatesError(result) && piece != NULL; piece = piece->next)
		SetObjectToError(result, Compile(piece, function->staticFunction));

	/* Find out which piece applies by evaluating the piece conditions.
	If a null condition is encountered, it's the default. */

	{
		bool fired = FALSE;
		for(piece = function->def;
		  !IndicatesError(result) && piece != NULL && piece->condition.body.s != NULL && !fired;
		  piece = fired ? piece : piece->next) {
			EvalFunctionExpr(&piece->condition, function->staticFunction, workingStack);
			PopObject(workingStack, result);
			conversionError = Conform(&booleanConversion, 1, result, 1);
			if(conversionError != SUCCESS) {
				DisposeIfScalar(result);
				SetObjectToError(result, conversionError);
			}
			else {
				fired = GetBoolean(&result->value.scalar);
				DisposeIfScalar(result);
			}
		}
	}

	/*fprintf(stderr, "Evaled conds\n");*/
	
	if(piece == NULL && !IndicatesError(result))
		SetObjectToError(result, OUTSIDEDOMAIN);
	
	if(piece != NULL && !IndicatesError(result)) {
		/* Evaluate the piece: */
	
		*tailCall = piece->tailCall;
	
		if(*tailCall) {
			/*fprintf(stderr, "Tail call\n");*/
			TailCall(function, piece, result, workingStack, proc);
			*tailCall = !IndicatesError(result); /* Need to unwind on error. */
		}
		else {
			struct Parameter returnValueConverter = returnValueConversion;	
			returnValueConverter.type = function->type;

			EvalFunctionExpr(&piece->value, function->staticFunction, workingStack);		
			PopObject(workingStack, result);

			/* Check and convert the return value if necessary: */
			
			conversionError = Conform(&returnValueConverter, 1, result, 1);
			if(conversionError != SUCCESS)
				SetObjectToError(result, conversionError);
			
			/*fprintf(stderr, "Finished evaling\n");*/
		}
	}
	
	/* Remove actuals from symtab, and decrement call nest level. */

	ClearArguments(proc, *tailCall);
	
	/* If profiling, count this piece of the function: */

	CompleteProfileEntry(proc, piece, &startTime);
}

/* Call a function assumed to have already executed successfully, meaning some checks can be skipped. */
static void CallPrecompiledProgramaticallyDefinedFunction(
	const struct Function *function, 
	BObject *result,
	BObject *arg,
	struct Stack *workingStack,
	bool *tailCall,
	struct Process *proc)
{
	struct Piece *piece;
	Error error = SUCCESS;
	int preHeight = StkHeight(workingStack);	
	PfHighResolutionTimeStamp startTime;

	BeginProfileEntry(proc, &startTime);
	
	error = CreateArguments(proc, *tailCall, function, arg);
	
	{
		bool fired = FALSE;
		for(piece = function->def;
		  piece != NULL && piece->condition.body.s != NULL && !fired && error == SUCCESS;
		  piece = fired ? piece : piece->next) {
			/* Rather than popping each time, accumulate results on the stack, then cut it back. */
			BObject *obj = EvalFunctionExpr(&piece->condition, function->staticFunction, workingStack);
			error = ObjectAsError(obj); /* was: Dereference */
			fired = error == SUCCESS && GetBoolean(IsVariable(obj) ? VarData(obj) : &obj->value.scalar);
		}
	}
	
	if(piece == NULL && error == SUCCESS)
		error = OUTSIDEDOMAIN;
	
	if(error == SUCCESS) {
		*tailCall = piece->tailCall;
	
		if(*tailCall) {
			TailCall(function, piece, result, workingStack, proc);
			*tailCall = !IndicatesError(result);
		}
		else {
			CopyObject(result, EvalFunctionExpr(&piece->value, function->staticFunction, workingStack));
			if((error = DereferenceObject(result)) == SUCCESS)
				error = ChangeType(&result->value.scalar, function->type);
		}
	}
	
	if(error != SUCCESS)
		SetObjectToError(result, error);

	/* Drop intermediate results en masse to save individual stack popping overhead. */
	CutExprStk(workingStack, StkHeight(workingStack) - preHeight);
	
	ClearArguments(proc, *tailCall);
	
	CompleteProfileEntry(proc, piece, &startTime);
}

/* TODO doesn't cope with redundant parentheses around the invocation */
static bool DetermineIfTailCall(const QString *name, const struct CompiledExpr *valExpr)
{
	int nesting = 0;
	short i;
	
	if(!QsEqNoCase(name, &valExpr->body.s[0]))
		return FALSE;
	
	for(i = 1; i < valExpr->length; i++) {
		Nest(&valExpr->body.s[i], &nesting);
		if(nesting == 0 && QsGetFirst(&valExpr->body.s[i]) != ')')
			return FALSE;
	}
	
	return TRUE;
}

static QString *CopyTokens(const QString *from, int count)
{
	int i;
	QString *to = count > 0 ? New(sizeof(QString) * count) : NULL;
	
	for(i = 0; i < count; i++)
		QsCopy(&to[i], &from[i]);
	
	return to;
}

static const QString *MatchingRParen(const QString *lparen)
{
	const QString *s = lparen;
	int nesting = 0;
	
	assert(s != NULL && QsGetFirst(s) == '(');
	
	for( ; (QsGetFirst(s) != ')' || nesting > 1) && !IsTerminator(s); s++)
		Nest(s, &nesting);
	
	return QsGetFirst(s) != ')' ? NULL : s;
}

static bool Redefinition(const struct Function *f, enum TypeRule newType, const struct Parameter *newParam,
	short newParamCount, const struct Piece *newPiece)
{
	if(!IsDefFunction(f) || f->type != newType || f->numArgs != newParamCount) {
		/*fprintf(stderr, "redef 1: %d, %d, %d\n", !IsDefFunction(f), f->type != newType, f->numArgs != newParamCount);*/
		return TRUE;
	}
	
	{
		short i;
		for(i = 0; i < newParamCount; i++)
			if(!QsEqNoCase(&f->parameter[i].name, &newParam[i].name) || f->parameter[i].type != newParam[i].type) {
				/*fprintf(stderr, "redef 2: %d, %d, %d, %d\n", !QsEqNoCase(&f->parameter[i].name, &newParam[i].name), 
					f->parameter[i].type != newParam[i].type, f->parameter[i].type, newParam[i].type);
				QsWrite(&newParam[i].name, stderr);
				fputc('\n', stderr);
				QsWrite(&f->parameter[i].name, stderr);*/
				return TRUE;
			}
	}

	/* Check for multiple fall-through cases. Only the absence of a condition is checked -
		DEF a(x) where 1 as "foo"
		DEF a(x) where 1 as "bar"
	isn't detected as a redefinition. */

	if(newPiece->condition.body.s == NULL) {
		const struct Piece *p;
		for(p = f->def; p != NULL; p = p->next)
			if(p->condition.body.s == NULL) {
				/*fprintf(stderr, "redef 3\n");*/
				return TRUE;
			}
	}
	
	return FALSE;
}

void Def_(const QString *toks, unsigned nToks)
{
	QString name;
	const QString *condExpr = NULL, *valueExpr, *scan;
	BObject *definition;
	struct Piece *piece;
	struct Parameter *params = NULL;
	short condExprLength = 0, valExprLength = 0;
	short paramCount = 0, workSpaceSize;
	enum TypeRule outType;
	bool isStatic = FALSE;

	/* Shortest function is(e.g.): DEF f AS 1 | */
	
	if(nToks < 4 || !IsName(&toks[0])) {
		CauseError(BADSYNTAX);
		return;
	}

	/* Function definitions aren't allowed inside subprograms at present, per AmigaBASIC,
		though there's no particularly compelling reason for this restriction. */

	if(Proc()->callNestLevel > SCOPE_MAIN) {
		CauseError(NESTEDSUBS);
		return;
	}

	/* Get type from name: */
	
	name = toks[0];
	outType = UsualTypeConversionToProduce(TypeForName(Proc(), &name));
	if(IsTypeSpecifier(QsGetLast(&name))) {
		QString fullName;
		QsCopy(&fullName, &name);
		QsGetSubstring(&name, &fullName, 0, QsGetLength(&name) - 1);
		QsDispose(&fullName);
	}

	/* Parse optional formal parameter list: */

	scan = &toks[1];
	if(QsGetFirst(scan) == '(') {
		Error paramError = SUCCESS;
		const QString *paramList = scan + 1, *rparen = MatchingRParen(scan);
		
		if(rparen == NULL || rparen == paramList) {
			CauseError(BADSYNTAX);
			return;
		}
		
		if((paramError = CheckNameList(paramList, rparen - paramList, FALSE)) != SUCCESS
		|| (params = ParseNameList(paramList, rparen - paramList, &paramCount, LITERAL)) == NULL) {
			CauseError(paramError == SUCCESS ? NOMEMORY : paramError);
			return;
		}
		
		/*if(paramCount > MAX_FUNCTION_PARAMS) {
			CauseError(LONGLINE);
			return;
		}*/
		
		scan = rparen + 1;
	}
	
	/* Scan optional STATIC: */
	
	if(QsEqNoCase(scan, &g_StaticKeyword)) {
		isStatic = TRUE;
		++scan;
	}
	
	/* Scan optional conditional expression(piece): */

	if(QsEqNoCase(scan, &g_WhereKeyword)) {
		condExpr = scan + 1;
		for(++scan; !QsEqNoCase(scan, &g_AsKeyword) && !IsTerminator(scan); scan++)
			++condExprLength;
		if(condExprLength == 0) {
			CauseError(BADSYNTAX);
			return;
		}
	}

	/* Scan value-generating expression(body): */

	if(!QsEqNoCase(scan, &g_AsKeyword)) {
		CauseError(BADSYNTAX);
		return;
	}
	valueExpr = scan + 1;
	for(++scan; !IsTerminator(scan); scan++)
		++valExprLength;
	if(valExprLength == 0) {
		CauseError(BADSYNTAX);
		return;
	}
		
	/* Create a piece. */

	piece = (struct Piece *)New(sizeof(struct Piece));
	
	piece->next = NULL;	/* Always added at end of list. */

	piece->condition.body.s = CopyTokens(condExpr, condExprLength);
	piece->condition.length = condExprLength;
	piece->value.body.s = CopyTokens(valueExpr, valExprLength);
	piece->value.length = valExprLength;
	
	/* Record the location of the piece for error reporting, profiling, and debug dumping. */
	piece->defStart = Proc()->currentStatementStart;
	piece->defFinish = Proc()->currentPosition - 1;

	piece->compiled = FALSE;
	piece->tailCall = DetermineIfTailCall(&name, &piece->value);

	/* Calculate size of stack required to hold intermediate expression evaluation results. */
	
	workSpaceSize = valExprLength > condExprLength ? valExprLength : condExprLength;

	/* If function is already defined, assume adding a new piece.
	   Parameter count, types, and names(case not significant) must match.
	   Return type must match.
	   Otherwise, add a new function definition in the symtab. */

	definition = LookUp(&name, SCOPE_MAIN);
	if(definition != NULL) {
		struct Piece *p;
		struct Function *fcn;
		bool causeRedefineError;
		
		/* Check that existing definition is a DEFed fcn and that its type matches. */
		
		fcn = definition->value.function;
		
		/*if(definition->category == FUNCTION)
			PrintFunctionInfo(fcn);*/
		
		causeRedefineError = definition->category != FUNCTION
			|| Redefinition(fcn, outType, params, paramCount, piece);
		
		/* Since already know the parameter names and types, can throw them away. */

		if(params != NULL)
			Dispose(params);

		/* Fail if bad redefinition. */

		if(causeRedefineError) {
			CauseError(REDEFINE);
			return;
		}

		/* If all OK, append the piece to the function's list and 
		   increase the required workspace size if this piece contains
		   longer expressions. */

		fcn = definition->value.function;
		if(workSpaceSize > fcn->workSpaceSize)
			fcn->workSpaceSize = workSpaceSize;

		assert(fcn->def != NULL);

		for(p = fcn->def; p != NULL && p->next != NULL; p = p->next)
			;
		p->next = piece;
	}
	else {
		Error error;		
		struct Function *fcn = New(sizeof(struct Function));
		
		fcn->method = NULL;
		fcn->type = outType;
		fcn->def = piece;
		fcn->parameter = params;
		fcn->numArgs = paramCount;
		fcn->workSpaceSize = workSpaceSize;
		fcn->predefinedParameter = isStatic && paramCount != 0 ? New(sizeof(struct Variable *) * paramCount) : NULL;
		if(fcn->predefinedParameter != NULL) {
			short paramIdx;
			for(paramIdx = 0; paramIdx < paramCount; paramIdx++)
				fcn->predefinedParameter[paramIdx] = NULL;
		}
		fcn->staticFunction = isStatic;
			
		if((error = DefineFunction(&name, fcn, SCOPE_GLOBAL)) != SUCCESS)
			CauseError(error);
	}
	
	/*PrintFunctionInfo(fcn);*/
}

#if HT_VISIT_INCLUDES_BIN_PARAM
static bool ClearValue(unsigned binIndex, const QString *key, const void *val, void *param)
#else
static bool ClearValue(const QString *key, const void *val, void *param)
#endif
{
	DisposeScalar(&((struct Variable *)val)->value);
	return TRUE;
}

void ResetStaticFunctionParams(void)
{
	if(Proc()->staticFunctionParams != NULL)
		HtVisit(Proc()->staticFunctionParams, &ClearValue, NULL);
}

/*** Built-in functions ***/

/* Several familiar BASIC functions are implemented in prelude.bas rather than natively.
	Some function implementations are grouped along with relevant statements/commands -
e.g. the I/O related functions, such as LOF, are in io.c along with the I/O statements.
	The remainder of the functions - mostly utility string and mathematical ones -
are implemented in this file. */

static const enum TypeRule m_ArgForArgV[1] = { TR_NUM_TO_INT };		/* ArgV(i) */
static const enum TypeRule m_ArgForAtn[1] = { TR_SINGLE_TO_DOUBLE };	/* Atn(x) */
static const enum TypeRule m_ArgForCDbl[1] = { TR_SINGLE_TO_DOUBLE };	/* CDbl(x) */
static const enum TypeRule m_ArgForCInt[1] = { TR_SINGLE_TO_DOUBLE };	/* CInt(x) */
static const enum TypeRule m_ArgForCLng[1] = { TR_SINGLE_TO_DOUBLE };	/* CLng(x) */
static const enum TypeRule m_ArgForCollision[1] = { TR_NUM_TO_INT };	/* Collision(id) */
static const enum TypeRule m_ArgForCos[1] = { TR_SINGLE_TO_DOUBLE };	/* Cos(x) */
static const enum TypeRule m_ArgForCSng[1] = { TR_SINGLE_TO_DOUBLE };	/* CSng(x) */
static const enum TypeRule m_ArgForCvb[1] = { TR_STRING_ONLY };		/* Cvb(s) */
static const enum TypeRule m_ArgForCvd[1] = { TR_STRING_ONLY };		/* Cvd(s) */
static const enum TypeRule m_ArgForCvi[1] = { TR_STRING_ONLY };		/* Cvi(s) */
static const enum TypeRule m_ArgForCvl[1] = { TR_STRING_ONLY };		/* Cvl(s) */
static const enum TypeRule m_ArgForCvs[1] = { TR_STRING_ONLY };		/* Cvs(s) */
static const enum TypeRule m_ArgForEof[1] = { TR_NUM_TO_INT };		/* Eof(n) */
static const enum TypeRule m_ArgForExp[1] = { TR_SINGLE_TO_DOUBLE };	/* Exp(x) */
static const enum TypeRule m_ArgForFre[1] = { TR_NUM_TO_INT };		/* Fre(n) */
static const enum TypeRule m_ArgsForInStr[3] = { TR_NUM_TO_INT, TR_CHAR_TO_STRING, TR_CHAR_TO_STRING }; /* InStr(i, s, t) */
static const enum TypeRule m_ArgForInt[1] = { TR_SINGLE_TO_DOUBLE };	/* Int(x) */
static const enum TypeRule m_ArgForLen[1] = { TR_CHAR_TO_STRING };	/* Len(s) */
static const enum TypeRule m_ArgForLoc[1] = { TR_NUM_TO_INT };		/* Loc(n) */
static const enum TypeRule m_ArgForLof[1] = { TR_NUM_TO_INT };		/* Lof(n) */
static const enum TypeRule m_ArgForLog[1] = { TR_SINGLE_TO_DOUBLE };	/* Log(x) */
static const enum TypeRule m_ArgForMenuState[1] = { TR_STRING_ONLY };	/* MenuState(name) */
static const enum TypeRule m_ArgsForMid[3] = { TR_CHAR_TO_STRING, TR_NUM_TO_INT, TR_NUM_TO_INT }; /* Mid(s, n, m) */
static const enum TypeRule m_ArgForMkb[1] = { TR_BOOL_ONLY };			/* Mkb(b) */
static const enum TypeRule m_ArgForMkd[1] = { TR_SINGLE_TO_DOUBLE };	/* Mkb(b) */
static const enum TypeRule m_ArgForMki[1] = { TR_NUM_TO_INT };		/* Mki(i) */
static const enum TypeRule m_ArgForMkl[1] = { TR_NUM_TO_LONG };		/* Mkl(i) */
static const enum TypeRule m_ArgForMks[1] = { TR_NUM_TO_SINGLE };		/* Mks(i) */
static const enum TypeRule m_ArgForMouse[1] = { TR_NUM_TO_INT };		/* Mouse(n) */
/*static const enum TypeRule m_ArgForObjectFcns[1] = { TR_NUM_TO_INT }; */ /* ObjectX(id), ObjectY(id), ObjectVX(id), ObjectVY(id) */
static const enum TypeRule m_ArgsForGetObjectProperty[2] = { TR_NUM_TO_INT, TR_NUM_TO_INT };
static const enum TypeRule m_ArgForPeek[1] = { TR_INT_TO_LONG };		/* Peek(addr), PeekL(addr), PeekW(addr) */
static const enum TypeRule m_ArgsForPoint[2] = { TR_NUM_TO_INT, TR_NUM_TO_INT }; /* Point(x, y) */
static const enum TypeRule m_ArgForPX[1] = { TR_NUM_TO_INT };		/* Px~(x), Py~(y) */
static const enum TypeRule m_ArgForRandom[1] = { TR_NUM_TO_LONG };	/* Random(m) */
static const enum TypeRule m_ArgsForFRead[2] = { TR_NUM_TO_INT, TR_NUM_TO_LONG }; /* FRead(n, m) */
static const enum TypeRule m_ArgForSAdd[1] = { TR_STRING_ONLY };		/* SAdd(s) */
static const enum TypeRule m_ArgForSin[1] = { TR_SINGLE_TO_DOUBLE };	/* Sin(x) */
static const enum TypeRule m_ArgForSqr[1] = { TR_SINGLE_TO_DOUBLE };	/* Sqr(x) */
static const enum TypeRule m_ArgForStatus[1] = { TR_NUM_TO_INT };		/* Status(n) */
static const enum TypeRule m_ArgForStr[1] = { TR_EXTEND_NUM };		/* Str(x) */
static const enum TypeRule m_ArgForTan[1] = { TR_SINGLE_TO_DOUBLE };	/* Tan(x) */
static const enum TypeRule m_ArgForVal[1] = { TR_STRING_ONLY };		/* Val(s) */
static const enum TypeRule m_ArgsForWindowInfo[2] = { TR_NUM_TO_INT, TR_NUM_TO_INT }; /* WindowInfo(id, k) */
static const enum TypeRule m_ArgForWPTab[1] = { TR_NUM_TO_INT };	/* WPTab(x) */
static const enum TypeRule m_ArgForYieldScalarValue[1] = { TR_ANY };	/* v~(x) (internal function) */

static const struct Parameter m_ArgsForLBound[2] = {
	{ARRAY, TR_ANY, NULL, NO_NAME, 1, FALSE},
	{LITERAL, TR_SUBSCRIPT, NULL, NO_NAME, 1, FALSE}
};

static const struct Parameter m_ArgForVarPtr[1] = {
	{SCALAR_VAR, TR_ANY, NULL, NO_NAME, 1, FALSE}
};

struct BuiltInFunction {
	const char *name;
	void(*method)(Scalar *result, const BObject *args, unsigned argCount);
	enum TypeRule type;
	const enum TypeRule *argumentTypeReqs;
	short numArgs;
};

static const struct BuiltInFunction m_FuncDefinitions[] = {
	{"ARGC", ArgC_, TR_INT_ONLY, NULL, 0},
	{"ARGV", ArgV_, TR_STRING_ONLY, m_ArgForArgV, 1},
	{"ATN", Atn_, TR_DOUBLE_ONLY, m_ArgForAtn, 1},
	{"CALLBYNAME", CallByName_, TR_ANY, NULL, FN_VAR_ARGS},
	{"COLLISION", Collision_, TR_INT_ONLY, m_ArgForCollision, 1},
	{"COS", Cos_, TR_DOUBLE_ONLY, m_ArgForCos, 1},
	{"CVB", Cvb_, TR_BOOL_ONLY, m_ArgForCvb, 1},
	{"CVD", Cvd_, TR_DOUBLE_ONLY, m_ArgForCvd, 1},
	{"CVI", Cvi_, TR_INT_ONLY, m_ArgForCvi, 1},
	{"CVL", Cvl_, TR_LONG_ONLY, m_ArgForCvl, 1},
	{"CVS", Cvs_, TR_SINGLE_ONLY, m_ArgForCvs, 1},
	{"DATE", Date_, TR_STRING_ONLY, NULL, 0},
	{"EOF", Eof_, TR_BOOL_ONLY, m_ArgForEof, 1},
	{"ERL", Erl_, TR_INT_ONLY, NULL, 0},
	{"ERLAB", ErLab_, TR_STRING_ONLY, NULL, 0},
	{"ERR", Err_, TR_INT_ONLY, NULL, 0},
	{"EXECUTED", Executed_, TR_BOOL_ONLY, NULL, 0},
	{"EXP", Exp_, TR_DOUBLE_ONLY, m_ArgForExp, 1},
	{"FRE", Fre_, TR_LONG_ONLY, m_ArgForFre, 1},
	{"FREAD", FRead_, TR_STRING_ONLY, m_ArgsForFRead, 2},
	{"INKEY", InKey_, TR_STRING_ONLY, NULL, 0},
	{KW_INSTR, InStr_, TR_INT_ONLY, m_ArgsForInStr, 3},
	{"LBOUND", LBound_, TR_INT_ONLY, NULL, 2}, /* Args set when defined. */
	{KW_LEN, Len_, TR_INT_ONLY, m_ArgForLen, 1},
	{"LOC", Loc_, TR_LONG_ONLY, m_ArgForLoc, 1},
	{"LOF", Lof_, TR_LONG_ONLY, m_ArgForLof, 1},
	{"LOG", Log_, TR_DOUBLE_ONLY, m_ArgForLog, 1},
	{"MENUPICKED", MenuPicked_, TR_STRING_ONLY, NULL, 0},
	{"MENUSTATE", MenuState_, TR_STRING_ONLY, m_ArgForMenuState, 1},
	{KW_MID, Mid_, TR_STRING_ONLY, m_ArgsForMid, 3},
	{"MKB", Mkb_, TR_STRING_ONLY, m_ArgForMkb, 1},
	{"MKD", Mkd_, TR_STRING_ONLY, m_ArgForMkd, 1},
	{"MKI", Mki_, TR_STRING_ONLY, m_ArgForMki, 1},
	{"MKL", Mkl_, TR_STRING_ONLY, m_ArgForMkl, 1},
	{"MKS", Mks_, TR_STRING_ONLY, m_ArgForMks, 1},
	{"MOUSE", Mouse_, TR_INT_ONLY, m_ArgForMouse, 1},
	{"GETOBJECTPROPERTY", GetObjectProperty_, TR_INT_ONLY, m_ArgsForGetObjectProperty, 2},
	/*{"OBJECTVX", ObjectVX_, TR_INT_ONLY, m_ArgForObjectFcns, 1},
	{"OBJECTVY", ObjectVY_, TR_INT_ONLY, m_ArgForObjectFcns, 1},
	{"OBJECTX", ObjectX_, TR_INT_ONLY, m_ArgForObjectFcns, 1},
	{"OBJECTY", ObjectY_, TR_INT_ONLY, m_ArgForObjectFcns, 1},*/
	{"PEEK", Peek_, TR_INT_ONLY, m_ArgForPeek, 1},
	{"PEEKL", PeekL_, TR_LONG_ONLY, m_ArgForPeek, 1},
	{"PEEKW", PeekW_, TR_INT_ONLY, m_ArgForPeek, 1},
	{"POINT", Point_, TR_INT_ONLY, m_ArgsForPoint, 2},
	{KW_PX, PX_, TR_INT_ONLY, m_ArgForPX, 1},
	{KW_PY, PY_, TR_INT_ONLY, m_ArgForPX, 1},
	{"RANDOM", Random_, TR_INTEGRAL, m_ArgForRandom, 1},
	{"RND", Rnd_, TR_SINGLE_ONLY, NULL, 0},
	{"SADD", SAdd_, TR_LONG_ONLY, m_ArgForSAdd, 1},
	{"SIN", Sin_, TR_DOUBLE_ONLY, m_ArgForSin, 1},
	{"SQR", Sqr_, TR_DOUBLE_ONLY, m_ArgForSqr, 1},
	{"STATUS", Status_, TR_INTEGRAL, m_ArgForStatus, 1},
	{"STR", Str_, TR_STRING_ONLY, m_ArgForStr, 1},
	{"TAN", Tan_, TR_DOUBLE_ONLY, m_ArgForTan, 1},
	{"TIME", Time_, TR_STRING_ONLY, NULL, 0},
	{"TIMER", Timer_, TR_SINGLE_ONLY, NULL, 0},
	{"UBOUND", UBound_, TR_INT_ONLY, NULL, 2}, /* Args set when defined. */
	{"VAL", Val_, TR_NUMERIC, m_ArgForVal, 1},
	{"VARPTR", VarPtr_, TR_LONG_ONLY, NULL, 1}, /* Arg set when defined. */
	{"WCSRLIN", WCsrLin_, TR_INT_ONLY, NULL, 0},
	{"WINDOWINFO", WindowInfo_, TR_LONG_ONLY, m_ArgsForWindowInfo, 2},
	{"WPOS", WPos_, TR_INT_ONLY, NULL, 0},
	{"WPTAB", WPTab_, TR_STRING_ONLY, m_ArgForWPTab, 1},
	{"v~", YieldScalarValue_, TR_ANY, m_ArgForYieldScalarValue, 1}
};

#define LAST_FUNCTION YieldScalarValue_

static void DefineBuiltInFunction(const struct BuiltInFunction *fcn)
{
	/* TODO support defaults for function parameters - would be useful for MID etc. */
	static const struct Parameter ordinaryFunctionParam = { LITERAL, TR_ANY, NULL, NO_NAME, 1, FALSE };

	struct Parameter *param = NULL;

	if(fcn->numArgs > 0) {
		param = New(sizeof(struct Parameter) * fcn->numArgs);
		if(fcn->method == LBound_ || fcn->method == UBound_) {
			assert(fcn->numArgs == 2);
			param[0] = m_ArgsForLBound[0], param[1] = m_ArgsForLBound[1];
		}
		else if(fcn->method == VarPtr_) {
			assert(fcn->numArgs == 1);
			param[0] = m_ArgForVarPtr[0];
		}
		else {
			short i;

			/* Guard against inconsistency in parameter count, and definitions of formals - */
			assert((fcn->numArgs <= 0) == (fcn->argumentTypeReqs == NULL));

			for(i = 0; i < fcn->numArgs; i++) {
				param[i] = ordinaryFunctionParam;
				param[i].type = fcn->argumentTypeReqs[i];
			}
		}
	}

	{
		QString name;
		struct Function *newFunc = New(sizeof(struct Function));

		QsInitStaticNTS(&name, fcn->name);

		newFunc->method = fcn->method;
		newFunc->type = fcn->type;
		newFunc->def = NULL;
		newFunc->parameter = param;
		newFunc->numArgs = fcn->numArgs;
		newFunc->workSpaceSize = 0;
		newFunc->predefinedParameter = NULL;
		newFunc->staticFunction = FALSE;

		RequireSuccess(DefineFunction(&name, newFunc, SCOPE_BUILTIN));
	}
}

void DefineBuiltInFunctions(void)
{
	const struct BuiltInFunction *fcn = &m_FuncDefinitions[0];
	do {
		DefineBuiltInFunction(fcn);
	} while((fcn++)->method != LAST_FUNCTION);
}

bool AttemptToDefineBuiltInFunction(const QString *name)
{
	const struct BuiltInFunction *fcn = &m_FuncDefinitions[0];
	do {
		QString fcnName;
		QsInitStaticNTS(&fcnName, fcn->name);
		if(QsEqNoCase(&fcnName, name)) {
			DefineBuiltInFunction(fcn);
			return TRUE;
		}
	} while((fcn++)->method != LAST_FUNCTION);
	return FALSE;
}

void YieldScalarValue_(Scalar *result, const BObject *arg, unsigned count)
{
	assert(arg->category == LITERAL);
	CopyScalar(result, &arg->value.scalar);
}

void CallByName_(Scalar *result, const BObject *arg, unsigned count)
{
	BObject *defn;
	Scalar name;
	BObject resultObj;
	struct Stack workingStack;
	
	/* Parameter count checking for the referenced function is deferred
		until it's actually evaluated - just check that there's a function
		name there. */

	if(count == 0) {
		SetError(result, BADARGCOUNT);
		return;
	}

	if(arg[0].category != LITERAL) {
		SetError(result, BADARGTYPE);
		return;
	}

	CopyScalar(&name, &arg[0].value.scalar);
	ChangeType(&name, TR_CHAR_TO_STRING);

	if(ScalarIsError(&name)) {
		SetError(result, BADARGTYPE);
		return;
	}

	defn = LookUpCheckingType(&name.value.string, Proc()->callNestLevel);
	if(defn == NULL || defn->category != FUNCTION) {
		SetError(result, UNDEFINEDVARORFUNC);
		SetAdditionalErrorMessage("Not found: %.*s", QsGetData(&name.value.string), QsGetLength(&name.value.string));
		DisposeScalar(&name);
		return;
	}
	DisposeScalar(&name);
	
	/* Sneakily convert the parameters in-place - hence a nasty const-removing cast - */
	if(count > 1) {
		Error error = ConformForApplication(defn, (BObject *)arg + 1, count - 1);

		if(error != SUCCESS) {
			SetError(result, error);
			return;
		}
	}

	CreateExprStk(&workingStack, IsDefFunction(defn->value.function) ? defn->value.function->workSpaceSize : 1);

	CallFunction(&resultObj, defn->value.function, count == 1 ? NULL : (BObject *)arg + 1, count - 1, &workingStack);
	
	DisposeExprStk(&workingStack);
	
	if(resultObj.category != LITERAL)
		SetError(result, SCALAREXPECTED);
	else
		CopyScalar(result, &resultObj.value.scalar);

	DisposeIfScalar(&resultObj);
}

/* Doesn't include program name, unlike C's argc. */
static unsigned CLArgCount(void)
{
	return Opts()->argCount;
}

void ArgC_(Scalar *result, const BObject *arg, unsigned count)
{
	SetFromLong(result, (long)CLArgCount(), T_INT);
}

/* Note that n is 1-based(for parameters!) 0 gets the program name. */
static const char *CLArgument(unsigned n)
{
	assert(n <= CLArgCount());
	return n == 0 ? Opts()->fileName : Opts()->argument[n - 1];
}

void ArgV_(Scalar *result, const BObject *arg, unsigned count)
{
	short n = arg[0].value.scalar.value.number.s;
	
	if(n < 0 || n > (short)CLArgCount())
		SetError(result, OUTSIDEDOMAIN);
	else {
		const char *clArgument = CLArgument((unsigned)n);
		InitScalarAsString(result);
		QsCopyNTS(&result->value.string, clArgument);
	}
}

/* Previously this used to clear the statement cache as a side effect,
	but this is not safe, since the token sequence being executed
	has usually been fetched from the cache, and (for speed) it isn't
	copied, but merely a pointer to the cached structure. */ 
void Fre_(Scalar *result, const BObject *arg, unsigned count)
{
	long amount;
	short infoWanted = arg[0].value.scalar.value.number.s;
	
	if(infoWanted == -1) {
		size_t avail = PfAvailMem();
		amount = avail > LONG_MAX ? LONG_MAX : (long)avail;
	}
	else if(infoWanted == -2)
		amount = StackSpaceNeverUsed();
	else if(infoWanted == -3)
		amount = GetFreeFileBufferSpace(Proc()->buffer);
	else {
		size_t avail = HeapMemAvail();
		amount = avail > LONG_MAX ? LONG_MAX : (long)avail;
	}

	SetFromLong(result, amount, T_LONG);
}

void InStr_(Scalar *result, const BObject *arg, unsigned count)
{
	int pos = arg[0].value.scalar.value.number.s;
	const QString *searchIn = &arg[1].value.scalar.value.string;
	const QString *lookFor = &arg[2].value.scalar.value.string;
	int sLen = QsGetLength(searchIn), tLen = QsGetLength(lookFor), foundAt;
	
	/* Allow a starting search position of 0, which is treated as if it were 1. */
	if(pos < 0)
		SetError(result, BADSUBSTRING);
    else {
		foundAt = sLen == 0 || tLen == 0
			? sLen == 0 && tLen == 0 && pos <= 1
			: 1 + QsSearch(searchIn, lookFor, pos == 0 ? 0 : pos - 1);
			/* QString uses 0-based positions rather than BASIC's 1-based ones. */
		SetFromLong(result, foundAt, T_INT);
	}
}

void Len_(Scalar *result, const BObject *arg, unsigned count)
{
	SetFromLong(result, QsGetLength(&arg[0].value.scalar.value.string), T_INT);
}

void Mid_(Scalar *result, const BObject *arg, unsigned count)
{
	const QString *source = &arg[0].value.scalar.value.string;
	int startPos = arg[1].value.scalar.value.number.s;
	int nChars = arg[2].value.scalar.value.number.s;

	/* As for INSTR - start position is traditionally 1-relative in BASIC, but accept 0 too - */
	if(startPos < 0 || nChars < 0)
		SetError(result, BADSUBSTRING);
	else {
		InitScalarAsString(result);
		if(startPos <= (int)QsGetLength(source))
			QsGetSubstring(&result->value.string, source, startPos == 0 ? 0 : startPos - 1, nChars);
	}
}

void SAdd_(Scalar *result, const BObject *arg, unsigned count)
{
	if(!Opts()->unsafe)
		SetError(result, ER_UNSAFE);
	else
		SetFromLong(result, (long)QsGetData(&arg[0].value.scalar.value.string), T_LONG);
}

void Str_(Scalar *result, const BObject *arg, unsigned count)
{
	char convBuffer[64];
	NumberToCString(&arg[0].value.scalar.value.number, arg[0].value.scalar.type, &convBuffer[0], TRUE);
	InitScalarAsString(result);
	QsCopyNTS(&result->value.string, convBuffer);
}

void Val_(Scalar *result, const BObject *arg, unsigned count)
{
	ParseNumber(&arg[0].value.scalar.value.string, result);
}

static char DigitToChr(short d)
{
	assert(d >= 0 && d <= 9);
	if(d == 1) return '1';
	if(d == 2) return '2';
	if(d == 3) return '3';
	if(d == 4) return '4';
	if(d == 5) return '5';
	if(d == 6) return '6';
	if(d == 7) return '7';
	if(d == 8) return '8';
	if(d == 9) return '9';
	return '0';
}

void Date_(Scalar *result, const BObject *arg, unsigned count)
{
	short year, month, day;
	struct tm *currentTime;
	struct PfSystemTimeStamp stamp;
	time_t timer;
	QString *s = &result->value.string;

	PfGetSystemTimeStamp(&stamp);
	timer = PfConvertToTimeTTickCount(&stamp);
	currentTime = localtime(&timer);
	year = (short)currentTime->tm_year + 1900;
#ifdef AMIGA /* TODO this is weird */
	year += 8;
#endif
	month = (short)currentTime->tm_mon + 1;
	day = (short)currentTime->tm_mday;

	InitScalarAsString(result);
	/* TODO really should use sprintf or strftime */
	QsRepeat(s, 10, ' ');
	QsSetCharAt(s, 0, DigitToChr(month / 10));
	QsSetCharAt(s, 1, DigitToChr(month % 10));
	QsSetCharAt(s, 2, '-');
	QsSetCharAt(s, 3, DigitToChr(day / 10));
	QsSetCharAt(s, 4, DigitToChr(day % 10));
	QsSetCharAt(s, 5, '-');
	QsSetCharAt(s, 6, DigitToChr(year / 1000));
	QsSetCharAt(s, 7, DigitToChr((year % 1000) / 100));
	QsSetCharAt(s, 8, DigitToChr((year % 100) / 10));
	QsSetCharAt(s, 9, DigitToChr(year % 10));
}

/* The number of seconds which have elapsed since midnight. */
static double GetClock(void)
{
	struct tm *currentTime;
	struct PfSystemTimeStamp stamp;
	time_t timer;
	
	PfGetSystemTimeStamp(&stamp);
	timer = PfConvertToTimeTTickCount(&stamp);
	currentTime = localtime(&timer);
	return 3600.0 * currentTime->tm_hour + 60.0 * currentTime->tm_min + currentTime->tm_sec;
}

void Time_(Scalar *result, const BObject *arg, unsigned count)
{
	long clockSeconds = (long)GetClock();
	short hours = (short)(clockSeconds / 3600);
	short remainder = (short)(clockSeconds % 3600);
	short minutes = remainder / 60;
	short seconds = remainder % 60;	
	QString *s = &result->value.string;
	
	InitScalarAsString(result);
	/* TODO really should use sprintf or strftime */
	QsRepeat(s, 8, ' ');
	QsSetCharAt(s, 0,  DigitToChr(hours / 10));
	QsSetCharAt(s, 1,  DigitToChr(hours % 10));
	QsSetCharAt(s, 2,  ':');
	QsSetCharAt(s, 3,  DigitToChr(minutes / 10));
	QsSetCharAt(s, 4,  DigitToChr(minutes % 10));
	QsSetCharAt(s, 5,  ':');
	QsSetCharAt(s, 6,  DigitToChr(seconds / 10));
	QsSetCharAt(s, 7,  DigitToChr(seconds % 10));
}

void Timer_(Scalar *result, const BObject *arg, unsigned count)
{
	SetFromDouble(result, GetClock(), T_SINGLE);
}

static void EvalUnaryMathFunction(
	Scalar *result,
	const BObject *arg,
	double(__cdecl *f)(double),
	bool(*inDomain)(double))
{
	double x = arg[0].value.scalar.value.number.d;
	if(inDomain == NULL || inDomain(x)) {
		SetFromDouble(result, f(x), T_DOUBLE);
		if(errno == ERANGE)
			SetError(result, OVERFLOWERR);
	}
	else
		SetError(result, OUTSIDEDOMAIN);
}

void Sin_(Scalar *result, const BObject *arg, unsigned count)
{
	EvalUnaryMathFunction(result, arg, sin, NULL);
}

void Cos_(Scalar *result, const BObject *arg, unsigned count)
{
	EvalUnaryMathFunction(result, arg, cos, NULL);
}

void Tan_(Scalar *result, const BObject *arg, unsigned count)
{
	EvalUnaryMathFunction(result, arg, tan, NULL);
}

void Atn_(Scalar *result, const BObject *arg, unsigned count)
{
	EvalUnaryMathFunction(result, arg, atan, NULL);
}

void Exp_(Scalar *result, const BObject *arg, unsigned count)
{
	EvalUnaryMathFunction(result, arg, exp, NULL);
}

static bool InLogDomain(double x) { return x > 0; }

void Log_(Scalar *result, const BObject *arg, unsigned count)
{
	EvalUnaryMathFunction(result, arg, log, InLogDomain);
}

static bool InSqrDomain(double x) { return x >= 0; }

void Sqr_(Scalar *result, const BObject *arg, unsigned count)
{
	EvalUnaryMathFunction(result, arg, sqrt, InSqrDomain);
}

void Randomize_(BObject *arg, unsigned count)
{
	long seed = arg[0].value.scalar.value.number.l;
	
	if(seed == 0) {
		QString line;
		int result;
		
		QsInitNull(&line);
		
		do {
			Error error;
			
			puts("Enter random seed value: ");
			fflush(stdout);
			QsDispose(&line);
			error = ReadLine(stdin, &line);
			if(error != SUCCESS) {
				QsDispose(&line);
				CauseError(error);
				return;
			}
		}
		while((result = sscanf(QsGetData(&line), "%ld", &seed)) != 1);
		
		QsDispose(&line);
	}
	
	srand((unsigned)seed);
}

void Rnd_(Scalar *result, const BObject *arg, unsigned count)
{
	double r = (double)rand() / RAND_MAX; /* This must be evaluated in double precision to avoid a VBCC bug. */
	SetFromDouble(result, r, T_SINGLE);
}

void Random_(Scalar *result, const BObject *arg, unsigned count)
{
	long n = arg[0].value.scalar.value.number.l;
	if(n > 0)
		SetFromLong(result, rand() % n + 1, T_LONG);
	else
		SetError(result, OUTSIDEDOMAIN);
}
