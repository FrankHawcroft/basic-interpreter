/****** pit.c ******/

/*
	$VER: pit.c 0.16A (5.20.2015)

	Performance Improving Transformations; 'optimisation'.
*/

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "interpreter.h"
#include "builtin.h"
#include "process.h"
#include "heap.h"
#include "stack.h"
#include "hashtable.h"
#include "options.h"

bool EligibleForCaching(const struct TokenSequence *tokSeq, short callNestLevelWhenExecuted)
{
	return !Opts()->lowMemory 
		/* Labels and line numbers are defined at compile time rather than execution time,
			so labelled statements cannot be cached in subprograms. */
		&& (callNestLevelWhenExecuted == SCOPE_MAIN || QsIsNull(&tokSeq->label))
		&& tokSeq->next != NULL /* Not a partial 'statement', e.g. in a single-line IF. */
		&& InPotentiallyHotPath(); /* Assumes considering caching currently executing stmt. */
}

static bool GuaranteedGlobal(const QString *t) { return IsLiteral(t) || LexicallyGuaranteedBuiltIn(t); }

/* Whether to cache the converted form of a statement, rather than just tokens. */
static bool ShouldCachePreconvertedObjects(const struct TokenSequence *tokSeq, short callNestLevelWhenExecuted)
{	
	unsigned short n;
	bool allImmutablyExist;
	
	if(tokSeq->preconverted != NULL)
		return FALSE; /* already done ... */
		
	/* Quick execution assumes an 'ordinary' statement; and no point preconverting if no params. */
	if(IsMacro(tokSeq->command) || tokSeq->length <= 1)
		return FALSE;
	
	/* For short-running programs, the overhead of converting objects again before caching them, generally
		outweighs the performance gain. Static SUBs are considered an exception as they tend to be 'leaf'
		calls which presumably will happen frequently enough to justify pre-conversion. */
	if(callNestLevelWhenExecuted == SCOPE_STATIC) {
		/*for(n = 0, allImmutablyExist = TRUE; n < tokSeq->length && allImmutablyExist; n++)
			allImmutablyExist &= (GuaranteedNotDynamic(&tokSeq->rest[n])
				|| LookUp(&tokSeq->rest[n], callNestLevelWhenExecuted) != NULL);
		return allImmutablyExist;*/
		return TRUE;
	}

	/* Otherwise, unless -o specified, don't bother. */
	if(!Opts()->optimise)
		return FALSE;

	/* Outside a SUB, everything can be assumed to stick around. */
	if(callNestLevelWhenExecuted == SCOPE_MAIN)
		return TRUE;

	/* If in a non-STATIC SUB, it's only safe to cache objects if none of them are local labels or variables -
		erring on the side of caution, cache objects only if everything is a constant, or an operation on a
		constant. */	
	for(n = 0, allImmutablyExist = TRUE; n < tokSeq->length && allImmutablyExist; n++)
		allImmutablyExist &= GuaranteedGlobal(&tokSeq->rest[n]);
	
	return allImmutablyExist;
}

void StorePreconvertedObjects(struct TokenSequence *ts, short callNestLevelWhenExecuted)
{
	if(ShouldCachePreconvertedObjects(ts, callNestLevelWhenExecuted)
	&& (ts->preconverted = TolerantNew(sizeof(BObject) * ts->length)) != NULL) {
		unsigned short n;
		bool allResolved = TRUE;
		for(n = 0; n < ts->length && allResolved; n++) {
			ConvertToObject(&ts->rest[n], &ts->preconverted[n], callNestLevelWhenExecuted);
			allResolved &= !IsEmpty(&ts->preconverted[n]);
		}
		if(!allResolved) {
			unsigned short m;
			for(m = 0; m < n; m++)
				DisposeIfScalar(&ts->preconverted[m]);
			Dispose(ts->preconverted);
			ts->preconverted = NULL;
		}
	}
}

bool NoDynamicallyAllocatedMemory(const struct TokenSequence *ts)
{
	unsigned short n;
	bool noMalloc = TRUE;

	for(n = 0; n < ts->length && noMalloc; n++)
		noMalloc &= (IsPunctuation(&ts->rest[n]) || TypeIsNumeric(TypeOfToken(&ts->rest[n])));
	
	return noMalloc;
}

/* Covers CONST as well because it's just Let_ with extra parameter checking in its converter. */
static bool IsAssignmentStatement(const struct Statement *command)
{
	return !IsSubprogram(command) && !IsMacro(command) && command->method.builtIn == Let_;
}

const BObject *AssignmentTarget(const struct TokenSequence *ts, short callNestLevel)
{
	const BObject *vdef = NULL;
	if(IsAssignmentStatement(ts->command)) {
		const QString *v = &ts->rest[QsGetFirst(&ts->rest[0]) == '(' ? 1 : 0];
		vdef = LookUp(v, callNestLevel);
	}
	return vdef != NULL && IsVariable(vdef) ? vdef : NULL;
}

void ImproveIfAssignmentStatement(struct TokenSequence *ts, const BObject *vdef, short callNestLevelWhenExecuted)
{
	if(vdef == NULL)
		return;
		
	if((callNestLevelWhenExecuted == SCOPE_MAIN || callNestLevelWhenExecuted == SCOPE_STATIC)
	|| (vdef->category & (VARIABLE_IS_SHARED | VARIABLE_IS_ARRAY | VARIABLE_IS_REF))) {
		/* Either variable sticks around, or, if in a dynamic sub, assume it'll always be created
			by DIM or SHARED or as a reference parameter, before being assigned to. */
		QString letqPredef;
		QsInitStaticNTS(&letqPredef, KW_LETQ_PREDEF);
		RequireSuccess(GetStatement(&letqPredef, &ts->command));
	}
	else {
		/* Local scalar - not quite as quick, but can avoid full lookup, and type checks. */
		QString letqLocal;
		QsInitStaticNTS(&letqLocal, KW_LETQ_LOCAL);
		RequireSuccess(GetStatement(&letqLocal, &ts->command));
	}
}

static bool SuitableForInlining(const Scalar *s)
{
	return (TypeIsNumeric(s->type) && TypeIsExact(s->type))
		|| (TypeIsTextual(s->type)
			&& (s->type == T_CHAR
				|| QsGetLength((const QString *)GetPointer((Scalar *)s)) <= 16));
}

static const BObject *ResolveGlobalNamedConstant(const QString *token)
{
	/* Look up at the current CNL so that if a local hides a global
	defn, the local one is retrieved; in this case, don't return it,
	as obviously the value can't be relied on to stay unchanged between
	subprogram calls. */
	const BObject *defn = IsName(token) ? LookUpCheckingType(token, Proc()->callNestLevel) : NULL;
	return defn != NULL
		&& defn->category == NAMED_CONST
		&& LookUpCheckingType(token, SCOPE_MAIN) == defn
			? defn : NULL;
}

static bool IsConstant(const QString *tok)
{
	return IsLiteral(tok) || ResolveGlobalNamedConstant(tok) != NULL;
}

static bool IsFunctionOrOperator(const QString *token)
{
	const BObject *defn = LookUpCheckingType(token, Proc()->callNestLevel);
	return defn != NULL && (defn->category == FUNCTION || defn->category == OPERATOR);
}

static bool BuiltInFunctionIsDeterministic(void (*fm)(Scalar *, const BObject *, unsigned))
{
	return fm == YieldScalarValue_
		|| fm == Atn_
		|| fm == Cos_
		|| fm == Cvb_
		|| fm == Cvd_
		|| fm == Cvi_
		|| fm == Cvl_
		|| fm == Cvs_
		|| fm == Exp_
		|| fm == InStr_
		|| fm == Len_
		|| fm == Log_
		|| fm == Mid_
		|| fm == Mkb_
		|| fm == Mkd_
		|| fm == Mki_
		|| fm == Mkl_
		|| fm == Mks_
		|| fm == Sin_
		|| fm == Sqr_
		|| fm == Str_
		|| fm == Tan_
		|| fm == Val_;
}

#define MAX_FUNCTION_DETERMINISM_CHECK_RECURSION 5

static bool IsDeterministic(const QString *t, bool inFunction, unsigned depth, struct HashTable *fcns)
{
	static const bool exists = TRUE;
	
	const BObject *defn;
	
	if(depth > MAX_FUNCTION_DETERMINISM_CHECK_RECURSION)
		return FALSE;
	
	if(HtLookUp(fcns, t) != NULL || IsConstant(t))
		return TRUE;
	
	defn = LookUpCheckingType(t, Proc()->callNestLevel);
		
	if(defn == NULL) /* Assume function compilation (and therefore optimisation) happens at first call.
						Therefore, any referenced global vars are assumed to exist at this point - else
						an error will occur on evaluation, so no harm in classifying the function incorrectly. */
		return inFunction;
	else if(IsVariable(defn)
	  && (IsArray(defn)
		|| LookUp(t, SCOPE_MAIN) == defn
		|| LookUp(t, Proc()->callNestLevel - inFunction) == defn))
				/*GetActualCallNestLevel(t, defn) <= Proc()->callNestLevel - inFunction)) */
		return FALSE; /* Refers to a global or subprogram variable. IsConstant covers global constants. */
	/*else if(defn->category == OPERATOR)
		return TRUE;*/
	else if(defn->category == FUNCTION) {
		const struct Function *f = defn->value.function;
		if(IsDefFunction(f)) {
			const struct Piece *piece;
			
			if(f->staticFunction)
				return TRUE; /* TODO figure it out ... */
			
			HtAdd(fcns, t, (void *)&exists); /* cast away const */
			
			for(piece = f->def; piece != NULL; piece = piece->next) {
				const QString *scan;
				for(scan = piece->condition.body.s; scan != NULL && scan < piece->condition.body.s + piece->condition.length; scan++)
					if(HtLookUp(fcns, scan) == NULL && !IsDeterministic(scan, TRUE, depth + 1, fcns))
						return FALSE;
				for(scan = piece->value.body.s; scan != NULL && scan < piece->value.body.s + piece->value.length; scan++)
					if(HtLookUp(fcns, scan) == NULL && !IsDeterministic(scan, TRUE, depth + 1, fcns))
						return FALSE;
			}
			
			HtDelete(fcns, t);
		}
		else
			return BuiltInFunctionIsDeterministic(f->method);		
	}
	
	return TRUE; /* Assume operator, punctuation, or label. */
}

/* Assumes in prefix form and already determined to be syntactically and semantically valid -
	i.e. there won't be a contextual type where the token is the first one. */
static SimpleType WithLeftContext(enum TypeRule required, const QString *token)
{
	SimpleType leftContext = T_MISSING;
	
	if(Contextual(required)) {
		BObject left;
		
		ConvertToObject(token - 1, &left, Proc()->callNestLevel); /* TODO too simplistic - handle array indexing and function calls */
		
		if(GetSimpleType(&left) != T_EMPTY)
			leftContext = GetSimpleType(&left);
		
		DisposeIfScalar(&left);
	}
	
	return leftContext;
}

static void MakeQuotedToken(const Scalar *v, QString *t)
{
	QsInitNull(t);

	if(v->type == T_STRING) {
		QsCopyChar(t, '\"');
		QsAppend(t, (const QString *)GetPointer((Scalar *)v));
		QsAppendChar(t, '\"');
	}
	else {
		QsChar convBuffer[64];

		convBuffer[0] = NUL;

		if(v->type == T_CHAR) {
			/* TODO also, there's the quote problem */
			if(GetCharacter(v) != 0)
				sprintf(convBuffer, "\"%c\"", GetCharacter(v));
			else
				strcpy(convBuffer, "\"\"");
		}
		else if(v->type == T_BOOL)
			sprintf(convBuffer, "%d", (int)GetBoolean(v));
		else if(v->type == T_INT) {
			long n = GetLong(v);
			if(0 <= n && n <= 9)
				sprintf(convBuffer, "%ld", n);
			else
				sprintf(convBuffer, "&h%hX", (short)n);
		}
		else if(v->type == T_LONG)
			sprintf(convBuffer, "&h%lX&", GetLong(v));
		else if(v->type == T_SINGLE || v->type == T_DOUBLE)
			sprintf(convBuffer, "%f%c", GetDouble(v), SpecifierFromType(v->type));

		QsCopyNTS(t, convBuffer);
	}
}
/*
#define MAX_TRACKED_NESTING 10

static const BObject *FunctorFor(const QString *ts, unsigned short nToks, const QString *t)
{
	const BObject *functor[MAX_TRACKED_NESTING];
	unsigned short paramNumber;
	int nesting;
	
	unsigned short rt;
				int relNesting = 0, fcount = 0;
				const struct Parameter *formal = NULL;
				
				actual = 0;
				for(rt = t - 1; rt >= 1 && relNesting >= 0; rt--) {
					QString *rtok = &tokSeq->rest[rt];
					Nest(rtok, &relNesting);
					actual += relNesting == 0 && (QsGetFirst(rtok) == '(' || !IsPunctuation(rtok));
					if(relNesting == 0 && IsLParen(rtok - 1)) {
						const struct Operator *op = ResolveOperator(rtok);
						if(op != NULL) {
							formal = ParametersForOperator(op);
							fcount = OperandCount(op);
						}
					}
				}
}
*/

/* Substitution of literals for named constants
   ============================================

Where a globally defined named constant is used, if it is a short string,
character, or integer constant, its value will be 'inlined', because
using such constants is quicker than doing a symtab lookup.
Large strings are not 'inlined', and nor are floating-point values -
because they are slower to convert from a string, and because conversion is
not necessarily stable (i.e. values can change when printed then parsed). */
static bool InlineNamedConstant(QString *tok)
{
	const BObject *defn = ResolveGlobalNamedConstant(tok);

	if(defn != NULL	&& SuitableForInlining(VarData(defn))) {
		QsDispose(tok);
		MakeQuotedToken(VarData(defn), tok);
		return TRUE;
	}

	return FALSE;
}

/* Substitution of named constants for literals
   ============================================
   
The inverse of substituting a literal token for a named constant; only
done for floating-point literals within subprograms and functions, because
literals at global scope will be pre-converted. */
static bool CreateNamedConstant(QString *tok, bool inFunction)
{
	static int constNumber = 0;
	bool created = FALSE;
	
	if(IsNumeric(tok) && Proc()->callNestLevel > SCOPE_MAIN && !InStaticContext(Proc())) {
		Scalar n;
		
		ParseToken(tok, &n);
		if(!TypeIsExact(n.type)) {
			BObject *c; /* vvv */
			QString name;
			char nameBuffer[64];
			
			sprintf(nameBuffer, "fc%d~", constNumber++);
			QsCopyNTS(&name, nameBuffer);
			
			if((c = DefineVariable(&name, n.type, SCOPE_GLOBAL, FALSE)) != NULL) {
				c->category = NAMED_CONST; /* vvv */
				CopyScalar(VarData(c), &n);
				
				QsCopy(tok, &name);
				
				created = TRUE;
			}
			
			QsDispose(&name);
		}
	}
	
	return created;
}

/* Removal of unnecessary function calls
   =====================================

The scalar value generating function (v~) is inserted for parenthesised expressions.
In many cases, this can be removed: if its parameter is a literal, an operator, or
another function invocation, then the result is already guaranteed not to be a reference. */
	
static const QString m_Removed = {"~x~", 3};

static bool RemoveRedundantFunction(QString *tok)
{
	if(IsLParen(tok)
	&& QsEqNoCase(tok + 1, &g_ScalarYieldingFunction)
	&& (IsLiteral(tok + 2)
	  || (IsLParen(tok + 2) && IsFunctionOrOperator(tok + 3)))) {
		int nesting = 1;
		QString *scan;

		QsDispose(tok);
		QsCopy(tok, &m_Removed);
		QsDispose(tok + 1);
		QsCopy(tok + 1, &m_Removed);

		for(scan = tok + 2; nesting > 1 || !IsRParen(scan); scan++)
			Nest(scan, &nesting);
		QsDispose(scan);
		QsCopy(scan, &m_Removed);

		return TRUE;
	}

	return FALSE;
}

#define MAX_CONSTANT_EXPR_LENGTH 12 /* e.g. ( f 1 2 3 4 5 6 7 8 ) | */

/* Evaluation of operators & functions with constant arguments
   ===========================================================

Since all operators in BASIC are deterministic, if an operator is applied to
constant operands, it can be evaluated once and its value substituted.
Some functions are also deterministic. */
static bool EvaluateConstantValuedExpression(QString *tok, bool inFunction)
{
	int nParams = 0;
	
	if(!IsLParen(tok))
		return FALSE;
	
	{
		QString *scan;
		
		for(scan = tok + 2; IsConstant(scan); scan++)
			++nParams;
	
		if(!IsRParen(scan) || nParams > MAX_CONSTANT_EXPR_LENGTH - 4) /* ( f [...] ) | */
			return FALSE;
	}
	
	{
		QString expr[MAX_CONSTANT_EXPR_LENGTH];
		int i;
		struct Stack stk;
		struct HashTable *fcns = HtCreate(11, NULL, &QsEqNoCase);
		bool evalIt = IsDeterministic(tok + 1, inFunction, 0, fcns);
	
		HtDispose(fcns);
		
		if(!evalIt)
			return FALSE;

		/* Ensure the expression terminates by adding '|'. */
		
		for(i = 0; i < nParams + 3; i++)
			QsCopy(&expr[i], tok + i);
		QsCopy(&expr[i], &g_Pipe);

		/* Evaluate it. */

		CreateExprStk(&stk, nParams);
		Eval(expr, DefaultConvert, 0, &stk);

		assert(StkHeight(&stk) == 1);
		assert(PeekExprStk(&stk, 0)->category == LITERAL);

		for(i = 0; i < nParams + 3; i++)
			QsDispose(&expr[i]);

		/* Substitute the value, and mark remaining tokens as unused. */
		/* TODO if f.p., shouldn't do this ... or at least, ensure more sig digits in MQT! */
		QsDispose(tok);
		MakeQuotedToken(&(PeekExprStk(&stk, 0)->value.scalar), tok);
		
		/* Clean up. */
		
		DisposeExprStk(&stk);
		for(i = 1; i < nParams + 3; i++) {
			QsDispose(tok + i);
			QsCopy(tok + i, &m_Removed);
		}

		return TRUE;
	}
}

/* Conversion of literal actual parameters
   =======================================

Where an actual parameter is a literal, it can be pre-converted according to the rules
for the parameter. The aim is to avoid type checking and conversion (see: semantics.c/Conform)
each time the operator, function, or statement is executed. */
static bool ConvertLiteral(const struct Parameter *formal, int fcount, QString *tok, unsigned actual)
{
	if(IsLiteral(tok) && formal != NULL) {
		const struct Parameter *f = FormalForActual(formal, fcount, actual);
		SimpleType originalType, leftContext;
		BObject val;
		bool changed;
			
		assert(f != NULL && f->kind == LITERAL);	
		
		ConvertToObject(tok, &val, Proc()->callNestLevel);
		originalType = val.value.scalar.type;
		leftContext = WithLeftContext(f->type, tok);
		
		/*fprintf(stderr, "Changing type of ");
		DumpObject(&val);
		fprintf(stderr, " to %d\n", f->type);*/
		
		if(leftContext != T_MISSING || !Contextual(f->type))
			ChangeTypeWithContext(&val.value.scalar, leftContext, f->type);

		changed = originalType != val.value.scalar.type;
		/*fprintf(stderr, "types: %u -> %u\n", originalType, val.value.scalar.type);*/
		
		assert(!IndicatesError(&val));
		
		if(changed) {
			QsDispose(tok);
			MakeQuotedToken(&val.value.scalar, tok);
		}
		
		DisposeIfScalar(&val);
		
		return changed;
	}
	
	return FALSE;
}

/* Substitution of default values
   ==============================

Defaults for statement parameters will be 'inlined'. Again, this may mean type checking and conversion
can be avoided. */
static bool SubstituteDefault(const struct Statement *stmt, QString *tok, int nesting, unsigned actual)
{
	if(nesting == 0 && QsEqNoCase(tok, &g_Missing)) {
		const struct Parameter *formal = FormalForActual(stmt->formal, stmt->formalCount, actual);
			
		assert(formal != NULL && formal->kind == LITERAL);
		assert(formal->defaultValue != NULL);

		/*fprintf(stderr, "substituting default for param %d:\n", actual);*/
		
		QsDispose(tok);
		MakeQuotedToken(formal->defaultValue, tok);
		
		/*QsWrite(tok, stderr);
		fputc('\n', stderr);*/
		
		return TRUE;
	}
	
	return FALSE;
}

#define MAX_PASSES 10
#define MAX_NESTING_TRACKED_FOR_LITERAL_CONVERSION 10

extern const struct Parameter *GetPrototype(const BObject *applied, int *numFormals);

void Improve(struct TokenSequence *tokSeq)
{
	unsigned pass;
	bool changed = TRUE;
	
	/* Multiple passes are made over the statement, enabling optimisations that are made possible by
		other optimisations. */
	for(pass = 1; pass <= MAX_PASSES && changed; pass++) {	
		unsigned short t;
	
		changed = FALSE;

		{
			const BObject *functor[MAX_NESTING_TRACKED_FOR_LITERAL_CONVERSION];
			unsigned actualNumber[MAX_NESTING_TRACKED_FOR_LITERAL_CONVERSION];
			int nesting = 0;
			
			functor[nesting] = NULL;
			actualNumber[nesting] = 0;
		
			for(t = 0; t < tokSeq->length; t++) {
				QString *tok = &tokSeq->rest[t];
				
				Nest(tok, &nesting);
							
				if(1 <= nesting && nesting < MAX_NESTING_TRACKED_FOR_LITERAL_CONVERSION) {
					if(QsGetFirst(tok) == '(') {
						functor[nesting] = LookUp(tok + 1, Proc()->callNestLevel);
						actualNumber[nesting] = 0;
					}
					else if(t != 0 && QsGetFirst(tok - 1) != '(') {
						int nFormals = 0;
						const struct Parameter *params
							= functor[nesting] != NULL ? GetPrototype(functor[nesting], &nFormals) : NULL;
						changed |= ConvertLiteral(params, nFormals, tok, actualNumber[nesting]);
						++actualNumber[nesting];
					}
				}		
			}
		}
		
		for(t = 0; t < tokSeq->length; t++) {
			QString *tok = &tokSeq->rest[t];
			bool inFunction = QsIsNull(&tokSeq->statementName);
			
			changed |= InlineNamedConstant(tok);
			
			changed |= CreateNamedConstant(tok, inFunction);

			changed |= RemoveRedundantFunction(tok);

			changed |= EvaluateConstantValuedExpression(tok, inFunction);
		}
		
		if(changed) {
			unsigned short increment = 1;
			
			for(t = 0; t < tokSeq->length; t += increment)
				if(QsEqNoCase(&tokSeq->rest[t], &m_Removed)) {
					DeleteToken(tokSeq, t);
					increment = 0;
				}
				else
					increment = 1;
		}
		
		if(!QsIsNull(&tokSeq->statementName)) {
			unsigned actual = 0;
			int nesting = 0;
			
			/* If a statement is supplied, assume it's already been established to exist ... */
			
			if(tokSeq->command == NULL && GetStatement(&tokSeq->statementName, &tokSeq->command) != SUCCESS) {
				assert(FALSE);
				return;
			}
			
			/* Done after the main pass because sensitive to nesting, which might change as a result
				of other improvements. */
			
			for(t = 0; t < tokSeq->length; t++) {			
				QString *tok = &tokSeq->rest[t];
				
				Nest(tok, &nesting);
				
				assert(nesting >= 0);
				
				if(nesting == 0)
					changed |= ConvertLiteral(tokSeq->command->formal, tokSeq->command->formalCount, tok, actual);
				
				changed |= SubstituteDefault(tokSeq->command, tok, nesting, actual);

				actual += nesting == 0;
			}
		}
	}
	
	/*fprintf(stderr, "Optimisation passes = %u\n", pass - 1);*/
}
