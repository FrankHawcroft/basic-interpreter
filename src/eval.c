/****** eval.c ******/

/*
	$VER: eval.c 0.16 (12.12.2012)

	Expression evaluation. Conversion to prefix is a prerequisite - see syntax.c.
*/

#include <stdio.h>
#include "interpreter.h"
#include "stack.h"

#define EXPR_STK_LIMIT 667 /* due to increasing size by scale factor of 1.5, means the max height is 1000 */

static bool ExtendStack(struct Stack *stack, int required)
{
	bool ok = FALSE;
	unsigned limit = StkLimit(stack);
	unsigned newSize = (3 * limit) / 2;
		
	if(StkHeight(stack) + required > newSize)
		newSize = StkHeight(stack) + required;
		
	if(StkHeight(stack) <= EXPR_STK_LIMIT) {
		ok = StkResize(stack, newSize);
/*
#ifdef DEBUG
		fprintf(stderr, 
			ok ? "[ExprStack: extended from size %u --> %u]\n" 
				: "[ExprStack: FAILED to extend from size %u --> %u]\n", limit, newSize);
#endif
*/
	}
/*
#ifdef DEBUG
	else
		fprintf(stderr, "[ExprStack: required size %u is above absolute limit of %u and stack will not be extended!]\n", 
			newSize, EXPR_STK_LIMIT);
#endif
*/	
	if(!ok) {
		BObject err;
		if(StkFull(stack))
			CutExprStk(stack, 1);
		SetObjectToError(&err, ER_STACK_OVERFLOW);
		StkPush(stack, &err);
	}
	
	return ok;
}

INLINE bool ExtendStackIfNecessary(struct Stack *stack, int required)
{	
	return StkSpaceRemaining(stack) >= required || ExtendStack(stack, required);
}

/* To save copying to a temporary object, expression evaluation doesn't push objects on the stack
	in the usual way, but instead copies directly to memory at the TOS pointer. This means the
	eval functions have to check there's enough stack space first. Doing a direct memory copy
	(structure assignment) also means care must be taken with long-lived reference-counted (string)
	objects, to ensure only one reference is held. */
INLINE void AdjustStackPointersFollowingDirectPush(struct Stack *stack)
{
	stack->top += stack->itemSize;
	/* Don't maintain the high water mark, since it isn't actually needed for the expr eval stack. */
	/*if(stack->top > stack->highest)
		stack->highest = stack->top;*/
	++stack->height;
}

static void Apply(const BObject *functor, struct Stack *stack, unsigned count)
{
	BObject *param;
	Error error = ARRAYEXPECTED;
	BObject result;
	
	assert(count != 0 && count <= StkHeight(stack)); /* TODO height same means infinite recursion. Detect? */
	
	param = PeekExprStk(stack, (int)count - 1);
	
	if(functor->category == OPERATOR) {
		assert(count == OperandCount(functor->value.opRef)); /* assume syntax checked */
		result.category = LITERAL;		
		if((error = ConformQuickly(ParametersForOperator(functor->value.opRef), param, count)) == SUCCESS)
			EvalOperation(&result.value.scalar, functor->value.opRef,
				&param[0].value.scalar, count == 1 ? NULL : &param[1].value.scalar);
	}
	else if((error = ConformForApplication(functor, param, count)) == SUCCESS) {
		if(functor->category == FUNCTION)	
			CallFunction(&result, functor->value.function, param, count, stack);
		else if(IsVariable(functor)
		&& (error = IndexArray(&result.value.variable, VarPtr(functor), param, count)) == SUCCESS)
			result.category = (result.value.variable.dim.few[0] != -1 ? ARRAY : SCALAR_VAR) | VARIABLE_IS_REF;
	}
	
	if(error != SUCCESS)
		SetObjectToError(&result, error);
	
	CutExprStk(stack, count);
	
	/* Because one or more args have just been popped, no need to check stack space. */
	*(BObject *)stack->top = result;
	AdjustStackPointersFollowingDirectPush(stack);
	
	/* Strictly, the applied object should be disposed of, but not doing it saves time for all
		valid objects (arrays, functions, operators). If a string literal was appplied, it would
		need to be disposed of, but this is caught by syntax checking, and the risk of a memory
		leak (if syntax checking is bypassed or buggy) isn't worth the overhead on every
		legitimate application. */
	/*DisposeIfScalar(functor);*/
	
	/*fprintf(stderr, "[Eval-->: ");
	DumpObject(&result);
	fprintf(stderr, "]\n");*/
}

static void EvalParameterlessFunction(const struct Function *f, struct Stack *exprStack)
{
	BObject result;
		
	assert(!StkFull(exprStack));
	
	/* Unfortunately, need to check the arg count at eval time because an expr might be precompiled
		before all the functions it uses are defined (in a STATIC function, for instance). */
	if(f->numArgs == 0)
		CallFunction(&result, f, NULL, 0, exprStack);
	else
		SetObjectToError(&result, BADARGCOUNT);
	
	*(BObject *)exprStack->top = result;
	AdjustStackPointersFollowingDirectPush(exprStack);
}

/* Evaluate a sequence of expressions, pushing the results on the stack. */
const QString *Eval(const QString *toks, Interner intern, unsigned tokIndex, struct Stack *exprStack)
{
	const QString *ct;
	char firstCh;

	assert(toks != NULL && intern != NULL && exprStack != NULL);

	/*{
		int i;
		fprintf(stderr, "[-->Eval: ");
		for(i = 0; i < 4 && (firstCh = QsGetFirst(toks + i)) != '|' && firstCh != ')'; i++) {
			fputc(' ', stderr);
			QsWrite(toks + i, stderr);
		}
		fprintf(stderr, "%s]\n", firstCh != '|' && firstCh != ')' ? "..." : "");
	}*/

	for(ct = toks; (firstCh = QsGetFirst(ct)) != '|' && firstCh != ')'; ct++, tokIndex++) {	
		if(firstCh == '(') {
			/* A general 'apply' expression - function, operator, or subscripted array variable,
				which will be consistently in the Lisp-like prefix (f a b c ...) form. */

			const QString *post;
			unsigned priorHeight = StkHeight(exprStack);
			BObject obj;
			
			post = Eval(ct + 2, intern, tokIndex + 2, exprStack);
			assert(post > ct + 2);	
			intern(tokIndex + 1, ct + 1, &obj);	
			Apply(&obj, exprStack, StkHeight(exprStack) - priorHeight);
			tokIndex += post - ct;
			ct = post;
		}
		else {
			/* If not an 'apply' form it's an unsubscripted variable, a literal, a label, or a
				parameterless function. Intern directly to the TOS, to avoid an extra object copy. */
			
			if(StkFull(exprStack) && !ExtendStackIfNecessary(exprStack, 1))
				return ct + 1; /* TODO - wrong - need to search for the matching rparen or an EOS marker */
			
			intern(tokIndex, ct, (BObject *)exprStack->top);
			
			if(((BObject *)exprStack->top)->category == FUNCTION)
				EvalParameterlessFunction(((BObject *)exprStack->top)->value.function, exprStack);
			else
				AdjustStackPointersFollowingDirectPush(exprStack);
			
			/*fprintf(stderr, "[Eval-->: ");
			DumpObject((BObject *)exprStack->top);
			fprintf(stderr, "]\n");*/
		}
	}
	
	return ct;
}

/* A quicker and smaller version of Eval, usable when the expression has already been converted to BObjects. */
const BObject *EvalPreconverted(const BObject *exprSeq, struct Stack *exprStack, int stackSpaceRequired)
{
	assert(exprSeq != NULL && exprStack != NULL);
	assert(stackSpaceRequired >= 0);
	
	/* Avoid checking stack space repeatedly in the loop - */
	if(!ExtendStackIfNecessary(exprStack, stackSpaceRequired))
		return exprSeq + stackSpaceRequired - 1; /* TODO - wrong - need to search for the matching rparen or an EOS marker */

	for( ; exprSeq->category != PUNCTUATION || !exprSeq->value.punctuation->terminatesExpressionSequence; exprSeq++) {
		if(exprSeq->category == PUNCTUATION) {
			/* Function, operator, or subscripted array variable. */
			const BObject *post;
			unsigned priorHeight = StkHeight(exprStack);
		
			/* Only nested Lisp-like expressions are expected here at eval time; apart from expression terminators,
				punctuation should have been stripped out in the transformation to prefix. */
			assert(exprSeq->value.punctuation->introducesNestedExpressionSequence);
		
			post = EvalPreconverted(exprSeq + 2, exprStack, stackSpaceRequired - 3);		
			assert(post > exprSeq + 2);
			Apply(exprSeq + 1, exprStack, StkHeight(exprStack) - priorHeight);			
			exprSeq = post;
		}
		else if(exprSeq->category != FUNCTION) {
			/* Variable, literal, or label. */
			CopyObject((BObject *)exprStack->top, exprSeq);
				/* CopyObject is needed rather than a direct structure assignment, because the pre-converted expression
					sequence is long-lived. */
			AdjustStackPointersFollowingDirectPush(exprStack);
		}
		else
			/* Parameterless function. */
			EvalParameterlessFunction(exprSeq->value.function, exprStack);
	}
	
	return exprSeq;
}

extern void DefaultOutOfMemoryHandler(size_t);

void CreateExprStk(struct Stack *stack, unsigned maxHeight)
{
	StkInit(stack);
	if(StkCreate(stack, sizeof(BObject), maxHeight) == NULL)
		DefaultOutOfMemoryHandler(maxHeight * sizeof(BObject));
}

void ClearExprStk(struct Stack *stack)
{
	StkClear(stack, (StackItemDisposer)&DisposeIfScalar);
}

void CutExprStk(struct Stack *stack, int count)
{
	StkDiscard(stack, count, (StackItemDisposer)&DisposeIfScalar);
}

void DisposeExprStk(struct Stack *stack)
{
	ClearExprStk(stack);
	StkDispose(stack);
}

#ifdef DEBUG

/* Displays the contents of the stack. The top of the stack is to the right. */
void DumpExprStk(const struct Stack *stack)
{
	int offset;
	for(offset = StkHeight(stack) - 1; offset >= 0; offset--) {
		fputc(' ', stderr);
		DumpObject(PeekExprStk(stack, offset));	
	}
	fprintf(stderr, ".\n");
}

#endif /* DEBUG */
