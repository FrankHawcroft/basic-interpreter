/****** eval.c ******/

/*
	$VER: eval.c 0.16 (12.12.2012)

	Expression evaluation. Conversion to prefix is a prerequisite - see syntax.c.
*/

#include <stdio.h>
#include "interpreter.h"
#include "stack.h"

#define EXPR_STK_LIMIT 667 /* due to increasing size by scale factor of 1.5, means the max height is 1000 */

static bool ExtendStackIfNecessary(struct Stack *stack, int required)
{
	bool ok = TRUE;
	
	if(StkSpaceRemaining(stack) < required) {
		if(StkHeight(stack) <= EXPR_STK_LIMIT) {
			unsigned limit = StkLimit(stack);
			unsigned newSize = (3 * limit) / 2;
			if(StkHeight(stack) + required > newSize)
				newSize = StkHeight(stack) + required;
			ok = StkResize(stack, newSize);
/*
#ifdef DEBUG
			fprintf(stderr, 
				ok ? "[ExprStack: extended from size %u --> %u]\n" 
				   : "[ExprStack: FAILED to extend from size %u --> %u]\n", limit, newSize);
#endif
*/
		}
		else
			ok = FALSE;
		
		if(!ok) {
			BObject err;
			if(StkFull(stack))
				CutExprStk(stack, 1);
			SetObjectToError(&err, ER_STACK_OVERFLOW);
			StkPush(stack, &err);
		}
	}
	
	return ok;
}

INLINE void AdjustStackPointersFollowingDirectPush(struct Stack *stack)
{
	stack->top += stack->itemSize;
	/* Don't maintain the high water mark, since it isn't actually needed for the expr eval stack. */
	/*if(stack->top > stack->highest)
		stack->highest = stack->top;*/
	++stack->height;
}

/* Pushes the BObject onto the stack. The stack is extended if necessary.
	The stack simply does a 'shallow' copy of the object structure, so newTop
	should be considered discarded after this function has been called, and
	should not be RemoveObject()d. 
	If the stack can't be extended because it has reached the stack height
	limit, the top object will be discarded and an overflow error pushed. 
	
	PopObject - defined as a macro in interpreter.h - is the complement of
	PushObject - the unstacked BObject is memcpy()d back off the stack, and
	the caller takes ownership of any memory allocated in it. */
INLINE void PushObject(struct Stack *stack, const BObject *newTop)
{
	/* StkFull is quicker than calling StkSpaceRemaining (it's a macro, with no multiplication) */
	if(!StkFull(stack)) {
		/* Special-cased rather than calling StkPush, because
			structure assignment is quicker than memcpy with Amiga-GCC. */
		*(BObject *)stack->top = *newTop;
		AdjustStackPointersFollowingDirectPush(stack);
	}
	else if(ExtendStackIfNecessary(stack, 1))
		StkPush(stack, newTop);
}

static void Apply(const BObject *functor, struct Stack *stack, unsigned count)
{
	BObject *param;
	Error error;
	BObject result;
	
	assert(count != 0 && count <= StkHeight(stack)); /* TODO height same means infinite recursion. Detect? */
	
	param = PeekExprStk(stack, count - 1);
	error = ConformForApplication(functor, param, count);
	
	if(error != SUCCESS)
		SetObjectToError(&result, error);
	else if(functor->category == OPERATOR) {
		assert(count == OperandCount(functor->value.opRef)); /* assume syntax checked */
		result.category = LITERAL;
		EvalOperation(&result.value.scalar, functor->value.opRef,
			&param[0].value.scalar, count == 1 ? NULL : &param[1].value.scalar);
	}
	else if(functor->category == FUNCTION)
		CallFunction(&result, functor->value.function, param, count, stack);
	else if(IsVariable(functor)) {
		if((error = IndexArray(&result.value.variable, VarPtr(functor), param, count)) == SUCCESS)
			result.category = (result.value.variable.dim.few[0] != -1 ? ARRAY : SCALAR_VAR) | VARIABLE_IS_REF;
		else
			SetObjectToError(&result, error);
	}
	else
		SetObjectToError(&result, ARRAYEXPECTED);
	
	CutExprStk(stack, count);
	
	/* Because one or more args have just been popped, no need to check stack space. */
	*(BObject *)stack->top = result;
	AdjustStackPointersFollowingDirectPush(stack);
	
	/* Strictly, the applied object should be disposed of, but not doing it saves time for all
		valid objects (arrays, functions, operators). If a string literal was appplied, it would
		need to be disposed of, but this is caught by syntax checking, and the risk of a memory
		leak (if syntax checking is bypassed or buggy) isn't worth the overhead on every
		legitimate application. */
	/*RemoveObject(functor, FALSE);*/
	
	/*fprintf(stderr, "[Eval-->: ");
	DumpObject(&result);
	fprintf(stderr, "]\n");*/
}

static void EvalParameterlessFunction(const struct Function *f, struct Stack *exprStack)
{
	BObject result;
	/* Unfortunately, need to check the arg count at eval time because an expr might
		be precompiled before all the functions it uses are defined (in a STATIC function,
		for instance). */
	if(f->numArgs == 0)
		CallFunction(&result, f, NULL, 0, exprStack);
	else
		SetObjectToError(&result, BADARGCOUNT);
	PushObject(exprStack, &result);
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
		BObject obj;
	
		if(firstCh == '(') {
			/* A general 'apply' expression - function, operator, or subscripted array variable,
				which will be consistently in the Lisp-like prefix (f a b c ...) form. */

			const QString *post;
			unsigned priorHeight = StkHeight(exprStack);
			
			post = Eval(ct + 2, intern, tokIndex + 2, exprStack);
			assert(post > ct + 2);	
			intern(tokIndex + 1, ct + 1, &obj);
			
			Apply(&obj, exprStack, StkHeight(exprStack) - priorHeight);
			tokIndex += post - ct;
			ct = post;
		}
		else if(!StkFull(exprStack)) {
			/* If not an 'apply' form it's an unsubscripted variable, a literal, a label, or a
				parameterless function. Intern directly to the TOS, to avoid an extra object copy. */
			
			intern(tokIndex, ct, (BObject *)exprStack->top);
			
			if(((BObject *)exprStack->top)->category == FUNCTION)
				EvalParameterlessFunction(((BObject *)exprStack->top)->value.function, exprStack);
			else
				AdjustStackPointersFollowingDirectPush(exprStack);
			
			/*fprintf(stderr, "[Eval-->: ");
			DumpObject(&obj);
			fprintf(stderr, "]\n");*/
		}
		else {
			/* If stack's full, intern to a temporary location, then push. Slower, but the stack can grow itself. */
			intern(tokIndex, ct, &obj);
			if(obj.category == FUNCTION)
				EvalParameterlessFunction(obj.value.function, exprStack);
			else
				PushObject(exprStack, &obj);
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

static void ShallowDispose(BObject *obj) { RemoveObject(obj, FALSE); }

#define ESTK_DISPOSE (StackItemDisposer)&ShallowDispose

void ClearExprStk(struct Stack *stack)
{
	StkClear(stack, ESTK_DISPOSE);
}

void CutExprStk(struct Stack *stack, int count)
{
	StkDiscard(stack, count, ESTK_DISPOSE);
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
