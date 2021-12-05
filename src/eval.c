/****** eval.c ******/

/*
	$VER: eval.c 0.16 (12.12.2012)

	Expression evaluation. Conversion to prefix is a prerequisite - see syntax.c.
*/

#include <stdio.h>
#include "interpreter.h"
#include "stack.h"

INLINE void AdjustStackPointersFollowingDirectPush(struct Stack *stack)
{
	stack->top += stack->itemSize;
	if(stack->top > stack->highest)
		stack->highest = stack->top;
	++stack->height;
}

#define EXPR_STK_LIMIT 667 /* due to increasing size by scale factor of 1.5, corresponds to a limit of 1000 */

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
	if(!StkFull(stack)) {
		/* Special-cased rather than calling StkPush, because
			structure assignment is quicker than memcpy with Amiga-GCC. */
		*(BObject *)stack->top = *newTop;
		AdjustStackPointersFollowingDirectPush(stack);
	}
	else if(StkFull(stack) && StkHeight(stack) <= EXPR_STK_LIMIT) {
		StkResize(stack, StkHeight(stack) < 8 ? 16 : (3 * StkHeight(stack)) / 2);
		/*fprintf(stderr, "[Stack extended from size %d --> %d]\n", height, height < 8 ? 16 : (3 * height) / 2);*/
		StkPush(stack, newTop);
	}
	else {
		BObject err;
		
		CutExprStk(stack, 1);
		SetObjectToError(&err, ER_STACK_OVERFLOW);
		StkPush(stack, &err);
	}
}

static void Apply(const BObject *functor, struct Stack *stk, unsigned priorHeight)
{
	BObject *param;
	unsigned count;
	Error error;
	BObject result;
	
	assert(StkHeight(stk) > (int)priorHeight); /* TODO height same means infinite recursion. Detect? */
			
	count = StkHeight(stk) - priorHeight;
	param = PeekExprStk(stk, count - 1);
	
	error = ConformForApplication(functor, param, count);
	
	if(error != SUCCESS)
		SetObjectToError(&result, error);
	else if(functor->category == OPERATOR) {
		assert(count == OperandCount(functor->value.opRef));
		result.category = LITERAL;
		EvalOperation(&result.value.scalar, functor->value.opRef,
			&param[0].value.scalar, count == 1 ? NULL : &param[1].value.scalar);
	}
	else if(functor->category == FUNCTION)
		CallFunction(&result, functor->value.function, param, count, stk);
	else if(IsVariable(functor)) {
		if((error = IndexArray(&result.value.variable, VarPtr(functor), param, count)) == SUCCESS)
			result.category = (result.value.variable.dim.few[0] != -1 ? ARRAY : SCALAR_VAR) | VARIABLE_IS_REF;
		else
			SetObjectToError(&result, error);
	}
	else
		SetObjectToError(&result, ARRAYEXPECTED);
	
	CutExprStk(stk, count);
	PushObject(stk, &result);
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

static void EvalParameterlessFunction(BObject *f, struct Stack *stk)
{
	/* Unfortunately, need to check the arg count at eval time because an expr might
		be precompiled before all the functions it uses are defined (in a STATIC function,
		for instance). */
	if(f->value.function->numArgs == 0)
		/* Evaluate in-place, reusing the supplied function object to store the result. */
		CallFunction(f, f->value.function, NULL, 0, stk);
	else
		SetObjectToError(f, BADARGCOUNT);
	PushObject(stk, f);
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
			/* A general 'apply' expression - function, operator, or subscripted array variable. */

			const QString *post;
			unsigned priorHeight = StkHeight(exprStack);
			
			post = Eval(ct + 2, intern, tokIndex + 2, exprStack);	
			assert(post > ct + 2);	
			intern(tokIndex + 1, ct + 1, &obj);
			Apply(&obj, exprStack, priorHeight);
			tokIndex += post - ct;
			ct = post;
		}
		else if(!StkFull(exprStack)) {
			/* If not an 'apply' (f a b c ...) form, it's an unsubscripted variable, a literal, a label,
				or a parameterless function. Special-cased to intern directly to the TOS, to avoid an
				extra object copy. */
			
			intern(tokIndex, ct, (BObject *)exprStack->top);
			
			if(((BObject *)exprStack->top)->category == FUNCTION) {
				obj = *(BObject *)exprStack->top;
				EvalParameterlessFunction(&obj, exprStack);
			}
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
				EvalParameterlessFunction(&obj, exprStack);
			else
				PushObject(exprStack, &obj);
		}
	}
	
	return ct;
}

const BObject *EvalPreconverted(const BObject *exprSeq, struct Stack *exprStack)
{
	const BObject *cobj;

	assert(exprSeq != NULL && exprStack != NULL);

	for(cobj = exprSeq; cobj->category != PUNCTUATION || !cobj->value.punctuation->terminatesExpressionSequence; cobj++) {
		BObject result;
		
		switch(cobj->category) {
			case PUNCTUATION: {
				/* Function, operator, or subscripted array variable. */
				const BObject *post;
				unsigned priorHeight = StkHeight(exprStack);
			
				/* Only nested Lisp-like expressions are expected here at eval time; apart from expression terminators,
					punctuation should have been stripped out in the transformation to prefix. */
				assert(cobj->value.punctuation->introducesNestedExpressionSequence);
			
				post = EvalPreconverted(cobj + 2, exprStack);		
				assert(post > cobj + 2);
				Apply(cobj + 1, exprStack, priorHeight);			
				cobj = post;
				break;
			}
			case FUNCTION:
				/* Parameterless. Copy, to get a 'scratch' object, eval in place, and push the result. */
				CopyObject(&result, cobj);
				EvalParameterlessFunction(&result, exprStack);
				break;
			default:
				if(!StkFull(exprStack)) {
					CopyObject((BObject *)exprStack->top, cobj);			
					AdjustStackPointersFollowingDirectPush(exprStack);
				}
				else {
					CopyObject(&result, cobj);
					PushObject(exprStack, &result);
				}
				break;
		}
	}
	
	return cobj;
}

void CreateExprStk(struct Stack *stack, unsigned maxHeight)
{
	StkInit(stack);
	StkCreate(stack, sizeof(BObject), maxHeight);
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
