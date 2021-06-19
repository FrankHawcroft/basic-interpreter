/****** eval.c ******/

/*
	$VER: eval.c 0.16 (12.12.2012)

	Expression evaluation. Conversion to prefix is a prerequisite - see syntax.c.
*/

#include <stdio.h>
#include "interpreter.h"
#include "stack.h"
#include "process.h"

#define EXPR_STK_LIMIT 667 /* due to increasing size by scale factor of 1.5, corresponds to a limit of 1000 */

/* Pushes the BObject onto the stack. The stack is extended if necessary.
	The stack simply does a memcpy ('shallow') copy of the object, so newTop
	should be considered discarded after this function has been called, and
	should not be RemoveObject()d. 
	If the stack can't be extended because it has reached the stack height
	limit, the top object will be discarded and an overflow error pushed. 
	
	PopObject - defined as a macro in interpreter.h, for performance on older
	systems - is the complement of PushObject - the unstacked BObject is memcpy()d
	back off the stack, and the caller takes ownership of any memory
	allocated in it. */
INLINE void PushObject(struct Stack *stack, const BObject *newTop)
{
	if(StkFull(stack) && StkHeight(stack) <= EXPR_STK_LIMIT) {
		StkResize(stack, StkHeight(stack) < 8 ? 16 : (3 * StkHeight(stack)) / 2);
		/*fprintf(stderr, "[Stack extended from size %d --> %d]\n", height, height < 8 ? 16 : (3 * height) / 2);*/
	}
	
	if(!StkFull(stack))
		StkPush(stack, newTop);
	else {
		BObject err;
		
		CutExprStk(stack, 1);
		SetObjectToError(&err, ER_STACK_OVERFLOW);
		StkPush(stack, &err);
	}
}

static void Apply(const BObject *obj, BObject *param, unsigned count, BObject *result, struct Stack *stk)
{
	Error error = IsEmpty(obj) ? UNDEFINEDVARORFUNC : ConformForApplication(obj, param, count);
	
	if(error == SUCCESS) {
		if(obj->category == OPERATOR) {
			assert(count == OperandCount(obj->value.opRef));
			result->category = LITERAL;
			EvalOperation(&result->value.scalar, obj->value.opRef,
				&param[0].value.scalar, count == 1 ? NULL : &param[1].value.scalar);
		}
		else if(IsVariable(obj)) {
			if((error = IndexArray(&result->value.variable, VarPtr(obj), param, count)) == SUCCESS)
				result->category = (result->value.variable.dim.few[0] != -1 ? ARRAY : SCALAR_VAR) | VARIABLE_IS_REF; /* vvv */
		}
		else if(obj->category == FUNCTION)
			CallFunction(result, obj->value.function, param, count, stk);
		else
			error = ARRAYEXPECTED;
	}
	
	if(error != SUCCESS)
		SetObjectToError(result, error);
}

static void EvalParameterlessFunction(BObject *f, struct Stack *stk)
{
	if(f->value.function->numArgs == 0)
		CallFunction(f, f->value.function, NULL, 0, stk);
	else
		SetObjectToError(f, BADARGCOUNT);
}

/* Evaluate a sequence of expressions, pushing the results on the stack. */
const QString *Eval(const QString *toks, Interner intern, unsigned tokIndex, struct Stack *exprStack)
{
	const QString *ct;
	char firstCh;
	bool outermost = tokIndex == 0;

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
			BObject result;
			unsigned preHeight = StkHeight(exprStack), count;

			intern(tokIndex + 1, ct + 1, &obj);
			post = Eval(ct + 2, intern, tokIndex + 2, exprStack);
			
			assert(post > ct + 2);
			
			tokIndex += post - ct;
			ct = post;

			assert(StkHeight(exprStack) > (int)preHeight); /* TODO height same means infinite recursion. Detect */

			count = StkHeight(exprStack) - preHeight;
			Apply(&obj, PeekExprStk(exprStack, count - 1), count, &result, exprStack);
			RemoveObject(&obj, FALSE);
			CutExprStk(exprStack, count);
			PushObject(exprStack, &result);
			
			/*fprintf(stderr, "[Eval-->: ");
			DumpObject(&result);
			fprintf(stderr, "]\n");*/
		}
		else {
			/* An unsubscripted variable, a literal, a label, or a parameterless function. */

			intern(tokIndex, ct, &obj);
			if(obj.category == FUNCTION)
				EvalParameterlessFunction(&obj, exprStack);
			PushObject(exprStack, &obj);
			
			/*fprintf(stderr, "[Eval-->: ");
			DumpObject(&obj);
			fprintf(stderr, "]\n");*/
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
		
		if(cobj->category == PUNCTUATION && cobj->value.punctuation->introducesNestedExpressionSequence) {
			/* A general 'apply' expression - function, operator, or subscripted array variable. */

			const BObject *post;
			unsigned preHeight = StkHeight(exprStack), count;

			post = EvalPreconverted(cobj + 2, exprStack);
			
			assert(post > cobj + 2);
			assert(StkHeight(exprStack) > (int)preHeight);

			count = StkHeight(exprStack) - preHeight;
			Apply(cobj + 1, PeekExprStk(exprStack, count - 1), count, &result, exprStack);
			
			cobj = post;
			CutExprStk(exprStack, count);	
		}
		else {	
			CopyObject(&result, cobj);
			if(cobj->category == FUNCTION)
				EvalParameterlessFunction(&result, exprStack); /* Eval in-place */
		}
		
		PushObject(exprStack, &result);
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
