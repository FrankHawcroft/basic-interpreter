/****** stack.h ******/

/*
	$VER: stack.h 0.16 (5.12.2012)

	A generic stack data type.
*/

#ifndef STACK_H_INCLUDED
#define STACK_H_INCLUDED

#include "platform.h"

struct Stack {
	PfGranularType *base;
	PfGranularType *top;
	PfGranularType *limit;
	PfGranularType *highest; /* Highest point ever reached. */
	size_t itemSize;
	size_t height;
};

extern void StkInit(struct Stack *stk);
extern void StkCreate(struct Stack *stk, size_t itemSize, unsigned maxHeight);
extern void StkResize(struct Stack *stk, unsigned newLimit); /* Only increasing the size is allowed. */
extern void StkPush(struct Stack *stk, const void *item);
extern void StkPop(struct Stack *stk, void *item);
#define StkHeight(stk) ((stk)->height)
/*extern int StkHeight(const struct Stack *stk);*/
extern int StkSpaceRemaining(const struct Stack *stk);
extern int StkHighWaterMark(const struct Stack *stk);
extern int StkLimit(const struct Stack *stk);
#define StkFull(stk) ((stk)->top >= (stk)->limit)
extern void *StkPeek(const struct Stack *stk, int offset); /* where 0 corresponds to TOS */

typedef void (*StackItemDisposer)(void *);

/* Discard a number of items from the stack - the function passed is called on each item, top to bottom. */
extern void StkDiscard(struct Stack *stk, unsigned count, StackItemDisposer dispose);

/* Clear the stack, disposing of each item using the function supplied. */
extern void StkClear(struct Stack *stk, StackItemDisposer dispose);

/* Clear without disposing of each item. */
extern void StkClearQuick(struct Stack *stk);

/* Dispose of the stack without clearing it first. */
extern void StkDispose(struct Stack *stk);

DIAGNOSTIC_FN_DECL(void StkRunTests(void));

#endif /* stack.h */
