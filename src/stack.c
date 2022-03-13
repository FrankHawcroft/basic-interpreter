/****** stack.c ******/

/*
	$VER: stack.c 0.16A (5.18.2015)

	A 'generic' stack type.
*/

#include <string.h> /* memset, memcpy */
#include <stdio.h> /* size_t, and printf for testbed output */
#include <stdlib.h> /* for testbed */
#include "common.h"
#include "platform.h"
#include "heap.h"
#include "stack.h"

#ifdef DEBUG

static bool StkIsValid(const struct Stack *stk)
{
	return (stk->base == NULL && stk->top == NULL && stk->limit == NULL)
		|| (stk->base != NULL && stk->itemSize > 0 && stk->top >= stk->base && stk->top <= stk->limit);
}

#endif

static PfGranularType *StkOffset(const struct Stack *stk, int idx)
{
	return stk->base + idx * stk->itemSize;
}

void StkInit(struct Stack *stk)
{
	stk->base = stk->top = stk->limit = stk->highest = NULL;
	stk->itemSize = stk->height = 0;
}

struct Stack *StkCreate(struct Stack *stk, size_t itemSize, unsigned maxHeight)
{
	assert(stk->base == NULL);
	assert(itemSize != 0 && maxHeight != 0);

	/* Stacks are allocated on the heap, rather than keeping them completely
		separate. Seems reasonable - but means you need to bear control flow
		and expression evaluation stack sizes in mind when using a fixed-size heap. */

	stk->base = (PfGranularType *)TolerantNew(itemSize * maxHeight);
	if(stk->base == NULL)
		return NULL;
	
	stk->top = stk->highest = stk->base;
	stk->itemSize = itemSize;
	stk->height = 0;
	stk->limit = StkOffset(stk, maxHeight);

	assert(StkIsValid(stk));
	
	return stk;
}

bool StkResize(struct Stack *stk, unsigned newLimit)
{
	int height = StkHeight(stk), hwm = StkHighWaterMark(stk);
	PfGranularType *oldStorage = stk->base;
	
	assert(newLimit >= (unsigned)StkLimit(stk));
	
	stk->base = NULL;
	if(StkCreate(stk, stk->itemSize, newLimit) == NULL) {
		stk->base = oldStorage;
		return FALSE;
	}
	stk->height = height;
	if(height > 0)
		memcpy(stk->base, oldStorage, stk->itemSize * (unsigned)height);
	stk->top = StkOffset(stk, height);
	stk->highest = StkOffset(stk, hwm);
	
	assert(StkIsValid(stk));
	
	Dispose(oldStorage);
	
	return TRUE;
}

void StkPush(struct Stack *stk, const void *item)
{
#ifdef DEBUG
	/* Callers need to check this precondition - */
	if(StkFull(stk)) {
		fprintf(stderr, "[Stack] error: overflow - exiting.\n");
		exit(EXIT_FAILURE);
	}
#endif

	memcpy((void *)stk->top, item, stk->itemSize);
	stk->top += stk->itemSize;
	++stk->height;

	if(stk->top > stk->highest)
		stk->highest = stk->top;
}

void StkPop(struct Stack *stk, void *item)
{
	/* Callers need to check - */
	assert(stk->top > stk->base);

	if(item != NULL)
		memcpy(item, stk->top - stk->itemSize, stk->itemSize);
	stk->top -= stk->itemSize;
	--stk->height;
}

static int Scaled(const PfGranularType *higher, const PfGranularType *lower, const struct Stack *stk)
{
	return (higher - lower) / stk->itemSize;
}

int StkSpaceRemaining(const struct Stack *stk)
{
	return Scaled(stk->limit, stk->top, stk);
}

int StkHighWaterMark(const struct Stack *stk)
{
	return Scaled(stk->highest, stk->base, stk);
}

int StkLimit(const struct Stack *stk)
{
	return Scaled(stk->limit, stk->base, stk);
}

void *StkPeek(const struct Stack *stk, int offset)
{
	assert(offset < StkHeight(stk));

	return stk->top - (offset + 1) * stk->itemSize;
}

void StkDiscard(struct Stack *stk, int n, StackItemDisposer dispose)
{
	assert(n >= 0);
	assert(n <= StkHeight(stk));

	stk->height -= n;
	
	while(n-- > 0)
		dispose(stk->top -= stk->itemSize);
}

void StkClear(struct Stack *stk, StackItemDisposer dispose)
{
	if(stk->base != NULL)
		StkDiscard(stk, StkHeight(stk), dispose);
}

/* Clear quickly - use only when items don't contain pointers
to dynamically allocated memory. */
void StkClearQuick(struct Stack *stk)
{
	stk->top = stk->base;
	stk->height = 0;
}

/* Does not clear first! Use only when known to be clear,
or if stacked items don't contain pointers to dynamically
allocated memory. */
void StkDispose(struct Stack *stk)
{
	assert(StkIsValid(stk));

	if(stk->base != NULL)
		Dispose(stk->base);
	StkInit(stk);
}

#ifdef DEBUG

/****** Stack testbed ******/

/* To try to cut down on inter-module testing dependencies, don't rely on the heap for per-item allocation ... 
	StkCreate does, but that's only one allocation. */
static void FreeStr(void *item) { char *s = *(char **)item; if(s != NULL) free(s); }

void StkRunTests(void)
{
	struct Stack s;
	char *item1, *item2, *item3, *itempopped;
	int i, heightBefore;

	fprintf(stderr, "-- Stack self-tests running ...\n");
	
	/* StkInit */
	StkInit(&s);
	
	/* StkCreate, StkHeight */
	assert(StkCreate(&s, sizeof(char *), 10) != NULL);
	assert(StkHeight(&s) == 0); /* empty has height 0 */

	/* StkPush */
	item1 = strdup("First");
	StkPush(&s, &item1);
	item2 = strdup("Second");
	StkPush(&s, &item2);
	assert(StkHeight(&s) == 2); /* height after pushing two strings */

	/* StkPop */
	StkPop(&s, &itempopped);
	/*fprintf(stderr, "Popped: %s\n", itempopped);*/
	assert(strcmp(itempopped, "Second") == 0 && StkHeight(&s) == 1); /* pop behaves as expected */
	free(itempopped);

	/* StkPush following pop */
	item3 = strdup("Third");
	StkPush(&s, &item3);
	/*fprintf(stderr, "Pushed another string.\nNow scanning stack top to bottom:\n");*/
	for(i = 0; i < StkHeight(&s); i++)
		assert(*(char **)StkPeek(&s, i) != NULL); /* fprintf(stderr, "%s\n", */ 

	/* StkResize, StkLimit */
	heightBefore = StkHeight(&s);
	StkResize(&s, 12);
	assert(StkHeight(&s) == heightBefore);
	assert(StkLimit(&s) == 12);
	
	/* StkClear */
	StkClear(&s, FreeStr);
	assert(StkHeight(&s) == 0); /* empty once cleared */
	/*fprintf(stderr, "Cleared.\nHeight now: %d\n", StkHeight(&s));*/
	
	/* StkDispose */
	StkDispose(&s);
	/*fprintf(stderr, "Disposed.\n");*/
	
	fprintf(stderr, "-- Stack self-tests finished.\n");
}

#endif /* DEBUG defined (testbed) */
