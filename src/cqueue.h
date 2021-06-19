/****** cqueue.h ******/

/*
	$VER: cqueue.h 0.16A (3.9.2017)

	A simple fixed-size circular queue (FIFO): so simple it's implemented in this header.
*/

#ifndef CQUEUE_H_INCLUDED
#define CQUEUE_H_INCLUDED

#include <stdlib.h>
#include <string.h>
#include "stack.h"

typedef StackItemDisposer CQEntryDisposer;

struct CircularQueue {
	struct Stack stk; /* Inefficient, but suffices - only used for event queues, which are
						not particularly large. */
	CQEntryDisposer dispose;
};

static void CreateCQ(struct CircularQueue *q, size_t os, size_t capacity, CQEntryDisposer dispose)
{
	assert(q != NULL);
	assert(os != 0);
	assert(capacity != 0);
	
	StkInit(&q->stk);
	StkCreate(&q->stk, os, capacity);
	q->dispose = dispose;
}

#define CQIsFull(q) StkFull(&(q)->stk) 

#define CQIsEmpty(q) (StkHeight(&(q)->stk) == 0)

static bool DequeueFromCQ(struct CircularQueue *q, void *item)
{
	if(CQIsEmpty(q))
		return FALSE;
	else if(StkHeight(&q->stk) == 1) {
		StkPop(&q->stk, item);
		return TRUE;
	}
	else {
		if(item != NULL)
			memcpy(item, StkPeek(&q->stk, StkHeight(&q->stk) - 1), q->stk.itemSize);
		memmove(StkPeek(&q->stk, StkHeight(&q->stk) - 1), StkPeek(&q->stk, StkHeight(&q->stk) - 2),
			q->stk.itemSize * (StkHeight(&q->stk) - 1));
		StkDiscard(&q->stk, 1, q->dispose);
		return TRUE;
	}
}

static bool EnqueueOnCQ(struct CircularQueue *q, void *item, bool dropItemIfFull)
{
	if(CQIsFull(q) && dropItemIfFull)
		DequeueFromCQ(q, NULL);
	
	if(!CQIsFull(q)) {
		StkPush(&q->stk, item);
		return TRUE;
	}
	else
		return FALSE;
}

#define ClearCQ(q) StkClear(&(q)->stk, (q)->dispose)

static void DisposeCQ(struct CircularQueue *q)
{
	ClearCQ(q);
	StkDispose(&q->stk);
}

#endif /* CQUEUE_H_INCLUDED */
