/****** heap.h ******/

/*
	$VER: heap.h 0.16A (12.9.2015)

	Memory management. The heap is singleton and global.
	
	On platforms where multiple tasks can run the same image (PF_REENTRANT),
	it is also somewhat thread-safe.
*/

#ifndef BAS_HEAP_H_INCLUDED
#define BAS_HEAP_H_INCLUDED

#include "common.h"

/*** Debugging support ***/

/* Choose how much sanity checking is done by the heap.

0 --> no checking - recommended for 'release' (NDEBUG) builds.

1 --> simple checking for common signs of freeing twice or other heap corruption.

2 --> the above, plus pads allocations with guard words to check for writing (just) beyond
the ends.

3 --> both of the above, plus the filename and line number where each allocation is made
is tracked. 

4 --> all of the above, plus memory vectors returned by New() and released using Dispose()
will be filled with random values. This helps to detect use of uninitialised memory, and
use after freeing. The symptom of such problems when this option is enabled is often 
that the program crashes, so it should be used with caution on systems which don't provide
memory protection and/or the ability to attach a debugger to a process when it does something
illegal. This option adds some time overhead per allocation and deallocation as well. */

#ifdef DEBUG
#define HEAP_SANITY_CHECK_LEVEL 2 /* Set to between 1 and 4 */
#else
#define HEAP_SANITY_CHECK_LEVEL 0 /* Generally, set to 0 if not in a DEBUG build */
#endif

/* Create a heap with the given initial size, and optionally fix the size.
	If fixedSize == FALSE, the heap will grow (and shrink) dynamically. */
extern void CreateHeap(size_t initialSize, bool fixedSize);

/* Install an out of memory handling function. This is called if New() is unable
	to allocate memory. The default out of memory handler does minimal clean up
	and then calls exit(). */
typedef void (*OutOfMemoryHandler)(size_t sizeRequested);
extern void SetOutOfMemoryHandler(OutOfMemoryHandler handler);

/* Allocate memory.
	If sufficient memory is available, New and TolerantNew will return a pointer to
the allocated memory. Memory is not initialised!
	New will call the out of memory handler if it fails. If this function returns -
which the default noted above does not - New will then return NULL.
	TolerantNew differs from New in that it will just return NULL if memory cannot
be allocated, rather than calling the out of memory handler.
	Unless a fixed size heap was specified in CreateHeap, these functions will attempt
to grow the heap by allocating more memory from the operating system, if there isn't
sufficient memory already reserved.
	It's assumed if multitasking (PF_REENTRANT), tasks will take responsibility for safe
sharing - i.e. if tasks ever share pointers to allocated memory, they will do their own
arbitration for access to it, and they will also ensure they never free memory which
is still being used by other tasks. */
#if HEAP_SANITY_CHECK_LEVEL >= 3
extern void *TrackedNew(size_t size, const char *file, int line);
extern void *TrackedTolerantNew(size_t size, const char *file, int line);
#define New(size) TrackedNew((size), __FILE__, __LINE__)
#define TolerantNew(size) TrackedTolerantNew((size), __FILE__, __LINE__)
#else
extern void *New(size_t size);
extern void *TolerantNew(size_t size);
#endif

/* Dispose of previously allocated memory. This may cause a chunk of memory to be
returned to the operating system. */
extern void Dispose(void *blockAddr);

/* Dispose of all allocated memory. No memory allocated via New and TolerantNew may
be used after this function has been called. The heap may be created again after this
call. */
extern void DisposeHeap(void);

/* Get the amount of available memory in bytes. Unless a fixed size heap was specified,
this is not a very meaningful concept, because more memory may always be requested from
the operating system. */
extern size_t HeapMemAvail(void);

/* Get the total amount of memory currently reserved by the heap. */
extern size_t TotalHeapSize(void);

/* Get a textual form of the compile-time heap options. */
extern const char *HeapOptionDescription(void);

/* Print information to stderr about memory usage. */
DIAGNOSTIC_FN_DECL(void PrintHeapStatus(void));

/* Run some simple sanity tests. */
DIAGNOSTIC_FN_DECL(void RunHeapTests(void));

/* Get a form of a pointer suitable for debugging output etc., which doesn't include the whole address. */
DIAGNOSTIC_FN_DECL(unsigned short PointerDisplayValue(const void *));

#endif /* BAS_HEAP_H_INCLUDED */
