/****** heap.c ******/

/*
	$VER: heap.c 0.16A (5.13.2015)
	
	Memory management. The heap is very simple - it returns 'real' pointers,
	as opposed to indirect handles, and therefore does not provide features that
	this would allow, such as compaction. I.e. it works similarly to the Clib
	malloc()/free() model. 
		The heap size can optionally be fixed on creation, and various debugging
	and verification options can be switched on at compile time.
		Because the heap uses a bitmap to track free memory, and (by default) does
	a linear search when allocating, it does not scale to very large heaps -
	the bitmap is only effective when the heap is kilobytes or megabytes in size,
	rather than gigabytes, unless the allocation granularity is made wastefully large.
		The heap supports use by multiple tasks if PF_REENTRANT is defined.
*/

#include <limits.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "common.h"
#include "platform.h"
#include "heap.h"
#include "bitvector.h"

/*** Compile-time options ***/

/* Defining USE_SYSTEM_ALLOCATOR as true means the system memory management functions -
	e.g. malloc and free - will be substituted for the custom heap implementation. This 
	may give better performance on some systems. */

#define USE_SYSTEM_ALLOCATOR FALSE

/* Defining RANDOM_PROBE as true means a different allocation strategy is used. The default
	strategy is to do a linear search of each region for the first free block of sufficient size.
	RANDOM_PROBE attempts to satisfy the allocation at a randomly-chosen location in the region.
	If a number of unsuccessful probes occur, the next region is tried. This will often result in
	faster allocations, at the expense of more wasted memory. It may also result in failed
	allocations when using a fixed-size heap, where the linear strategy would succeed. */

#define RANDOM_PROBE FALSE

/* Defining SMALL_HEAP as true means that heap regions (and hence allocated objects)
	are limited to a smaller maximum size, but each memory allocation has a smaller
	overhead for its tracked length, and there may be slightly less allocation tracking
	overhead for small objects in the free map. SMALL_HEAP is only supported
	on platforms which don't require longword alignment. It trades off slightly greater
	memory efficiency (assuming small objects tend to be allocated more than larger ones)
	for slower allocation (because the bitmap is higher-resolution; i.e. some factor (e.g. 2)
	times as many bits need to be checked/set/cleared on each New() or Dispose()).
		Generally this should be set to FALSE, but the TRUE option may be useful on very 
	memory-constrained platforms.
		Affects: HeapAlignmentType, AllocationSizeType, and MAX_ALLOCATION. */
		
#define SMALL_HEAP FALSE

#if SMALL_HEAP && PF_REQUIRES_STRICT_ALIGNMENT
#error SMALL_HEAP is not supported on this system due to its alignment requirements.
#endif

/* Define as true to count the number of memory blocks of different sizes that are allocated. */

#define HISTOGRAM FALSE

#if USE_SYSTEM_ALLOCATOR && (RANDOM_PROBE || SMALL_HEAP || HISTOGRAM)
#error contradictory heap options
#endif

/*** SetOutOfMemoryHandler, DefaultOutOfMemoryHandler ***/

/* When New() is unable to allocate memory, an out of memory handler is called.
	The default handler prints a message and exits the program with a failure status.
	TolerantNew differs from New in that it returns NULL rather than calling the
	out of memory handler. */

static OutOfMemoryHandler m_OutOfMemoryHandler = NULL;

void SetOutOfMemoryHandler(OutOfMemoryHandler handler)
{
	m_OutOfMemoryHandler = handler;
}

void DefaultOutOfMemoryHandler(size_t sizeRequested)
{
	fprintf(stderr, "[Heap] error: out of memory - failed to allocate " SIZE_FMT " bytes - exiting.\n", sizeRequested);
	DisposeHeap();
	PfFinish();
	exit(EXIT_FAILURE);
}

/*** Wrapper for malloc, free ***/

#if USE_SYSTEM_ALLOCATOR

void CreateHeap(size_t initialSize, bool fixedSize) { }

#if HEAP_SANITY_CHECK_LEVEL >= 3
void *TrackedNew(size_t size, const char *file, int line)
#else
void *New(size_t size)
#endif
{
	void *mem = malloc(size);
	if(mem == NULL && m_OutOfMemoryHandler != NULL)
		(*m_OutOfMemoryHandler)(size);
	return mem;
}

#if HEAP_SANITY_CHECK_LEVEL >= 3
void *TrackedTolerantNew(size_t size, const char *file, int line) { return malloc(size); }
#else
void *TolerantNew(size_t size) { return malloc(size); }
#endif

void Dispose(void *blockAddr) { free(blockAddr); }

void DisposeHeap(void) { }

size_t HeapMemAvail(void) { return PfAvailMem(); }

size_t TotalHeapSize(void) { return 99999; }

#ifdef DEBUG
void PrintHeapStatus(void) { fprintf(stderr, "Using malloc, free.\n"); }

void RunHeapTests(void) { fprintf(stderr, "Using malloc, free.\n"); }
#endif

#else /* !USE_SYSTEM_ALLOCATOR */

/*** Header ***/

/* Per-allocation header, which also serves as the 'sub-atomic' unit from which allocation blocks are formed. */

/* Alignment type for allocation units. */
#if SMALL_HEAP
typedef short HeapAlignmentType;
#else
typedef PfAlignmentType HeapAlignmentType;
#endif

/* The type used to store allocation sizes. 
	AllocationSizeType should generally be the same size as HeapAlignmentType, or smaller;
	it is worth keeping as a separate type in case HeapAlignmentType is non-integral. */
#if SMALL_HEAP
typedef unsigned short AllocationSizeType;
#else
typedef unsigned long AllocationSizeType;
#endif

union Header {
	HeapAlignmentType dummyToEnsureAlignment;
	struct {
#if HEAP_SANITY_CHECK_LEVEL >= 2	
		unsigned long endGuard; /* Magic value guard used after allocated vector. */
#endif
		AllocationSizeType size;
#if HEAP_SANITY_CHECK_LEVEL >= 3
		/* Location of the call to New - */
		const char *file;
		int line;
#endif
#if HEAP_SANITY_CHECK_LEVEL >= 2	
		unsigned long startGuard; /* Magic value guard used before allocated vector. */
#endif
	} h;
	/* Usable allocated vector follows this structure in memory. */
};

#define HEADER_SIZE sizeof(union Header)

/* Size of a block in header units. */
#if HEAP_SANITY_CHECK_LEVEL >= 3
#define UNITS_PER_BLOCK 2
#elif HEAP_SANITY_CHECK_LEVEL >= 2
#define UNITS_PER_BLOCK 4
#else
#define UNITS_PER_BLOCK 12
#endif

/* Size of a block in bytes. */
#define BLOCK_SIZE (UNITS_PER_BLOCK * HEADER_SIZE)

#if HEAP_SANITY_CHECK_LEVEL >= 2
#define SANITY_CHECK_MAGIC_VALUE 0x890ABCDE
#endif

/*** HeapRegion ***/

/* The heap is divided into 'regions', 'pages', and 'blocks', in decreasing order of size.
	A region is a contiguous vector of memory obtained from the system, from which
allocations will be made.
	A page is a convenience unit, consisting of a group of blocks which it is efficient
to traverse when scanning the region's free map.
	A block is the most important division, and is the atomic allocation unit. The bitmap
recording free space in the heap operates at the block level - i.e. there is an overhead of
1 bit per BLOCK_SIZE bytes in the heap for free map accounting.
	On a multi-tasking system, regions are dedicated to particular tasks. */

struct HeapRegion {
	struct HeapRegion *nextRegion;
	union Header *memory;	/* The fixed-size piece of memory to allocate in. */
	Bits *map;				/* Bitmap of free space. Bit set means corresponding block is free. */
	unsigned long size;		/* In blocks. */
	unsigned long start;	/* 'Low water mark' for allocation scans - i.e. index before which it is 
								guaranteed that no '1' bits occur in the free map. */
	unsigned long freeBlocks; /* Total free blocks in this region. */
	unsigned long freePages; /* Completely free pages. */
	PfTaskIdentifier owner;
#if HEAP_SANITY_CHECK_LEVEL > 0
	unsigned long unmatchedAllocations; /* Number of calls to New() as-yet unmatched by a call to Dispose(). */
#endif
#ifdef DEBUG
	/* Statistical information: */
	unsigned long allocations; /* Total number of allocations made. */
	unsigned long searches; /* Number of times the region was probed, including failed attempts to allocate. */
#endif /* DEBUG */
};

/* Minimum allowed region size, in bytes. */
#define MIN_REGION_SIZE (BV_BITS_PER_WORD * BLOCK_SIZE)

/* Maximum allowed region size. */
#if SMALL_HEAP
#define MAX_REGION_SIZE ((SHRT_MAX - 1) * BLOCK_SIZE)
#else
#define MAX_REGION_SIZE SIZE_MAX
#endif

/*** Local functions and variables ***/

static struct HeapRegion *m_FirstRegion = NULL;
static PfMutex m_RegionListLock;
static bool m_FixedSizeHeap;

static struct HeapRegion *AddRegion(size_t);
static void DisposeRegion(struct HeapRegion *);
static void RemoveRegion(struct HeapRegion *);
static void RemoveAllOwnedRegions(void);
static union Header *AllocateFromRegion(struct HeapRegion *, unsigned long nBlocks);
static size_t NewRegionSize(size_t);

/*** Debugging messages ***/

/* Consistency checking is kept independent of DEBUG-level assertions so heap debugging
	can happen when the interpreter is otherwise in a 'release' build configuration. */
#if HEAP_SANITY_CHECK_LEVEL > 0
#define SANITY_CHECK(cond, msg) \
	if(!(cond)) { \
		fprintf(stderr, msg); \
		fprintf(stderr, "[Heap] this is a non-recoverable error: exiting.\n"); \
		exit(EXIT_FAILURE); }
#else
#define SANITY_CHECK(cond, msg)
#endif

/*** Allocation size statistics ***/

#if HISTOGRAM
#define MAX_POWER_OF_2 11 /* So max size tracked = 2048 bytes */
static unsigned long m_CountOfSize[MAX_POWER_OF_2 + 1];
static unsigned long m_BiggerCount;

static void CountAllocationSize(size_t);
static void PrintHistogram(void);
#endif /* HISTOGRAM */

/*** CreateHeap ***/

/* Allocates an initial heap region.

In: size -- in bytes.
	fixedSize -- controls whether further heap regions can be added. */

void CreateHeap(size_t size, bool fixedSize)
{
#if !PF_REENTRANT
	assert(m_FirstRegion == NULL);
#endif
	assert(size != 0);

	if(size > MAX_REGION_SIZE) {
		fprintf(stderr, "[Heap] error: initial size of " SIZE_FMT " is not allowed - exiting.\n", size);
		exit(EXIT_FAILURE);
	}
	
	if(size < MIN_REGION_SIZE)
		size = MIN_REGION_SIZE;

	if(m_FirstRegion == NULL) {
		SetOutOfMemoryHandler(&DefaultOutOfMemoryHandler);

#if PF_REENTRANT
		m_FixedSizeHeap = FALSE;
#else
		m_FixedSizeHeap = fixedSize;
#endif
		
		PfInitialiseMutex(&m_RegionListLock);
	}
	
	if(AddRegion(size) == NULL) {
		fprintf(stderr, "[Heap] error: failed to allocate initial " SIZE_FMT " bytes from system - exiting.\n", size);
		PfFinish();
		exit(EXIT_FAILURE);
	}
}

/*** AddRegion ***/

/* Adds a region of at least the given size in bytes, rounded up to a suitable actual size.
	Allocates a HeapRegion structure and memory for the region, and initialises
the free map. The new region is added at the start of the global list. */

static struct HeapRegion *AddRegion(size_t size)
{
	struct HeapRegion *newRegion;

#if !PF_REENTRANT
	assert(m_FirstRegion == NULL || !m_FixedSizeHeap);
#endif

	/* Convert size to a number of blocks which is also a multiple of BV_BITS_PER_WORD. */

	size = (size + BLOCK_SIZE - 1) / BLOCK_SIZE;
	size = ((size + BV_BITS_PER_WORD - 1) / BV_BITS_PER_WORD) * BV_BITS_PER_WORD;

	/* Allocate memory for region, heap, and bitmap. */

	newRegion = PfAllocMem(sizeof(struct HeapRegion));
	if(newRegion != NULL) {
		newRegion->memory = PfAllocMem(size * BLOCK_SIZE);
		newRegion->size = size;
	}
	if(newRegion != NULL && newRegion->memory != NULL)
		newRegion->map = NewBitVector(size, TRUE);
	
	if(newRegion == NULL || newRegion->memory == NULL || newRegion->map == NULL) {
		DisposeRegion(newRegion);	
		return NULL;
	}
	else {
		/* Initialise region fields. */
		
		newRegion->freeBlocks = size;
		newRegion->owner = PfGetCurrentTaskIdentifier();
		newRegion->freePages = size / BV_BITS_PER_WORD;
		newRegion->start = 0;
		
#if HEAP_SANITY_CHECK_LEVEL > 0
		newRegion->unmatchedAllocations = 0;
#endif
#ifdef DEBUG
		newRegion->allocations = newRegion->searches = 0;
#endif
				
		/* Add the region at the start of the list so it will be searched first. */

		PfBeginExclusiveExecution(&m_RegionListLock);
		newRegion->nextRegion = m_FirstRegion;
		m_FirstRegion = newRegion;
		PfEndExclusiveExecution(&m_RegionListLock);

		return newRegion;
	}
}

/*** DisposeRegion ***/

static void DisposeRegion(struct HeapRegion *region)
{
	/* Free memory, map, and structure itself. Silently tolerate a not-yet-fully constructed region - */
	
	if(region != NULL) {
		if(region->memory != NULL)
			PfFreeMem(region->memory, region->size * BLOCK_SIZE);
		if(region->map != NULL)
			DisposeBitVector(region->map, region->size);
		PfFreeMem(region, sizeof(struct HeapRegion));
	}
}

/*** RemoveRegion ***/

static void RemoveRegion(struct HeapRegion *region)
{
	struct HeapRegion **pred;

	PfBeginExclusiveExecution(&m_RegionListLock);

	/* Find predecessor and unlink - */
	
	for(pred = &m_FirstRegion; *pred != NULL && *pred != region; pred = &(*pred)->nextRegion)
		;
	
	assert(*pred != NULL);
	
	*pred = region->nextRegion;

	PfEndExclusiveExecution(&m_RegionListLock);

	/* Dispose - */
	
	DisposeRegion(region);
}

/*** RemoveAllOwnedRegions ***/

static void RemoveAllOwnedRegions(void)
{
	struct HeapRegion *region, *savedNext, **pred;

	PfBeginExclusiveExecution(&m_RegionListLock);
	
	for(region = m_FirstRegion, pred = &m_FirstRegion; region != NULL; region = savedNext) {
		savedNext = region->nextRegion;
		if(PfGetCurrentTaskIdentifier() == region->owner) {
			*pred = savedNext;
			DisposeRegion(region);
		}
		else
			pred = &region->nextRegion;
	}
	
	PfEndExclusiveExecution(&m_RegionListLock);
}

/*** New ***/

/* Allocates and returns a pointer to a block of memory 'count' bytes in size.
	If the memory cannot be allocated, the out of memory handler is called.
If this does not exit the program or make a non-local jump, NULL will be returned.
	The heap must have been created before calling this function. 
	Memory returned is NOT initialised! */

#if HEAP_SANITY_CHECK_LEVEL >= 3
void *TrackedNew(size_t count, const char *file, int line)
#else
void *New(size_t count)
#endif
{
	void *memory
#if HEAP_SANITY_CHECK_LEVEL >= 3
		= TrackedTolerantNew(count, file, line);
#else
		= TolerantNew(count);
#endif

	if(memory == NULL && m_OutOfMemoryHandler != NULL)
		m_OutOfMemoryHandler(count);

	return memory;
}

#if HEAP_SANITY_CHECK_LEVEL >= 4

static void FillWithRandomValues(void *memory, size_t count)
{
	size_t i;

	count *= sizeof(PfGranularType) / sizeof(unsigned char);
	for(i = 0; i < count; i++)
		((unsigned char *)memory)[i] = rand() % UCHAR_MAX;
}

#endif

/*** TolerantNew ***/

/* Allocates 'count' bytes of memory if possible, returning NULL if not. */

#if HEAP_SANITY_CHECK_LEVEL >= 3
void *TrackedTolerantNew(size_t count, const char *file, int line)
#else
void *TolerantNew(size_t count)
#endif
{
	struct HeapRegion *region;
	union Header *allocated = NULL;
	unsigned long nBlocks;

	assert(m_FirstRegion != NULL); /* Heap must have been created. */
	assert(count != 0);

#if HISTOGRAM
	CountAllocationSize(count);
#endif
	
	/* Convert object size desired to a number of atomic blocks, including space for the header - */

#if HEAP_SANITY_CHECK_LEVEL >= 2
#define TRAILER_SIZE HEADER_SIZE /* Used for the end guard word. */
#else
#define TRAILER_SIZE 0
#endif

	nBlocks = (count + HEADER_SIZE + TRAILER_SIZE + BLOCK_SIZE - 1) / BLOCK_SIZE;

	/* fprintf(stderr, SIZE_FMT " bytes -> %lu blocks\n", count, nBlocks); */
	
	for(region = m_FirstRegion;
		region != NULL && (allocated = AllocateFromRegion(region, nBlocks)) == NULL;
		region = region->nextRegion)
		;
		
	if(allocated == NULL && !m_FixedSizeHeap && (region = AddRegion(NewRegionSize(count))) != NULL)
		allocated = AllocateFromRegion(region, nBlocks);
	
	if(allocated == NULL)
		return NULL;

	/* Record allocation size. */

	allocated->h.size = (AllocationSizeType)nBlocks;

#if HEAP_SANITY_CHECK_LEVEL >= 3
	allocated->h.file = file;
	allocated->h.line = line;
#endif

#if HEAP_SANITY_CHECK_LEVEL >= 2
	allocated->h.startGuard = SANITY_CHECK_MAGIC_VALUE;
	allocated[nBlocks * UNITS_PER_BLOCK - 1].h.endGuard = SANITY_CHECK_MAGIC_VALUE;
		/* Unfortunately, because only the allocation size in blocks is stored, checking the end guard word is only
			useful for detecting 'bad' out of bounds bugs - not off-by-one-byte or other small transgressions.
			Storing the allocation size in bytes rather than blocks would address (no pun intended) this problem, 
			but would constrain	the maximum object size when using SMALL_HEAP. */
#endif

#if HEAP_SANITY_CHECK_LEVEL >= 4
	FillWithRandomValues(&allocated[1], count);
#endif

	return &allocated[1];
}

static union Header *AddressOf(const struct HeapRegion *region, long block)
{
	return &region->memory[block * UNITS_PER_BLOCK];
}

static long IndexOf(const struct HeapRegion *region, const union Header *h)
{
	return (h - region->memory) / UNITS_PER_BLOCK;
}

/*** AllocateFromRegion ***/

static union Header *AllocateFromRegion(struct HeapRegion *region, unsigned long nBlocks)
{
	long beginning = -1;

	/* Fail quickly if not the region used by this task, or definitely not enough space. */

	if(region->freeBlocks < nBlocks || region->owner != PfGetCurrentTaskIdentifier())
		return NULL;

	/* Search the free map. */

#if RANDOM_PROBE
	{
		long attempts = region->freeBlocks / nBlocks;
		
		beginning = -1;
	
		while(attempts-- > 0 && beginning < 0) {
			long p = rand() % region->size, contiguous = 0;
			
#ifdef DEBUG
			++region->searches;
#endif

			beginning = p;
			for( ; p < region->size && BitSet(region->map, p) && contiguous < nBlocks; p++, contiguous++)
				;
			
			if(contiguous < nBlocks)
				beginning = -1;
		}
	}
#else
	beginning = FindContiguousOnes(region->map, region->size, region->start, nBlocks);
#ifdef DEBUG
	++region->searches;
#endif
#endif /* !RANDOM_PROBE */

	/* Check for a failed search. */
	
	if(beginning < 0)
		return NULL;
	
	//fprintf(stderr, "++ Allocated at %ld, %lu blocks\n", beginning, nBlocks);
	
	/* Update the free map. */
	
	UpdateBitVector(region->map, beginning, nBlocks, FALSE);

	/* Update first possible location of free memory -
		(1) if allocated starting at (or before!) the current low water mark; or
		(2) if a single-block allocation. */

	if((unsigned long)beginning <= region->start + 1 || (!RANDOM_PROBE && nBlocks == 1))
		region->start = beginning + nBlocks;

	/* Deduct from free blocks. */
	
	region->freeBlocks -= nBlocks;
	
#ifdef DEBUG
	++region->allocations;
#endif
#if HEAP_SANITY_CHECK_LEVEL > 0
	++region->unmatchedAllocations;
#endif
		
	return AddressOf(region, beginning);
}

/*** NewRegionSize ***/

/* Calculate the size in bytes required for a new heap region. */

static size_t NewRegionSize(size_t requiredAllocation)
{
	size_t halfSize = TotalHeapSize() / 2;
	size_t scaled = halfSize > MAX_REGION_SIZE ? MAX_REGION_SIZE : halfSize; /* Increase heap size by 50% ... */
	size_t size = requiredAllocation > scaled ? requiredAllocation : scaled; /* ... unless more needed ... */
	if(size < MIN_REGION_SIZE)
		size = MIN_REGION_SIZE; /* ... or would be less than minimum ... */
	if(size > MAX_REGION_SIZE)
		size = MAX_REGION_SIZE; /* ... or greater than maximum. */
	return size; 
}

/*** TotalHeapSize ***/

size_t TotalHeapSize(void)
{
	struct HeapRegion *region;
	size_t total = 0;
	for(region = m_FirstRegion; region != NULL; region = region->nextRegion)
		total += region->size * BLOCK_SIZE;
	return total;
}

/*** Dispose ***/

/* Returns the memory at the address given to the pool available for allocation. 
If HEAP_SANITY_CHECK_LEVEL > 0, additional sanity checking is done on the address.

The containing region is removed and its memory returned to the system if/when all
its memory is disposed of, unless in multitasking mode, or it is the only region in
the list. */

void Dispose(void *addr)
{
	struct HeapRegion *region;
	long beginning;
	AllocationSizeType *length;
	union Header *header;
	
	/* Find the region containing the block; perform some sanity checking -
		that address is indeed in the heap, and refers to a block that might be valid. */

#if PF_REQUIRES_STRICT_ALIGNMENT
	SANITY_CHECK((intptr_t)addr % sizeof(HeapAlignmentType) == 0,
		"[Heap] Dispose - memory to free is not aligned.\n");
#endif

	PfBeginExclusiveExecution(&m_RegionListLock);
	for(region = m_FirstRegion;
		region != NULL
			&& (addr < (void *)region->memory || addr >= (void *)AddressOf(region, region->size)
				|| region->owner != PfGetCurrentTaskIdentifier());
		region = region->nextRegion)
		;
	PfEndExclusiveExecution(&m_RegionListLock);

	SANITY_CHECK(region != NULL,
		"[Heap] Dispose - attempt to free memory not allocated from heap, or freed twice.\n");

	/* If not found, and sanity checking suppressed, avoid crashing - */
	if(region == NULL) {
		fprintf(stderr, "[Heap] Dispose - ignoring attempt to free memory not allocated from heap, or freed twice.\n");
		return;
	}

	header = (union Header *)addr - 1;
	length = &header->h.size;
	beginning = IndexOf(region, header);

	SANITY_CHECK(region->unmatchedAllocations > 0,
		"[Heap] Dispose - region records no outstanding allocations (probably freed twice).\n");
	SANITY_CHECK(beginning >= 0,
		"[Heap] Dispose - block index < 0 (internal error!).\n");
	SANITY_CHECK(beginning < region->size,
		"[Heap] Dispose - block index too large (internal error!).\n");
		
#if HEAP_SANITY_CHECK_LEVEL >= 3
	if(*length <= 0)
		fprintf(stderr, "[Heap] Dispose - size = %ld blocks, allocated at %s:%d\n",
				header->h.size, header->h.file, header->h.line);
#endif
	SANITY_CHECK(*length > 0,
		"[Heap] Dispose - zero or negative block length (probably freed twice).\n");

	SANITY_CHECK(beginning + *length <= region->size,
		"[Heap] Dispose - block too large (probably out-of-bounds access overwrote allocated size).\n");
	SANITY_CHECK(region->freeBlocks + *length <= region->size,
		"[Heap] Dispose - too many free blocks in region (free twice or allocation size corrupted).\n");
#if HEAP_SANITY_CHECK_LEVEL >= 2
	SANITY_CHECK(header->h.startGuard == SANITY_CHECK_MAGIC_VALUE,
		"[Heap] Dispose - start guard word has been overwritten.\n");

#if HEAP_SANITY_CHECK_LEVEL >= 3
	if(header[*length * UNITS_PER_BLOCK - 1].h.endGuard != SANITY_CHECK_MAGIC_VALUE) {
		fprintf(stderr, "[Heap] Dispose, end guard word overwritten - size = %ld blocks, allocated at %s:%d\n",
				header->h.size, header->h.file, header->h.line);
	}
#endif
	
	SANITY_CHECK(header[*length * UNITS_PER_BLOCK - 1].h.endGuard == SANITY_CHECK_MAGIC_VALUE,
		"[Heap] Dispose - end guard word has been overwritten.\n");
#endif

	/* Update free map ... */

	UpdateBitVector(region->map, beginning, *length, TRUE);

	/* ... and count of free blocks. */
	
	region->freeBlocks += *length;
	
#if HEAP_SANITY_CHECK_LEVEL >= 4
	FillWithRandomValues(addr, *length * BLOCK_SIZE - HEADER_SIZE - TRAILER_SIZE);
#endif

	/* Zero length in the memory vector - helps to pick up 'free twice' errors. 
		Not done unless in sanity checking mode, as avoiding this access improves locality of reference. */

#if HEAP_SANITY_CHECK_LEVEL > 0
	*length = 0;
#endif

	/* Move 'start' back to this address if before current value. */

	if(beginning >= 0 && (unsigned long)beginning < region->start)
		region->start = beginning;
	
#if HEAP_SANITY_CHECK_LEVEL > 0
	--region->unmatchedAllocations;
#endif

	/* Delete the region if it's now all free, and safe to do so. */

#if !PF_REENTRANT
	if(region->freeBlocks == region->size
#if HEAP_SANITY_CHECK_LEVEL > 0
	&& region->unmatchedAllocations == 0
#endif /* HEAP_SANITY_CHECK_LEVEL */
	&& (region != m_FirstRegion || region->nextRegion != NULL))
		RemoveRegion(region);
#endif /* !PF_REENTRANT */
}

#if HEAP_SANITY_CHECK_LEVEL >= 3

/* TODO not comprehensive - only reports on first allocation in each
	contiguously allocated region - thorough reporting would require an
	auxiliary data structure. But suffices for the problem at hand. */
static void ReportOutstandingAllocations(const struct HeapRegion *region)
{
	bool prevFree = TRUE;
	long block;
	
	for(block = 0; block < region->size; block++) {
		if(!BitSet(region->map, block) && prevFree) {
			union Header *header = AddressOf(region, block);
			fprintf(stderr, "[Heap] size = %lu blocks, allocated at %s:%d\n",
				header->h.size, header->h.file, header->h.line);
		}
		prevFree = BitSet(region->map, block);
	}
}

#endif /* HEAP_SANITY_CHECK_LEVEL >= 3 */

/*** DisposeHeap ***/

/* Releases memory used for the heap. If in DEBUG mode, prints warning msg if 
calls to New() were more numerous than calls to Dispose(). However, this is 
not (always) a serious problem: many error conditions cause an abort without
balancing allocations and deallocations.

Post: all memory allocated from the system by the heap is returned. */

void DisposeHeap(void)
{
#if HEAP_SANITY_CHECK_LEVEL > 0
	struct HeapRegion *region;
	unsigned long totalUnmatchedAllocations = 0;
	
	for(region = m_FirstRegion; region != NULL; region = region->nextRegion) {
		totalUnmatchedAllocations += (PfGetCurrentTaskIdentifier() == region->owner ? region->unmatchedAllocations : 0);
#if HEAP_SANITY_CHECK_LEVEL >= 3
		if(region->unmatchedAllocations != 0)
			ReportOutstandingAllocations(region);
#endif
	}

	if(totalUnmatchedAllocations != 0)
		fprintf(stderr, "[Heap] warning - %lu call(s) to New were not matched by Dispose.\n", 
			totalUnmatchedAllocations);
#endif /* HEAP_SANITY_CHECK_LEVEL > 0 */

	RemoveAllOwnedRegions();
}

/*** HeapMemAvail ***/

/* Total number of bytes available in heap. (Regardless of fragmentation!) */

size_t HeapMemAvail(void)
{
	struct HeapRegion *region;
	size_t available = 0;
	for(region = m_FirstRegion; region != NULL; region = region->nextRegion)
		available += (PfGetCurrentTaskIdentifier() == region->owner ? (size_t)region->freeBlocks : 0);
	return available * BLOCK_SIZE;
}

/*** HeapOptionDescription ***/

#define EXPAND(x) #x
#define SCL(x) "HEAP_SANITY_CHECK_LEVEL==" EXPAND(x) "\n"

const char *HeapOptionDescription(void)
{
	return
#if USE_SYSTEM_ALLOCATOR
		"USE_SYSTEM_ALLOCATOR; "
#endif
#if PF_REENTRANT
		"PF_REENTRANT; "
#endif
#if SMALL_HEAP
		"SMALL_HEAP; "
#endif
#if HISTOGRAM
		"HISTOGRAM; "
#endif
		SCL(HEAP_SANITY_CHECK_LEVEL);
}

#ifdef DEBUG

/* A truncated version of the pointer which can be displayed in debug output etc. */
unsigned short PointerDisplayValue(const void *p) { return (unsigned short)((intptr_t)p & USHRT_MAX); }

/*** PrintHeapStatus ***/

/* Prints a 'free map' showing used and unused memory in each region, plus a
header giving the heap and block sizes, and some basic allocation stats. */

void PrintHeapStatus(void)
{
	struct HeapRegion *region;
	unsigned long totalAllocations = 0, unownedRegions = 0;

	fprintf(stderr, "Atomic blocks of " SIZE_FMT " bytes.\n"
		"Free maps (each character represents a 'page' of " SIZE_FMT " bytes):\n"
		"* = all allocated; + = partially allocated; - = all free.\n"
		"Most recently added region is listed first.\n", 
		BLOCK_SIZE, BLOCK_SIZE * (size_t)BV_BITS_PER_WORD);

	for(region = m_FirstRegion; region != NULL; region = region->nextRegion) {
		unsigned long block;
		int colCount = 1;
		
		if(PfGetCurrentTaskIdentifier() != region->owner) {
			++unownedRegions;
			continue;
		}
		
		fprintf(stderr, "%lu blocks in region ....%hX:\n", region->size, PointerDisplayValue(region));
		for(block = 0; block < region->size; block += BV_BITS_PER_WORD) {
			char code = '+'; /* Partially used. */
			if(BV_MapWord(region->map, block) == ~(Bits)0)
				code = '-';
			else if(BV_MapWord(region->map, block) == 0)
				code = '*';
			
			fputc(code, stderr);
			if(colCount++ % 80 == 0)
				fputc('\n', stderr);
		}
		fputc('\n', stderr);
		
		fprintf(stderr, "Start block is %lu.\n", region->start);
		/*fprintf(stderr, "%lu fragments.\n", region->fragments);*/
		fprintf(stderr, "%lu successful allocations made so far in this region.\n", region->allocations);
		fprintf(stderr, "%lu scans of this region.\n", region->searches);
		
		totalAllocations += region->allocations;
	}
	
	fprintf(stderr, "TOTAL: %lu successful allocations made so far.\n", totalAllocations);
	if(unownedRegions != 0)
		fprintf(stderr, "(%lu regions owned by other tasks not shown.)\n", unownedRegions);

#if HISTOGRAM
	PrintHistogram();
#endif
}

static void PrintRegionMap(const struct HeapRegion *region)
{
	unsigned long block;
	
	for(block = 0; block < region->size; block++)
		fputc(BitSet(region->map, block) ? '-' : '*', stderr);
	fputc('\n', stderr);
	/*fprintf(stderr, "%lu fragments.\n", region->fragments);*/
}

/*** Heap testbed ***/

/* TODO test a dynamic heap too */

static void CheckIntegrity(const char *p, char c, int n)
{
	while(n-- > 0)
		assert(*p++ == c);
}

void RunHeapTests(void)
{
	char *a, *b, *c, *d;
	size_t avail;

	/* fprintf(stderr, "-- Heap self-tests running ...\n"); */
	
#if PF_REENTRANT
	if(m_FirstRegion != NULL)
		return;
#endif

	/* 1. Create a small fixed-size heap. */
	
	CreateHeap(1024, TRUE);
#if !USE_SYSTEM_ALLOCATOR
	assert(HeapMemAvail() >= 1024);
#endif

	/* 2. Allocate a small object, then a large one, then another small one. */

#define A_LEN 5
#define B_LEN 900
#define C_LEN 11

	a = New(A_LEN);
	memset(a, 'a', A_LEN);
	
	/*PrintRegionMap(m_FirstRegion);*/
	
	b = New(B_LEN);
	memset(b, 'b', B_LEN);
	
	/*PrintRegionMap(m_FirstRegion);*/
	
	c = New(C_LEN);
	memset(c, 'c', C_LEN);
		
	/*PrintRegionMap(m_FirstRegion);*/
	
	CheckIntegrity(a, 'a', A_LEN);
	CheckIntegrity(b, 'b', B_LEN);
	CheckIntegrity(c, 'c', C_LEN);
	
	/*PrintHeapStatus();
	ReportOutstandingAllocations(m_FirstRegion);*/
	
	/* 3. Free the large object and assert that memory available has increased. */
	
	avail = HeapMemAvail();
	Dispose(b);
	
	/*PrintRegionMap(m_FirstRegion);*/
	
#if !USE_SYSTEM_ALLOCATOR
	assert(HeapMemAvail() > avail);
#endif

	CheckIntegrity(a, 'a', A_LEN);
	CheckIntegrity(c, 'c', C_LEN);
	
	/* 4. Allocate a medium-sized object - i.e. must be allocated in space
		vacated by object b, assuming a reasonable MIN_REGION_SIZE ... */
	
#define D_LEN 200

	/*PrintHeapStatus();*/

	d = New(D_LEN);
	memset(d, 'd', D_LEN);
		
	CheckIntegrity(a, 'a', A_LEN);
	CheckIntegrity(c, 'c', C_LEN);
	CheckIntegrity(d, 'd', D_LEN);
	
	/*PrintRegionMap(m_FirstRegion);
	PrintHeapStatus();
	ReportOutstandingAllocations(m_FirstRegion);*/
	
	/* 5. Free all the objects and assert no memory has leaked. */

	Dispose(c);
	
	CheckIntegrity(a, 'a', A_LEN);
	CheckIntegrity(d, 'd', D_LEN);

	Dispose(a);
		
	CheckIntegrity(d, 'd', D_LEN);
	
	Dispose(d);

#if !USE_SYSTEM_ALLOCATOR
	assert(HeapMemAvail() >= 1024);
#endif

	/* 6. Dispose of the heap. */

	DisposeHeap();
	
	/* fprintf(stderr, "-- Heap self-tests finished.\n"); */
}

#endif /* DEBUG */

#if HISTOGRAM

/*** CountAllocationSize ***/

static void CountAllocationSize(size_t size)
{
	int tableIndex;
	size_t val = 1;

	assert(size > 0);
	
	for(tableIndex = 0; tableIndex <= MAX_POWER_OF_2 && size > val; tableIndex++)
		val <<= 1;
	if(tableIndex <= MAX_POWER_OF_2)
		++m_CountOfSize[tableIndex];
	else
		++m_BiggerCount;
}

/*** PrintHistogram ***/

static void PrintHistogram(void)
{
	int tableIndex;
	size_t val = 1;

	fprintf(stderr, "Histogram:\n");
#if PF_REENTRANT
	fprintf(stderr, "(Global for all tasks!)\n");
#endif
	for(tableIndex = 0; tableIndex <= MAX_POWER_OF_2; tableIndex++, val <<= 1)
		fprintf(stderr, "<= %ld: %ld\n", val, m_CountOfSize[tableIndex]);
	fprintf(stderr, "> %d: %ld\n", 1 << MAX_POWER_OF_2, m_BiggerCount);
}

#endif /* HISTOGRAM */

#endif /* !USE_SYSTEM_ALLOCATOR */
