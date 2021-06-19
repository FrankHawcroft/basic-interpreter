/****** bitvector.h ******/

/*
	$VER: bitvector.h 0.16 (03.11.2015)
	
	A simple bit vector type.
*/

#ifndef BITVECTOR_H_INCLUDED
#define BITVECTOR_H_INCLUDED

#include <limits.h>

typedef unsigned long Bits;

/* Allocate a new vector of 'size' bits all set to 1 or 0. Must be freed using DisposeBitVector. */
extern Bits *NewBitVector(size_t size, bool value);

/* Free the vector's memory. The size (again in bits) is required on some platforms,
	so should always be supplied. */
extern void DisposeBitVector(Bits *, size_t size);

/* Search for the first sequence of 1s of at least the desired length, starting at the given offset.
	Returns -1 if not present. */
extern long FindContiguousOnes(Bits *v, long vectorLength, long start, long desiredLength);

/* Set a region of a vector to 1s or 0s. No bounds checking! */
extern void UpdateBitVector(Bits *v, long beginning, long count, bool value);

/* These help avoid the perils of mixing signed and unsigned arithmetic. */
#ifdef LONG_BIT
#define BV_BITS_PER_WORD ((long)LONG_BIT)
#else
#define BV_BITS_PER_WORD ((long)(sizeof(Bits) * CHAR_BIT))
#endif

#define BV_StorageSize(nbits) (sizeof(Bits) * ((nbits) + BV_BITS_PER_WORD - 1) / BV_BITS_PER_WORD)

/* Kept as macros to improve performance with older compilers. No bounds checking! */
#define BV_Index(n) ((n) / BV_BITS_PER_WORD)
#define BV_Mask(n) (1L << ((n) % BV_BITS_PER_WORD))
#define BV_MapWord(bv, n) ((bv)[BV_Index(n)])

/* Test a bit. */
#define BitSet(bv, n) ((BV_MapWord(bv, n) & BV_Mask(n)) != 0)

/* Set a bit. */
#define SetBit(bv, n) (BV_MapWord(bv, n) |= BV_Mask(n))

/* Clear a bit. */
#define ClearBit(bv, n) (BV_MapWord(bv, n) &= ~BV_Mask(n))

#ifdef DEBUG
extern void RunBitVectorTests(void);
#endif

#endif /* BITVECTOR_H_INCLUDED */
