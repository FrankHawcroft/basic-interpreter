/****** bitvector.c ******/

/*
	$VER: bitvector.c 0.16 (03.11.2015)
	
	Bit vector implementation.
*/

#include <string.h>
#include "common.h"
#include "platform.h"
#include "bitvector.h"

#define NONE_SET ((Bits)0)
#define ALL_SET (~NONE_SET) 
#define TOP_SET ((Bits)1 << (BV_BITS_PER_WORD - 1))

Bits *NewBitVector(size_t size, bool value)
{
	Bits *v;
	
	assert(size != 0);
	
	v = PfAllocMem(BV_StorageSize(size));

	if(v != NULL)
		memset(v, value ? ~NUL : NUL, BV_StorageSize(size));
	
	return v;
}

void DisposeBitVector(Bits *v, size_t size)
{
	assert(v != NULL);
	
	PfFreeMem(v, BV_StorageSize(size));
}

/* Do a linear search of the vector for a sufficiently long block of 1s; first-fit. */
long FindContiguousOnes(Bits *v, long vectorLength, long start, long desiredLength)
{
	long contiguous = 0, beginning = -1, i;
	
	assert(v != NULL);
	assert(start >= 0);
	assert(desiredLength >= 1);
	
	for(i = start - start % BV_BITS_PER_WORD;
	  contiguous < desiredLength && i + desiredLength - contiguous < vectorLength;
	  i += BV_BITS_PER_WORD) {
		Bits word = BV_MapWord(v, i);

		if(word == ALL_SET) {
			if((contiguous += BV_BITS_PER_WORD) == BV_BITS_PER_WORD)
				beginning = i;
		}
		else if(word != NONE_SET && ((word & TOP_SET) || contiguous + BV_BITS_PER_WORD - 1 >= desiredLength)) {
			long j;
			
/*#if (TOP_SET >> 1) > TOP_SET
#define ASR_FOR_UNSIGNED TRUE
#else
#define ASR_FOR_UNSIGNED FALSE
#endif*/

			/* This loop kept as small as possible, to fit in cache on older processors. */
			for(j = 0;
/*#if ASR_FOR_UNSIGNED
				j < BV_BITS_PER_WORD &&
#endif*/
			  word != NONE_SET;
			  j++, word >>= 1) {
				  if(word & 1) {
					  if(++contiguous == 1)
						  beginning = i + j;
					  if(contiguous >= desiredLength)
						  break;
				  }
				  else
					  contiguous = 0;
			}
			
			if(j < BV_BITS_PER_WORD && contiguous < desiredLength)
				contiguous = 0;
		}
		else
			contiguous = 0;
	}
	
	return contiguous >= desiredLength ? beginning : -1;
}

void UpdateBitVector(Bits *v, long beginning, long count, bool value)
{
	long limit = beginning + count, scan;
	Bits wholeWord = value ? ALL_SET : NONE_SET;
	
	for(scan = beginning; scan < limit && scan % BV_BITS_PER_WORD != 0; scan++)
		value ? SetBit(v, scan) : ClearBit(v, scan);

	for( ; scan < limit - (limit % BV_BITS_PER_WORD == 0 ? 0 : BV_BITS_PER_WORD); scan += BV_BITS_PER_WORD)
		BV_MapWord(v, scan) = wholeWord;
	
	for( ; scan < limit; scan++)
		value ? SetBit(v, scan) : ClearBit(v, scan);
}

#ifdef DEBUG

#define TEST_VECTOR_SIZE 64

void RunBitVectorTests(void)
{
	Bits *v1, *v2;
	long idx;
	int i;
	
	fprintf(stderr, "-- Bit vector self-tests running ...\n");
	
	/* Create vectors - */
	v1 = NewBitVector(TEST_VECTOR_SIZE, TRUE);
	v2 = NewBitVector(TEST_VECTOR_SIZE, FALSE);
	
	/* BitSet */
	for(i = 0; i < TEST_VECTOR_SIZE; i++) {
		assert(BitSet(v1, i));
		assert(!BitSet(v2, i));
	}
	
	/* SetBit */
	SetBit(v2, 15);
	assert(BitSet(v2, 15));
	assert(!BitSet(v2, 14));
	assert(!BitSet(v2, 16));
	
	/* ClearBit */
	ClearBit(v2, 15);
	assert(!BitSet(v2, 15));
	assert(!BitSet(v2, 14));
	assert(!BitSet(v2, 16));
	
	/* UpdateBitVector */
	UpdateBitVector(v2, 21, 6, TRUE);
	
	for(i = 21; i < 21 + 6; i++)
		assert(BitSet(v2, i));
	assert(!BitSet(v2, 20));
	assert(!BitSet(v2, 27));
	
	/* FindContiguousOnes */
	idx = FindContiguousOnes(v2, TEST_VECTOR_SIZE, 0, 6);
	assert(idx == 21);
	
	/* Clean up */
	DisposeBitVector(v1, TEST_VECTOR_SIZE);
	DisposeBitVector(v2, TEST_VECTOR_SIZE);
	
	fprintf(stderr, "-- Bit vector self-tests finished.\n");
}

#endif /* DEBUG */
