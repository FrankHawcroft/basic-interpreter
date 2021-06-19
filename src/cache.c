/****** cache.c ******/

/*
	$VER: cache.c 0.16 (11.22.2014)
	
	Cache implementation.
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "common.h"
#include "heap.h"
#include "qstring.h"
#include "hashtable.h"
#include "cache.h"
#ifdef DEBUG
#include "platform.h" /* strdup */
#endif

struct Cache {
	struct HashTable *table;
	unsigned tableSize; /* Number of bins. */
	unsigned tableCapacity;
	void (*disposeValue)(void *);
	unsigned numEntries;
	unsigned maxEntries;
#ifdef DEBUG
	unsigned long hits;
	unsigned long misses;
#endif
};

/* Return a PRN in the range [0, max) - copied from K&R(2) */
static unsigned PseudoRandom(unsigned max)
{
	static unsigned long nextRand = 1;
	nextRand = 1103515245 * nextRand + 12345;
	return (unsigned)(nextRand / 65536) % max;
}

static struct HashTable *CreateTable(unsigned tableSize, void (*disposeValue)(void *))
{
	return HtCreate(tableSize, disposeValue, &QsEqual);
}

struct Cache *CreateCache(unsigned capacity, unsigned tableSize, void (*disposeValue)(void *))
{
	struct Cache *newCache = New(sizeof(struct Cache));
	
	assert(capacity != 0);
	assert(disposeValue != NULL);

	/* If tableSize is 0, allocate as many hash bins as the envisaged capacity of the cache,
	which may entail a large amount of memory being used.
		As a gesture at getting a reasonable modulus for hashes, ensure that at least there'll
	be an odd number of bins. */
	if(tableSize == 0)
		tableSize = 4 * capacity;
	tableSize = tableSize + (tableSize % 2 == 0);
	
	newCache->table = CreateTable(tableSize, disposeValue);
	newCache->tableSize = tableSize;
	newCache->tableCapacity = capacity;
	newCache->disposeValue = disposeValue;
	newCache->numEntries = 0;
	newCache->maxEntries = capacity;
#ifdef DEBUG
	newCache->hits = newCache->misses = 0;
#endif

	return newCache;
}

void ClearCache(struct Cache *cache)
{
	assert(cache != NULL);
	assert(cache->table != NULL);
	
	HtDispose(cache->table);
	cache->table = CreateTable(cache->tableSize, cache->disposeValue);
	cache->numEntries = 0;
}

void DisposeCache(struct Cache *cache)
{
	assert(cache != NULL);
	assert(cache->table != NULL);
	
	HtDispose(cache->table);
	Dispose(cache);
}

static void CreateKey(QString *actualKey, const void *rawKey)
{
	/* Initialise the string from the pointer's raw bytes, rather than printing it
		into a string (commented out below) - this avoids a memory allocation,
		at the expense of a lower quality hash. Only the low-order short word
		of the address is used - this works well for the interpeter, where the
		cache is used only to cache things in the program buffer, which for
		BASIC programs will almost always be less than 64K in size. */
	
	unsigned short intKey = (unsigned long)rawKey & USHRT_MAX;
	QsCopyData(actualKey, (const QsChar *)&intKey, sizeof(intKey) / sizeof(QsChar));
	
	/* Better quality key creation, but entails a memory allocation - */
	/*char ptrBuf[32];
	sprintf(ptrBuf, "%p", rawKey);
	QsCopyNTS(actualKey, ptrBuf);*/
}

struct KeyNSearchState
{
	unsigned counter, n;
	const QString *key;
};

#if HT_VISIT_INCLUDES_BIN_PARAM
static bool GetNthKeyVisitor(unsigned binIndex, const QString *key, const void *val, void *param)
#else
static bool GetNthKeyVisitor(const QString *key, const void *val, void *param)
#endif
{
	struct KeyNSearchState *searchState = (struct KeyNSearchState *)param;
	
	assert(param != NULL);
	
	if(searchState->counter++ >= searchState->n) {
		searchState->key = key;
		return FALSE;
	}
	else
		return TRUE;
}

static const QString *GetNthKey(struct HashTable *table, unsigned n)
{
	struct KeyNSearchState searchState;
	searchState.counter = 0;
	searchState.n = n;
	searchState.key = NULL;
	HtVisit(table, GetNthKeyVisitor, &searchState);
	
	assert(searchState.key != NULL);
	
	return searchState.key;
}

void SetInCache(struct Cache *cache, const void *key, void *value)
{
	QString formattedKey;
	void *existing;

	assert(cache != NULL && key != NULL && value != NULL);
	
	CreateKey(&formattedKey, key);
	existing = HtLookUp(cache->table, &formattedKey);
	
	if(existing == value) /* already in cache */
		return;
	else if(existing != NULL) {
		HtDelete(cache->table, &formattedKey);
		--cache->numEntries;
	}
	else if(cache->numEntries == cache->maxEntries) {
		HtDelete(cache->table, GetNthKey(cache->table, PseudoRandom(cache->numEntries)));
		--cache->numEntries;
	}
	
	HtAdd(cache->table, &formattedKey, value);
	++cache->numEntries;
	
	QsDispose(&formattedKey);
	
	assert(cache->numEntries <= cache->maxEntries);
}

void *RetrieveFromCache(struct Cache *cache, const void *key)
{
	void *cachedValue;
	QString formattedKey;
	
	assert(cache != NULL && key != NULL);
	
	CreateKey(&formattedKey, key);
	cachedValue = HtLookUp(cache->table, &formattedKey);
	QsDispose(&formattedKey);

#ifdef DEBUG
	cache->hits += cachedValue != NULL;
	cache->misses += cachedValue == NULL;
#endif

	return cachedValue;
}

#ifdef DEBUG

/* static void DisposeValue(void *v) { free(v); } */

void RunCacheTests(void)
{
	const char *k1 = "key one", *k2 = "key two", *k3 = "key three", *nonexistent = "nonexistent key";
	char *v1 = strdup("cached value one"), *v2 = strdup("cached value two"), *v3 = strdup("cached value three"), *v3new = strdup("updated value three");
	struct Cache *c = CreateCache(2, 0, free);
	
	fprintf(stderr, "-- Cache self-tests running ...\n");
	
	assert(RetrieveFromCache(c, k1) == NULL);
	
	SetInCache(c, k1, v1);
	SetInCache(c, k2, v2);
	
	assert(RetrieveFromCache(c, k1) == v1);
	assert(RetrieveFromCache(c, k2) == v2);
	
	SetInCache(c, k3, v3);
	
	assert(c->numEntries == 2);
	assert(RetrieveFromCache(c, k3) == v3);
	
	SetInCache(c, k3, v3new);
	
	assert(c->numEntries == 2);
	assert(RetrieveFromCache(c, k3) == v3new);
	
	assert(RetrieveFromCache(c, nonexistent) == NULL);
	
	DisposeCache(c);
	
	fprintf(stderr, "-- Cache self-tests finished.\n");
}

void PrintCacheInfo(const struct Cache *c)
{
	if (c != NULL)
		fprintf(stderr,
			"Num entries = %u, max entries = %u, hits = %lu, misses = %lu, hit ratio = %f%%\n",
			c->numEntries,
			c->maxEntries,
			c->hits,
			c->misses,
			c->hits == 0 && c->misses == 0 ? 0.0 : (c->hits / ((double)c->hits + c->misses)) * 100.0);
	else
		fprintf(stderr, "Cache not created.\n");
}

void DumpCache(const struct Cache *cache)
{
	if(cache != NULL) {
		fprintf(stderr, "-- Dumping cache %p\n", (void *)cache);
		HtDump(cache->table);
	}
}

#endif /* DEBUG */
