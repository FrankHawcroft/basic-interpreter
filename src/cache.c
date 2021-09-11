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
#include "platform.h"

/* Choose whether to use a simple closed array for the cache rather than a hash table with open chaining.
	On older platforms like the Amiga, using the hash table structure - which means converting
keys (pointers) to strings - incurs quite a substantial performance overhead: about 15% of the
interpreter's time is spent retrieving things from the statement and untaken branch caches.
	The advantage of using the full hash table structure is that it handles hash collisions, so that
thrashing is never a problem unless the cache reaches its capacity. The direct mapped structure has
quicker access, but collisions simply result in eviction of the previous entry, so thrashing is more likely. */

#define DIRECT_MAPPED TRUE

#if DIRECT_MAPPED
struct CacheEntry {
	const void *key;
	void *value;
};
#endif

struct Cache {
#if DIRECT_MAPPED
	struct CacheEntry *table;
#else
	struct HashTable *table;
#endif
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

#if !DIRECT_MAPPED
static struct HashTable *CreateTable(unsigned tableSize, void (*disposeValue)(void *))
{
	return HtCreate(tableSize, disposeValue, &QsEqual);
}
#endif

struct Cache *CreateCache(unsigned capacity, unsigned tableSize, void (*disposeValue)(void *))
{
	struct Cache *newCache = New(sizeof(struct Cache));
	
	assert(capacity != 0);
	assert(disposeValue != NULL);

	/* As a gesture at getting a reasonable modulus for hashes, ensure that at least there'll
	be an odd number of bins. */
	if(tableSize == 0)
		tableSize = 2 * capacity;
	tableSize = tableSize + (tableSize % 2 == 0);
	
#if DIRECT_MAPPED
	newCache->table = New(sizeof(struct CacheEntry) * tableSize);
	{
		unsigned n;
		for(n = 0; n != tableSize; n++)
			newCache->table[n].key = newCache->table[n].value = NULL;
	}
#else
	newCache->table = CreateTable(tableSize, disposeValue);
#endif

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

#if DIRECT_MAPPED

void DisposeCache(struct Cache *cache)
{
	ClearCache(cache);
	Dispose(cache->table);
	Dispose(cache);
}

void ClearCache(struct Cache *cache)
{
	unsigned n;
	for(n = 0; n != cache->tableSize; n++) {
		if(cache->table[n].key != NULL) {
			cache->disposeValue(cache->table[n].value);
			cache->table[n].key = cache->table[n].value = NULL;
		}
	}
	cache->numEntries = 0;
}

#define Combine(h, x) ((((h) << 5) + (h)) ^ (x))

static struct CacheEntry *EntryForKey(struct Cache *cache, const void *key)
{
	/*const unsigned stride = 8;
	unsigned hash = ((intptr_t)key / stride) % cache->tableSize;*/
	unsigned short n = (unsigned short)((intptr_t)key & USHRT_MAX);
	unsigned short hash = Combine(Combine(5381, n & UCHAR_MAX), n >> CHAR_BIT);
	return &cache->table[hash % cache->tableSize];
}

void SetInCache(struct Cache *cache, const void *key, void *value)
{
	struct CacheEntry *candidate = EntryForKey(cache, key);
	
	if(candidate->key != NULL) /* evict, on LRU principle */
		cache->disposeValue(candidate->value);
	else if(cache->numEntries == cache->maxEntries) { /* drop another randomly */
		unsigned n;
		do
			n = PseudoRandom(cache->tableSize);
		while(cache->table[n].key == NULL);
		
		cache->disposeValue(cache->table[n].value);
		cache->table[n].key = cache->table[n].value = NULL;
	}
	else /* no conflict */
		++cache->numEntries;
	
	candidate->key = key;
	candidate->value = value;
}

void *RetrieveFromCache(struct Cache *cache, const void *key)
{
	struct CacheEntry *candidate = EntryForKey(cache, key);
#ifdef DEBUG
	cache->hits += candidate->key == key;
	cache->misses += candidate->key != key;
#endif
	return candidate->key == key ? candidate->value : NULL;
}

#else /* !DIRECT_MAPPED */

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

#endif /* !DIRECT_MAPPED */

#ifdef DEBUG

void RunCacheTests(void)
{
	const char *k1 = "key one", *k2 = "key two", *k3 = "key three", *nonexistent = "nonexistent key";
	char *v1 = strdup("cached value one"), *v2 = strdup("cached value two"), *v3 = strdup("cached value three"), *v3new = strdup("updated value three");
	struct Cache *c = CreateCache(2, 20, free);
	
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
	if(c != NULL)
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
#if DIRECT_MAPPED
		{
			unsigned n;
			for(n = 0; n != cache->tableSize; n++)
				if(cache->table[n].key != NULL)
					fprintf(stderr, "%4u: %p => %p\n", n, cache->table[n].key, cache->table[n].value);
		}
		fprintf(stderr, "-- End of cache dump\n");
#else
		HtDump(cache->table);
#endif
	}
}

#endif /* DEBUG */
