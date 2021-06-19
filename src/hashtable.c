/****** hashtable.c ******/

/*
	$VER: hashtable.c 0.16 (03.31.2014)

	Hash table implementation.
*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include "hashtable.h"
#include "heap.h"

struct HashTable {
	struct Definition **bin;
	unsigned numBins;
	unsigned long numEntries;
#ifdef DEBUG
	unsigned long maxEntries;
#endif
	void (*disposeValue)(void *);
	bool (*keyEquality)(const QString *, const QString *);
};

#define DEFAULT_BINS 1087

struct HashTable *HtCreate(
	unsigned numBins,
	void (*disposeValue)(void *),
	bool (*keyEquality)(const QString *, const QString *))
{
	struct HashTable *ht = New(sizeof(*ht));
	
	ht->numBins = numBins == 0 ? DEFAULT_BINS : numBins;
	ht->bin = New(sizeof(struct Definition *) * ht->numBins);
	memset(ht->bin, NUL, sizeof(struct Definition *) * ht->numBins);
	ht->disposeValue = disposeValue;
	ht->keyEquality = keyEquality == NULL ? &QsEqNoCase : keyEquality;
	ht->numEntries = 0;
#ifdef DEBUG
	ht->maxEntries = 0;
#endif
	
	return ht;
}

void HtAddPreallocated(struct HashTable *ht, struct Definition *defn)
{
	struct Definition **bin = &ht->bin[QsHash(&defn->key, ht->numBins)];

	assert(HtLookUp(ht, &defn->key) == NULL);
	
	defn->next = *bin;
	*bin = defn;
	
	++ht->numEntries;
#ifdef DEBUG
	if(ht->numEntries > ht->maxEntries)
		ht->maxEntries = ht->numEntries;
#endif
}

void *HtTryAdd(struct HashTable *ht, struct Definition *defn)
{
	struct Definition **bin, *pair;

	assert(ht != NULL);
	assert(defn != NULL);
	
	bin = &ht->bin[QsHash(&defn->key, ht->numBins)];
	for(pair = *bin; pair != NULL; pair = pair->next)
		if((*ht->keyEquality)(&pair->key, &defn->key))
			return pair->value;
			
	defn->next = *bin;
	*bin = defn;
	++ht->numEntries;	
	return defn->value;
}

void HtAdd(struct HashTable *ht, const QString *key, void *value)
{
	struct Definition *defn = New(sizeof(*defn));
	
	QsCopy(&defn->key, key);
	defn->value = value;
	HtAddPreallocated(ht, defn);
}

void *HtLookUp(const struct HashTable *ht, const QString *key)
{
	struct Definition *pair;
	
	assert(ht != NULL);
	assert(key != NULL);
	
	for(pair = ht->bin[QsHash(key, ht->numBins)]; pair != NULL; pair = pair->next)
		if((*ht->keyEquality)(&pair->key, key))
			return pair->value;
	return NULL;
}

void *HtVisit(const struct HashTable *ht, HtVisitor visit, void *param)
{
	struct Definition *pair;
	unsigned i;
	
	for(i = 0; i < ht->numBins; i++)
		for(pair = ht->bin[i]; pair != NULL; pair = pair->next)
#if HT_VISIT_INCLUDES_BIN_PARAM
			if(!visit(i, &pair->key, pair->value, param))
#else
			if(!visit(&pair->key, pair->value, param))
#endif
				return pair->value;
	return NULL;
}

static bool MatchAll(
#if HT_VISIT_INCLUDES_BIN_PARAM
		unsigned bin, 
#endif
		const QString *key, const void *val, void *param)
{
	return TRUE;
}

struct SingleDefinitionMatchingParams {
	const QString *key;
	bool (*keyEquality)(const QString *, const QString *);
};

static bool MatchOne(
#if HT_VISIT_INCLUDES_BIN_PARAM
		unsigned bin, 
#endif
		const QString *key, const void *val, void *param)
{
	struct SingleDefinitionMatchingParams *mp = (struct SingleDefinitionMatchingParams *)param;
	return (*mp->keyEquality)(key, mp->key);
}

static void HtDeleteMatching(struct HashTable *ht, HtVisitor matches, const void *param,
	unsigned fromBin, unsigned toBin /*, bool softDelete*/)
{
	struct Definition *pair, *savedNext, **prevNext;
	unsigned i;
	
	assert(fromBin <= toBin);
	assert(toBin < ht->numBins);
	
	for(i = fromBin; i <= toBin && ht->numEntries != 0; i++) {
		prevNext = &ht->bin[i];
		for(pair = ht->bin[i]; pair != NULL; pair = savedNext) {
			savedNext = pair->next;
#if HT_VISIT_INCLUDES_BIN_PARAM
			if(matches(i, &pair->key, pair->value, (void *)param))
#else
			if(matches(&pair->key, pair->value, (void *)param))
#endif
			{
				QsDispose(&pair->key);
				if(ht->disposeValue != NULL)
					(*ht->disposeValue)(pair->value);
				Dispose(pair);
				*prevNext = savedNext;
				
				--ht->numEntries;
			}
			else
				prevNext = &pair->next;
		}
	}
}

void HtDelete(struct HashTable *ht, const QString *key)
{
	unsigned bin = QsHash(key, ht->numBins);
	struct SingleDefinitionMatchingParams params;
	params.key = key;
	params.keyEquality = ht->keyEquality;
	HtDeleteMatching(ht, MatchOne, &params, bin, bin /*, FALSE*/);
}

void HtClear(struct HashTable *ht /*, bool softDelete*/)
{
	assert(ht->numBins != 0);

	if(ht->numEntries != 0)
		HtDeleteMatching(ht, MatchAll, NULL, 0, ht->numBins - 1 /* , softDelete*/);

	assert(ht->numEntries == 0);
}

void HtDispose(struct HashTable *ht)
{
	HtClear(ht);
	Dispose(ht->bin);
	Dispose(ht);
}

#ifdef DEBUG

#ifdef VBCC /* doesn't have strdup */
extern char *strdup(const char *);
#endif

static void SimpleDispose(void *val) { free(val); }

void HtRunTests(void)
{
	struct HashTable *ht = HtCreate(0, SimpleDispose, NULL);
	QString k1, k2, k3, nonexistent;
	char *v1 = strdup("value one"), *v2 = strdup("value two"), *v3 = strdup("value three");
	
	fprintf(stderr, "-- Hash table self-tests running ...\n");
	
	QsInitStaticNTS(&k1, "key one");
	QsInitStaticNTS(&k2, "key two");
	QsInitStaticNTS(&k3, "key three");
	QsInitStaticNTS(&nonexistent, "key not added");
	
	HtAdd(ht, &k1, v1);
	HtAdd(ht, &k2, v2);
	HtAdd(ht, &k3, v3);
	
	assert(HtLookUp(ht, &k1) != NULL && strcmp(HtLookUp(ht, &k1), v1) == 0);
	assert(HtLookUp(ht, &k2) != NULL && strcmp(HtLookUp(ht, &k2), v2) == 0);
	assert(HtLookUp(ht, &k3) != NULL && strcmp(HtLookUp(ht, &k3), v3) == 0);
	assert(HtLookUp(ht, &nonexistent) == NULL);
	
	HtDelete(ht, &k2);
	
	assert(HtLookUp(ht, &k2) == NULL);
	assert(HtLookUp(ht, &k1) != NULL && strcmp(HtLookUp(ht, &k1), v1) == 0);
	assert(HtLookUp(ht, &k3) != NULL && strcmp(HtLookUp(ht, &k3), v3) == 0);
	
	HtDump(ht);
	
	HtDispose(ht);
	
	fprintf(stderr, "-- Hash table self-tests finished.\n");
}

void HtDump(const struct HashTable *ht)
{
	unsigned n;

	fprintf(stderr, "-- Hash table dump - num bins = %u\n", ht->numBins);

	for(n = 0; n < ht->numBins; n++) {
		struct Definition *defn = ht->bin[n];
		if(defn != NULL) {
			fprintf(stderr, "%4u: ", n);
			for( ; defn != NULL; defn = defn->next) {
				/*QsWrite(&defn->key, stderr);*/
				size_t cn;
				for(cn = 0; cn != QsGetLength(&defn->key); cn++) {
					char c = QsGetCharAt(&defn->key, cn);
					isprint(c) ? fputc(c, stderr) : fprintf(stderr, "\\x%x", c);
				}
				fprintf(stderr, " => <%p>%s", defn->value, defn->next == NULL ? ".\n" : ", ");
			}
		}
	}
	
	fprintf(stderr, "-- End of hash table dump\n");
}

INLINE unsigned Factorial(unsigned n)
{
	unsigned fact = 1;
	while(n != 0)
		fact *= n--;
	return fact;
}

void HtGetLoadInfo(const struct HashTable *ht, unsigned *bins, unsigned *defns, unsigned *max, unsigned *collisions)
{
	unsigned n;

	*bins = ht->numBins;
	*defns = *collisions = 0;
	
	for(n = 0; n < ht->numBins; n++) {
		struct Definition *defn;
		unsigned defnsInBin = 0;
		for(defn = ht->bin[n]; defn != NULL; defn = defn->next)
			++defnsInBin;
		*defns += defnsInBin;
		if(defnsInBin != 0)
			*collisions += defnsInBin - 1;
	}
	
	assert(*defns == ht->numEntries);
	
	*max = ht->maxEntries;
}

#endif /* DEBUG */
