/****** hashtable.h ******/

/*
	$VER: hashtable.h 0.16 (03.31.2014)

	A hash table type - 'generic' (void *) for the values stored; keys must be QStrings.
*/

#ifndef BAS_HASHTABLE_H_INCLUDED
#define BAS_HASHTABLE_H_INCLUDED

#include <stdlib.h>
#include "common.h"
#include "qstring.h"

struct Definition {
	struct Definition *next;
	QString key;
	void *value;
};

/* HashTable is an opaque structure - */
struct HashTable;

extern struct HashTable *HtCreate(
	unsigned numBins,
	void (*disposeValue)(void *), 
	bool (*keyEquality)(const QString *, const QString *));

extern void HtClear(struct HashTable *);

extern void HtDispose(struct HashTable *);

extern void HtAdd(struct HashTable *, const QString *key, void *value);

extern void HtAddPreallocated(struct HashTable *, struct Definition *);

extern void *HtTryAdd(struct HashTable *, struct Definition *);

extern void HtDelete(struct HashTable *, const QString *key);

extern void *HtLookUp(const struct HashTable *, const QString *key);

#ifdef DEBUG
#define HT_VISIT_INCLUDES_BIN_PARAM 1
#else
#define HT_VISIT_INCLUDES_BIN_PARAM 0
#endif

#if HT_VISIT_INCLUDES_BIN_PARAM
typedef bool (*HtVisitor)(unsigned binIndex, const QString *key, const void *val, void *param);
#else
typedef bool (*HtVisitor)(const QString *key, const void *val, void *param);
#endif

/* visit must return true to indicate that traversal should continue, false if it should stop.
Last visited value is returned. */
extern void *HtVisit(const struct HashTable *, HtVisitor visit, void *param);
		
DIAGNOSTIC_FN_DECL(void HtRunTests(void));
DIAGNOSTIC_FN_DECL(void HtDump(const struct HashTable *));
DIAGNOSTIC_FN_DECL(void HtGetLoadInfo(const struct HashTable *, unsigned *bins, unsigned *defns, unsigned *max, unsigned *collisions));

#endif /* BAS_HASHTABLE_H_INCLUDED */
