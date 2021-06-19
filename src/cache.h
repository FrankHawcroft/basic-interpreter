/****** cache.h ******/

/*
	$VER: cache.h 0.16A (5.12.2015)

	A simple generic cache.
*/

#ifndef BAS_CACHE_H_INCLUDED
#define BAS_CACHE_H_INCLUDED

/* Cache is an opaque type - */
struct Cache;

extern struct Cache *CreateCache(unsigned capacity, unsigned tableSize, void (*disposeValue)(void *));
extern void DisposeCache(struct Cache *);
extern void ClearCache(struct Cache *);
extern void SetInCache(struct Cache *, const void *key, void *value);
extern void *RetrieveFromCache(struct Cache *, const void *key);
DIAGNOSTIC_FN_DECL(void RunCacheTests(void));
DIAGNOSTIC_FN_DECL(void PrintCacheInfo(const struct Cache *));
DIAGNOSTIC_FN_DECL(void DumpCache(const struct Cache *));

#endif /* BAS_CACHE_H_INCLUDED */
