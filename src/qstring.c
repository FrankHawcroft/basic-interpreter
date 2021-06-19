/****** qstring.c ******/

/*
	$VER: qstring.c 0.16A (5.5.2015)

	'Quick string' implementation.
*/

#include "common.h"
#include "platform.h"
#include "heap.h"
#include "qstring.h"
#include "sign.h"

/****** Wide-character support ******/

#if QSTRING_USE_WCHAR

/* <wchar.h> is included by qstring.h */
#include <wctype.h>

#define CStrLen wcslen
#define CMemCpy wmemcpy
#define CMemSet wmemset
#define CMemCmp wmemcmp
#define CToUpper towupper
#define CFPutC fputwc
#define CEOF WEOF

#else

#include <string.h>
#include <ctype.h>

#define CStrLen strlen
#define CMemCpy memcpy
#define CMemSet memset
#define CMemCmp memcmp
#define CToUpper toupper
#define CFPutC fputc
#define CEOF EOF

#endif /* !QSTRING_USE_WCHAR */

/*** Constants, configurable limits and compile-time options ***/

/* Strings shorter than this length have their data stored _in_ the bytes of the
'data' member of the QString structure, rather than this necessarily being a
pointer to the string data as its declaration indicates. */

#define SHORT_STRING_LENGTH	((QsInternalLength)(sizeof(QsChar *) / sizeof(QsChar)))

/* When a non-static string needs storage allocated from the heap, if it is
less than this length, twice its actual length will be allocated. There are
two reasons why this might improve performance:

1. If it is common for small strings to have other small strings appended to them, 
in which case the overhead of another heap allocation may be saved. 

2. If the heap doesn't perform particularly well with very small allocations.

Unlike SHORT_STRING_LENGTH, this can be an arbitrary value up to QS_MAX_LENGTH / 2. */

#define MEDIUM_STRING_LENGTH 24

/* Maximum reference count value. */
	
#define MAX_REF_COUNT USHRT_MAX

/* Define this symbol to use a less-thorough, but quicker, hashing method in QsHash. */

#define FAST_HASH TRUE

/* Define this symbol true to use addition when hashing, rather than XOR. */
	
#define HASH_ADDS FALSE

/* Define this symbol true if your platform has a very fast memcmp implementation, to use
	it by preference when testing strings for equality. */
	
#define FAST_MEMCMP TRUE

/*** QStrData ***/

/* The header for reference-counted string data stored on the heap.
	If a QString structure has a negative length less than 
-SHORT_STRING_LENGTH, then the 'data' pointer actually points to one of these 
structures. The string data immediately follow this structure in memory. */

struct QStrData {
	unsigned short refCount; /* Reference count. */
	QsInternalLength actualLength; /* Length of following data, in characters. */
	QsChar data; /* First character of string data. */
	/* Rest of string follows this structure. */
};

/*** Module-private declarations ***/

static void QsCopyForWrite(QString *, size_t length);
static struct QStrData *QsAllocate(size_t);
static const QsChar *FindOneOf(const QsChar *start, QsInternalLength length, const QString *find);
static const QsChar *FindChar(const QsChar *start, QsInternalLength length, QsChar ch);
static unsigned long HashFor(const QsChar *, size_t);

#ifdef DEBUG
/* Memory use statistics. */

static unsigned long m_Copies = 0;	/* # string copies */
static unsigned long m_Allocations = 0;	/* # mem allocations */
static unsigned long m_Comparisons = 0; /* # comparisons */
static unsigned long m_Hashes = 0; /* # hash calculations */
#endif /* DEBUG */

/****** Inline module-private functions ******/

/* Defined early in the module in an attempt to assist older compilers with inlining. */

/*** QsSame ***/

/* Returns TRUE if two strings are identical; that is, they point to the
same data, or are short strings containing the same characters. In the
case of short strings, the unused bytes of the pointer field could contain
garbage. In this case, the function will only return TRUE if that garbage
is the same. For exact copies, this will hold, which is the important
thing. It will fail if two short strings were constructed differently;
but in that case, if the data was on the heap, the function would return
FALSE anyway. */

#define QsSame(s1, s2) ((s1)->data == (s2)->data)

/*** QsTriviallyEqual ***/

/* TRUE if the two strings can be considered equal in terms of their content
with a trivial, i.e. O(1), comparison. */

#define QsTriviallyEqual(s1, s2) ((s1) == (s2) || (QsIsNull(s1) && QsIsNull(s2)) || QsSame(s1, s2))

/*** QsTriviallyUnequal ***/

/* Conversely, TRUE if the two strings are definitely not equivalent. */

#if FAST_HASH || QSTRING_CACHE_HASH
#define QsTriviallyUnequal(s1, s2) (QsGetLength(s1) != QsGetLength(s2) || (QsGetLength(s1) > 40 && QsHash(s1, 0) != QsHash(s2, 0)))
#else
#define QsTriviallyUnequal(s1, s2) (QsGetLength(s1) != QsGetLength(s2))
#endif

#if QSTRING_CACHE_HASH
#define StoreHash(s, data, len) ((s)->hash = (unsigned short)HashFor((data), (len)))
#else
#define StoreHash(s, data, len)
#endif

/*** QsInitIntern ***/

INLINE void QsInitIntern(QString *s, struct QStrData *qsd, size_t len)
{
	assert(s != NULL);
	assert(len <= QS_MAX_LENGTH);

	s->data = (QsChar *)qsd;
	s->length = -(QsInternalLength)len;
#if QSTRING_CACHE_HASH
	s->hash = 0;
#endif
}

/*** QsAccess ***/

INLINE struct QStrData *QsAccess(const QString *s)
{
	assert(s->length < -SHORT_STRING_LENGTH);

	return (struct QStrData *)s->data;
}

/*** QsBuffer ***/

INLINE QsChar *QsBuffer(const QString *s)
{
	assert(s->length < 0);

	return s->length >= -SHORT_STRING_LENGTH ? (QsChar *)&s->data : &(QsAccess(s)->data);
}

/****** Public QString functions ******/

/*** QsInitNull ***/

QString *QsInitNull(QString *s)
{
	assert(s != NULL);
	
	s->data = NULL;
	s->length = 0;
	
	return s;
}

/*** QsInitStatic ***/

QString *QsInitStatic(QString *s, const QsChar *ptr, size_t len)
{
	assert(s != NULL);
	assert(len <= QS_MAX_LENGTH);
	assert(ptr != NULL || len == 0);
	
	s->data = (QsChar *)ptr;
	s->length = (QsInternalLength)len;
	StoreHash(s, ptr, len);

	return s;
}

/*** QsInitStaticPtrs ***/

QString *QsInitStaticPtrs(QString *s, const QsChar *start, const QsChar *end)
{
	assert(start != NULL && end != NULL);
	assert(end - start >= 0);
	
	return QsInitStatic(s, start, end - start + 1);
}

/*** QsInitStaticNTS ***/

/* Create a QString from a null-terminated string which is not going to change or go away. */

QString *QsInitStaticNTS(QString *s, const QsChar *cs)
{
	return QsInitStatic(s, cs, CStrLen(cs));
}

/*** QsCopy ***/

/* The 'to' string is assumed to be uninitialised. */

QString *QsCopy(QString *to, const QString *from)
{
	assert(to != NULL);
	assert(from != NULL);

	if(from->length >= -SHORT_STRING_LENGTH) /* short or static */
		*to = *from;
	else {
		struct QStrData *qsd = QsAccess(from);
		
		/*if(qsd->refCount == 0) {
			fprintf(stderr, "0 ref count for ");
			QsWrite(from, stderr);
		}*/
			
		assert(qsd->refCount != 0);
		
		if(qsd->refCount >= MAX_REF_COUNT) /* have to actually copy */
			QsCopyData(to, QsGetData(from), QsGetLength(from));
		else {
			++qsd->refCount;
			*to = *from;
		}
	}
	
#ifdef DEBUG
	++m_Copies;
#endif

	return to;
}

/*** QsJoin ***/

/* Appends 'suffix' to 'prefix' and puts the resulting string in 'result'.
Unlike QsCopy(), the caller must ensure that 'result' is a valid string. 
Returns FALSE and leaves result unchanged if the resulting string would
be illegally long. */

bool QsJoin(QString *result, const QString *prefix, const QString *suffix)
{
	size_t prefixLength = QsGetLength(prefix);
	size_t suffixLength = QsGetLength(suffix);
	size_t resultLength = prefixLength + suffixLength;
	bool appending = QsSame(result, prefix);

	assert(resultLength >= QsGetLength(result));
	
	/* Check for overflow of size_t or limit exceeded - */
	if(resultLength < prefixLength || resultLength < suffixLength || resultLength > QS_MAX_LENGTH)
		return FALSE;

	QsCopyForWrite(result, resultLength);
	if(!appending && prefixLength != 0)
		CMemCpy(QsBuffer(result), QsGetData(prefix), prefixLength);
	if(suffixLength > 0)
		CMemCpy(QsBuffer(result) + prefixLength, QsGetData(suffix), suffixLength);

	StoreHash(result, QsGetData(result), QsGetLength(result));

	return TRUE;
}

/* Joins 'suffix' to 'prefix' and leaves result in 'prefix'. */
bool QsAppend(QString *prefix, const QString *suffix)
{
	return QsJoin(prefix, prefix, suffix);
}

/*** QsToUpperCase ***/

/* Converts all the characters in the string into uppercase.  The source string
is unchanged; the destination string is assumed to be uninitialised when the
function is called, and will contain the uppercase string afterwards. */

QString *QsToUpperCase(QString *ucase, const QString *s)
{
	size_t length = QsGetLength(s);

	QsInitNull(ucase);
	QsCopyForWrite(ucase, length);

	if(length != 0) {
		size_t n;
		QsChar *up = QsBuffer(ucase);
		const QsChar *sp = QsGetData(s);

		for(n = 0; n < length; n++)
			*up++ = (QsChar)CToUpper(*sp++);
	}

	StoreHash(ucase, QsGetData(ucase), QsGetLength(ucase));

	return ucase;
}

/*** QsAppendChar ***/

/* Appends a single character to the string. */

QString *QsAppendChar(QString *s, QsChar ch)
{
	size_t newLength;

	assert(QsGetLength(s) < QS_MAX_LENGTH);

	newLength = QsGetLength(s) + 1;
	QsCopyForWrite(s, newLength);
	*(QsBuffer(s) + newLength - 1) = ch;

	StoreHash(s, QsGetData(s), QsGetLength(s));
	
	return s;
}

/*** QsSetCharAt ***/

/* Sets the character at the given index in the string.  Calling this function
on a static string results in the string becoming dynamic (i.e. copying its data). */

QString *QsSetCharAt(QString *s, size_t index, QsChar ch)
{
	assert(index < QsGetLength(s));
	
	QsCopyForWrite(s, QsGetLength(s));
	*(QsBuffer(s) + index) = ch;
	
	StoreHash(s, QsGetData(s), QsGetLength(s));

	return s;
}

/*** QsCopyChar ***/

/* Create a qstring from a single character. */

QString *QsCopyChar(QString *s, QsChar ch)
{
	QsInitNull(s);
	QsCopyForWrite(s, 1);
	*(QsBuffer(s)) = ch;

	StoreHash(s, QsGetData(s), QsGetLength(s));

	return s;
}

/*** QsCopyData ***/

/* Create a QString from character data. */

QString *QsCopyData(QString *s, const QsChar *data, size_t length)
{
	assert(data != NULL && length <= QS_MAX_LENGTH);
	
	QsInitNull(s);
	QsCopyForWrite(s, length);
	if(length > 0)
		CMemCpy(QsBuffer(s), data, length);

	StoreHash(s, QsGetData(s), QsGetLength(s));

	return s;
}

/*** QsCopyNTS ***/

/* Create a QString from a null-terminated string. */

QString *QsCopyNTS(QString *s, const QsChar *cs)
{
	return QsCopyData(s, cs, CStrLen(cs));
}

/*** QsRepeat ***/

/* Create a qstring consisting of a given character repeated a number of times. */

QString *QsRepeat(QString *s, size_t count, QsChar ch)
{
	assert(count <= QS_MAX_LENGTH);

	QsInitNull(s);
	QsCopyForWrite(s, count);
	if(count > 0)
		CMemSet(QsBuffer(s), ch, count);

	StoreHash(s, QsGetData(s), QsGetLength(s));

	return s;
}

/*** QsDispose ***/

/* Dispose of a qstring. For every qstring created that isn't known to only contain
static data, this function must be called to release any allocated memory. */

void QsDispose(QString *s)
{
	assert(s != NULL);

	if(s->length < -SHORT_STRING_LENGTH) {
		struct QStrData *qsd = QsAccess(s);
		assert(qsd->refCount != 0);
		if(--qsd->refCount == 0)
			Dispose(qsd);
	}
	QsInitNull(s);
}

/*** QsGetData ***/

const QsChar *QsGetData(const QString *s)
{
	assert(s != NULL);
	
	return s->length >= 0 ? s->data : QsBuffer(s);
}

/*** QsGetCharAt ***/

QsChar QsGetCharAt(const QString *s, size_t index)
{
	assert(!QsIsNull(s) && index < QsGetLength(s));
	
	return *(QsGetData(s) + index);
}

/*** QsGetLast ***/

QsChar QsGetLast(const QString *s)
{
	assert(!QsIsNull(s));

	return QsGetCharAt(s, QsGetLength(s) - 1);
}

/*** QsCopyToBuffer ***/

/* Converts a qstring into a C-style null-terminated string, up to a given length. */

void QsCopyToBuffer(const QString *s, QsChar *buffer, size_t bufferLength)
{
	size_t count;

	assert(buffer != NULL && bufferLength >= 1);

	count = QsGetLength(s) + 1 < bufferLength ? QsGetLength(s) : bufferLength - 1;
	
	if(count != 0)
		CMemCpy(buffer, QsGetData(s), count);
	buffer[count] = NUL;
}

/*** QsDupAsNTS ***/

/* Ala strdup() - returns a dynamically allocated C-style string containing a copy of the QString. */

QsChar *QsDupAsNTS(const QString *s)
{
	QsChar *nts = New((QsGetLength(s) + 1) * sizeof(QsChar));
	QsCopyToBuffer(s, nts, QsGetLength(s) + 1);

	return nts;
}

/*** QsWrite ***/

/* Prints the string to the given output stream. Returns true on success, false on error. */

bool QsWrite(const QString *s, FILE *stream)
{
	size_t len = QsGetLength(s), i;
	const QsChar *d = QsGetData(s);
	bool success = TRUE;

	for(i = 0; i != len && success; i++)
		success = CFPutC(d[i], stream) != CEOF;

	return success;
}

/*** QsTokenise ***/

/* This is a simple 'all at once' tokenisation function.  If successful, it
returns the number of tokens produced.  If there isn't enough space in the
tokens array, it returns -1.  Memory _is not allocated_ for the tokens
produced: they simply point into the source string's buffer. */

int QsTokenise(const QString *source, QString *token, size_t maxTokens, const QString *separator)
{
	int curToken = 0;
	const QsChar *first = NULL, *end = NULL;
	const QsChar *start = QsGetData(source);
	QsInternalLength length = (QsInternalLength)QsGetLength(source);

	assert(source != NULL && token != NULL && separator != NULL);
	assert(maxTokens != 0);

	first = start;
	end = FindOneOf(start, length, separator);
	while(end != NULL) {
		if((size_t)curToken >= maxTokens)
			return -1;

		if(first < end) {
			QsInitStaticPtrs(&token[curToken], first, end - 1);
			start = end;
			length -= (QsInternalLength)QsGetLength(&token[curToken]);
			++curToken;
		}
		else {
			++start;
			--length;
		}

		first = start;
		end = FindOneOf(start, length, separator);
	}

	/* 'Left over' characters at end. */

	if(length > 0) {
		if((size_t)curToken >= maxTokens)
			return -1;
		QsInitStatic(&token[curToken], start, length);
		++curToken;
	}

	return curToken;
}

/*** QsGetSubstring ***/

QString *QsGetSubstring(QString *result, const QString *s, size_t start, size_t count)
{
	size_t length = QsGetLength(s);

	assert(start < length || (start == 0 && length == 0));
	assert(count <= QS_MAX_LENGTH);

	/* It's allowed to have a count which extends beyond the end of the string
		- correct for this: */
	if(start + count > length)
		count = length - start;

	if(s->length >= 0) {
		/* 'Static' data, so can just point at the same data. */
		result->data = count == 0 ? NULL : s->data + start;
		result->length = (QsInternalLength)count;
		StoreHash(result, QsGetData(result), QsGetLength(result));
	}
	else if(start == 0 && count == length)
		/* The whole string -- becomes just a ref count increase. */
		QsCopy(result, s);
	else {
		/* Need to allocate string and copy substring into it. */
		QsInitNull(result);
		QsCopyForWrite(result, count);
		if(count > 0)
			CMemCpy(QsBuffer(result), QsGetData(s) + start, count);
		StoreHash(result, QsGetData(result), QsGetLength(result));
	}
	
	return result;
}

/*** QsCompare ***/

/* Compare the strings, returning a value as for the standard library function strcmp(). */

int QsCompare(const QString *s1, const QString *s2)
{
	size_t length1 = QsGetLength(s1), length2 = QsGetLength(s2);
	size_t compareLength = length1 < length2 ? length1 : length2;
	/* memcmp is used here because QStrings can contain embedded NULs.
		TODO the problem with this is that it is insensitive to ordering conventions that may
		apply in different character sets. */
	int compareResult = compareLength == 0 ? 0 : CMemCmp(QsGetData(s1), QsGetData(s2), compareLength);
#ifdef DEBUG
	++m_Comparisons;
#endif
	return compareResult != 0 ? compareResult : Sign((QsInternalLength)length1 - (QsInternalLength)length2);
}

/*** QsEqual ***/

/* An often-quicker special-case comparison: true if the strings' content is equal, false if not. */

bool QsEqual(const QString *s1, const QString *s2)
{
#ifdef DEBUG
	++m_Comparisons;
#endif
	return !QsTriviallyUnequal(s1, s2)
		&& (QsTriviallyEqual(s1, s2) || QsCompare(s1, s2) == 0);
}

/*** QsEqNoCase ***/

/* Another optimised special-case comparison function.
   Returns TRUE iff the strings are equal. Upper- and lowercase letters are treated the same. 
   Like QsCompare() and QsEqual(), this function allows the strings to contain NUL characters. */

bool QsEqNoCase(const QString *s1, const QString *s2)
{
	size_t l1 = QsGetLength(s1);
#ifdef DEBUG
	++m_Comparisons;
#endif
	if(l1 != QsGetLength(s2))
		return FALSE;
#if FAST_MEMCMP
	else if(CMemCmp(QsGetData(s1), QsGetData(s2), l1) == 0)
		return TRUE;
#endif
	else {
		const QsChar *c1 = QsGetData(s1), *c2 = QsGetData(s2);
		for( ; l1 != 0 && CToUpper(*c1) == CToUpper(*c2); l1--, c1++, c2++)
			;
		return l1 == 0;
	}
}

/*** QsSearch, QsSearchNoCase ***/

/* Searches 'text' for 'key', starting at the given 0-origin 'startPosition'.
	Returns the index of the key in text if found; -1 if not found.
	The null string only occurs within itself, at startPosition 0.
	If FAST_HASH is true, this is assumed to imply an O(1) hashing function,
which (via QsEqual) means this function tends to be O(n) in the length of the text
for non-perversely-hashing text and key combinations, rather than O(nm) (where m is
the key length). Fine-grained optimisation of this implementation is fairly reliant 
on the compiler's ability to inline, and then figure out that it doesn't have to keep
recalculating the hash of the key.
	Otherwise, a tuned brute-force search is used, which is O(nm). */

#if FAST_HASH

#define SearchImpl(name, stringsEqual) \
	QsInternalLength name(const QString *text, const QString *key, size_t startPosition) \
	{ \
		size_t keyLength = QsGetLength(key); \
		QsInternalLength limit = (QsInternalLength)QsGetLength(text) - keyLength, i; \
		\
		assert(startPosition < QS_MAX_LENGTH); \
		\
		for(i = (QsInternalLength)startPosition; i <= limit; i++) { \
			QString substr; \
			if(keyLength != 0 && !QsIsNull(text)) \
				/* Create a fake 'static' string to avoid possible memory allocation entailed by QsGetSubstring - */ \
				QsInitStatic(&substr, QsGetData(text) + i, keyLength); \
			else \
				QsInitNull(&substr); \
			\
			if(stringsEqual(&substr, key)) \
				return i; \
		} \
		return -1; \
	}
	
SearchImpl(QsSearch, QsEqual)

SearchImpl(QsSearchNoCase, QsEqNoCase)

#else /* !FAST_HASH */

#define SearchImpl(name, normChar) \
	QsInternalLength name(const QString *text, const QString *key, size_t startPosition) \
	{ \
		size_t keyLength = QsGetLength(key); \
		QsInternalLength limit = (QsInternalLength)QsGetLength(text) - keyLength, i; \
		const QsChar *t = QsGetData(text), *kp = QsGetData(key); \
		QsInternalLength j = 0; \
		\
		assert(startPosition < QS_MAX_LENGTH); \
		\
		for(i = (QsInternalLength)startPosition; i <= limit; i++) { \
			for(j = 0; j < keyLength && normChar(kp[j]) == normChar(t[i + j]); j++) \
				; \
			if(j == keyLength) \
				return i; \
		} \
		return -1; \
	}

SearchImpl(QsSearch, +)

SearchImpl(QsSearchNoCase, CToUpper)

#endif /* FAST_HASH */

/*** QsCompareToChar ***/

/* Compare the string to a single character.  Returns as for strcmp(). */

int QsCompareToChar(const QString *s, QsChar ch)
{
	QsChar b = !QsIsNull(s) ? QsGetFirst(s) : NUL;
	return b < ch ? -1 : b > ch || QsGetLength(s) > 1;
}

/*** QsCompareToNTS ***/

/* Compare the QString to the C-style string, returning a value as for the 
   standard library function strcmp(). */

int QsCompareToNTS(const QString *qs, const QsChar *cs)
{
	size_t qsLen, i;

	assert(qs != NULL && cs != NULL);

	qsLen = QsGetLength(qs);
	for(i = 0; i < qsLen && cs[i] != NUL && QsGetCharAt(qs, i) == cs[i]; i++)
		;
	return i >= qsLen || cs[i] == NUL 
		? (int)((QsInternalLength)qsLen - (QsInternalLength)i)
		: QsGetCharAt(qs, i) - cs[i];
}

/*** QsSearchForChar ***/

/* Like the standard library function strchr(). */

const QsChar *QsSearchForChar(const QString *s, QsChar ch)
{
	return QsIsNull(s) ? NULL : FindChar(QsGetData(s), (QsInternalLength)QsGetLength(s), ch);
}

/*** QsSearchForAny ***/

/* Like the standard library function strpbrk(). */

const QsChar *QsSearchForAny(const QString *s, const QString *find)
{
	return QsIsNull(s) ? NULL : FindOneOf(QsGetData(s), (QsInternalLength)QsGetLength(s), find);
}

/*** QsHash ***/

/* Get a hash code for a string. Case differences in characters are ignored.
The algorithm used is 'djb2' by Dan Bernstein, as given at http://www.cse.yorku.ca/~oz/hash.html */

#if HASH_ADDS
#define Combine(h, s, n) ((((h) << 5) + (h)) + CToUpper((s)[n]))
#else
#define Combine(h, s, n) ((((h) << 5) + (h)) ^ CToUpper((s)[n]))
#endif

static unsigned long HashFor(const QsChar *c, size_t keyLength)
{
	unsigned long hash = 5381;
	
#if FAST_HASH
	/* Because most BASIC symbols are short, only 1-3 characters long,
	hashing on at most four characters from the string tends to produce
	reasonable results. */
	
	hash = keyLength == 0 ? hash : Combine(hash, c, 0);
	hash = keyLength <= 1 ? hash : Combine(hash, c, keyLength - 1);
	hash = keyLength <= 2 ? hash : Combine(hash, c, keyLength - 2);
	hash = keyLength <= 3 ? hash : Combine(hash, c, keyLength >> 1);
#else
	/* A full hash function that uses every character in the string. */
	
	size_t i;
	
	for(i = 0; i < keyLength; i++)
		hash = Combine(hash, c, i);
#endif /* FAST_HASH */

#ifdef DEBUG
	++m_Hashes;
#endif

	return hash;
}

unsigned long QsHash(const QString *key, unsigned long modulo)
{
	unsigned long hash = 5381; /* = QsIsNull(key) ? 0 : CToUpper(QsGetFirst(key)); */

#if QSTRING_CACHE_HASH
	hash = key->hash != 0 ? key->hash : HashFor(QsGetData(key), QsGetLength(key));
#else
	const QsChar *c = QsGetData(key);
	size_t keyLength = QsGetLength(key);
	
#ifdef DEBUG
	++m_Hashes;
#endif

#if FAST_HASH
	/* Because most BASIC symbols are short, only 1-3 characters long,
	hashing on at most four characters from the string tends to produce
	reasonable results. */
	
	hash = keyLength == 0 ? hash : Combine(hash, c, 0);
	hash = keyLength <= 1 ? hash : Combine(hash, c, keyLength - 1);
	hash = keyLength <= 2 ? hash : Combine(hash, c, keyLength - 2);
	hash = keyLength <= 3 ? hash : Combine(hash, c, keyLength >> 1);
#else
	/* A full hash function that uses every character in the string. */
	size_t i;
	
	for(i = 0; i < keyLength; i++)
		hash = Combine(hash, c, i);
#endif /* !FAST_HASH */

	/*return hash;
	hash = HashFor(QsGetData(key), QsGetLength(key));*/
#endif /* ! QSTRING_CACHE_HASH */

	return modulo == 0 ? hash : hash % modulo;
}

#ifdef DEBUG

/*** QsPrintMemInfo ***/

void QsPrintMemInfo(void)
{
	fprintf(stderr, "%lu string copies; %lu memory allocations.\n"
		"%lu comparisons; %lu hashes.\n", m_Copies, m_Allocations, m_Comparisons, m_Hashes);
}

#endif /* DEBUG */

/****** Module-private functions ******/

/*** QsCopyForWrite ***/

/* Ensures the string can be written to and has a buffer of sufficient size.
If the string is 'static', it needs to be copied onto the heap. If already on
the heap, it will need to be copied if it is being referenced from more than
one place, or the string is being enlarged. Tiny ('short') strings can be stored 
in the space occupied by the pointer member of the data structure. In any case, the 
current contents of the string is preserved at the start of the string's 
buffer. The data in its memory after that is garbage. This function cannot be
used to shrink a string: the new length must be at least the old length. */

static void QsCopyForWrite(QString *s, size_t requiredLength)
{
	assert(requiredLength <= QS_MAX_LENGTH);
	assert(requiredLength >= QsGetLength(s));
	
	/* Cases that must be handled are:
		static or null --> short dynamic	--> A
		static or null --> long dynamic		--> B
		null --> null (requiredLength = 0)	--> A
		short dynamic --> short dynamic		--> A
		short dynamic --> long dynamic		--> B
		long dynamic --> long dynamic		--> B or C */

	if(requiredLength <= SHORT_STRING_LENGTH) {	/* CASE A */
		/* Either the string is already null, it's a short
		dynamic string, a static string, or the buffer is long enough
		already. */

		if(s->length > 0)
			/* Copy static data into short string data field. */
			CMemCpy(&s->data, s->data, s->length);
		/* If already a short dynamic string, data is unchanged. */
		s->length = -(QsInternalLength)requiredLength;
	}
	else if(s->length >= -SHORT_STRING_LENGTH /* CASE B */
	     || QsAccess(s)->actualLength < (QsInternalLength)requiredLength 
	     || QsAccess(s)->refCount > 1) {
		/* Note that in the above comparison, the actual allocated 
		length of the string is compared, not its 'visible' length, 
		if it's already on the heap. */

		struct QStrData *qsd = QsAllocate(requiredLength);
		if(QsGetLength(s) != 0)
			CMemCpy(&qsd->data, QsGetData(s), QsGetLength(s));
		QsDispose(s);
		QsInitIntern(s, qsd, requiredLength);
	}
	else /* CASE C */
		/* The string already has enough space preallocated in its 
		buffer, and has a single 'owner'. Extend it. */
		s->length = -(QsInternalLength)requiredLength;
}

/*** QsAllocate ***/

/* Allocates memory on the heap for a new string of the given size. */

static struct QStrData *QsAllocate(size_t length)
{
	struct QStrData *qsd;

	assert(length <= QS_MAX_LENGTH);
	assert(length > SHORT_STRING_LENGTH);
	
	if(length <= MEDIUM_STRING_LENGTH && length < QS_MAX_LENGTH / 2)
		length *= 2;

	qsd = (struct QStrData *)New((length - 1) * sizeof(QsChar) + sizeof(struct QStrData));
		/* - 1 for 'data' member (1st char) */
	qsd->refCount = 1;
	qsd->actualLength = (QsInternalLength)length;
	
	/* The contents of the string is not initialised. */

#ifdef DEBUG
	++m_Allocations;
#endif

	return qsd;
}

/*** FindOneOf ***/

static const QsChar *FindOneOf(const QsChar *start, QsInternalLength length, const QString *find)
{
	const QsChar *pos = NULL;
	size_t i;
	
	for(i = 0; i != QsGetLength(find) && pos == NULL; i++)
		pos = FindChar(start, length, QsGetCharAt(find, i));

	return pos;
}

/*** FindChar ***/

static const QsChar *FindChar(const QsChar *s, QsInternalLength length, QsChar ch)
{
	for( ; length > 0 && *s != ch; s++, length--)
		;
	return length > 0 ? s : NULL;
}

#ifdef DEBUG

/****** QString testbed ******/

/* These tests rely on the heap being available. */
/* TODO add to these! */

void QsRunTests(void)
{
	QString first, second, copyOfFirst, concatenated, extract;
	
	fprintf(stderr, "-- QString self-tests running ...\n");
	
	/* 1. Two strings initialised in different ways can be compared, and aren't equal. */
	QsInitStaticNTS(&first, "first");
	QsCopyNTS(&second, "second");
	assert(QsCompare(&first, &second) != 0);
	
	/* 2. A copy of a static string is equal to the original. */
	QsCopy(&copyOfFirst, &first);
	assert(QsCompare(&first, &copyOfFirst) == 0);
	
	/* 3. Concatenation yields a string of the expected length. */
	QsInitNull(&concatenated);
	QsJoin(&concatenated, &first, &second);
	assert(QsGetLength(&concatenated) == QsGetLength(&first) + QsGetLength(&second));
	
	/* 4. Substring works. */
	QsGetSubstring(&extract, &concatenated, QsGetLength(&first), QsGetLength(&second));
	assert(QsCompare(&extract, &second) == 0);
	
	/* 5. Disposing works (this test passes if it doesn't crash or leak memory!) */
	QsDispose(&first);
	QsDispose(&second);
	QsDispose(&copyOfFirst);
	QsDispose(&concatenated);
	QsDispose(&extract);
	
	fprintf(stderr, "-- QString self-tests finished.\n");
}

#endif /* DEBUG defined (testbed) */
