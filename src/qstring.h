/****** qstring.h ******/

/*
	$VER: qstring.h 0.16A (5.5.2015)

	QString ('quick string') is a string data type.

	QString offers a number of advantages over traditional C-style strings:

	(1) Reference counting, 'copy on write' behaviour, so that the string's
	content is not copied unless a mutable version is required.

	(2) Not copying the content when a string which the developer vouches to be
	both immutable and sufficiently long-lived is copied.
	
	For example, in the interpreter, literal strings in the program code don't
	need to have their content copied, because the program code is retained in
	memory for the entire lifetime of the program: so in initialisation from a
	string literal like the following, 's' just receives a 'static copy'; this
	points in to the program text, which is itself just a C-style string:

		s$ = "abc"
		
	This is possible because QStrings are not NUL-terminated.

	(3) Storing very short strings in the data structure itself, rather than on the 
	heap. The aim is to reduce further the number of potentially expensive memory
	management calls. Very short strings occur frequently in BASIC code, for 
	example in loops like this:

		asterisks% = 0
		for i = 1 to len(s)
			x$ = mid(s, i, 1) '' evaluating this creates a temporary 1-char string
			if x = "*" then asterisks = asterisks + 1
		next

	(4) (Sometimes) quicker equality comparison by comparing pointers, string
	lengths, and hash values, before doing a character-by-character comparison.
	Use of hashes can also speed up searching if a hash algorithm is used which
	only includes a sample of characters from a string.

	Because the interpretation of the structure members changes depending on where 
	the data resides, all QString operations must use the functions provided, rather 
	than directly accessing structure members. QStrings can safely be statically 
	initialised, however.
*/

#ifndef QSTRING_H_INCLUDED
#define QSTRING_H_INCLUDED

#include <limits.h>
#include <stdio.h> /* Annoyingly, required for FILE * */
#include <stdlib.h>
#include "common.h"

/*** Configuration - string character type, and maximum length ***/

/* -- Whether to use wide characters - this is a simplistic option that selects either 
'wchar_t' or 'char' as the character type for QString.

TODO (1) far more involved would be to support variable-length encodings, e.g. UTF-8,
which is a lot more widely used than wchar_t these days. Ideally this would be done
with transparent translation/comparison, ala Perl. Would really need to use an
existing library to do that. 
TODO (2) also, should support named Unicode entities when reading from files */
	
#define QSTRING_USE_WCHAR 0

#if QSTRING_USE_WCHAR
#include <wchar.h>
typedef wchar_t QsChar;
#else
typedef char QsChar;
#endif
	
/* -- String length type - if the 'long strings' option isn't defined, 'short'
is used to store string lengths. Because QStrings use the sign bit of the length
internally, typically this will allow strings up to 32767 characters long - obviously,
this depends on sizeof(short) for the platform. 
	If long strings are enabled, a 32-bit signed integer is used if compiling
with a C99-compliant compiler or MSVC; otherwise 'long' is used. */

#define QSTRING_LONG_STRINGS 0

#if QSTRING_LONG_STRINGS

#if REASONABLY_C99_COMPLIANT

#include <stdint.h>
typedef int32_t QsInternalLength;
#define QS_MAX_LENGTH INT32_MAX

#else

typedef long QsInternalLength;
#define QS_MAX_LENGTH LONG_MAX
#endif /* !REASONABLY_C99_COMPLIANT */

#else

typedef short QsInternalLength;
#define QS_MAX_LENGTH SHRT_MAX

#endif /* !QSTRING_LONG_STRINGS */

/*** Data structure and functions ***/

/* QStrings can be statically initialised - the data and length must both be supplied.
Apart from at initialisation, the structure members should not be accessed directly. */

typedef struct QString_struct {
	QsChar *data;
	QsInternalLength length;
} QString;

/* Constructors - initialise a string in various ways: */

/* -- as a 0-length, empty string: */
#define QS_NULL {NULL, 0} /* For static initialisation. */
extern QString *QsInitNull(QString *);

/* -- from C characters or strings (character arrays): */
extern QString *QsInitStatic(QString *, const QsChar *, size_t);	
extern QString *QsInitStaticPtrs(QString *, const QsChar *start, const QsChar *end);
extern QString *QsInitStaticNTS(QString *, const QsChar *); /* NTS = a C style 0-terminated string */
extern QString *QsCopyNTS(QString *, const QsChar *);
extern QString *QsCopyChar(QString *, QsChar);
extern QString *QsCopyData(QString *, const QsChar *, size_t length);
extern QString *QsRepeat(QString *, size_t count, QsChar);

/* -- from other QStrings: */
extern QString *QsCopy(QString *to, const QString *from);
extern QString *QsGetSubstring(QString *result, const QString *s, size_t start, size_t count);
extern bool QsJoin(QString *, const QString *, const QString *);
extern QString *QsToUpperCase(QString *ucase, const QString *in);

/* Mutators - modify an existing string in-place: */

extern QString *QsSetCharAt(QString *, size_t, QsChar);
extern QString *QsAppendChar(QString *, QsChar);
extern bool QsAppend(QString *prefix, const QString *suffix);

/* Destructor - deallocate a string: */

extern void QsDispose(QString *);

/* Accessors - access a string's contents or information about it: */

/* See also: QsGetLength, QsIsNull, QsGetFirst, defined inline below. */
extern const QsChar *QsGetData(const QString *); /* read only! */
extern QsChar QsGetCharAt(const QString *, size_t);
extern QsChar QsGetLast(const QString *);
extern unsigned QsHash(const QString *);
extern void QsCopyToBuffer(const QString *, QsChar *, size_t);
extern QsChar *QsDupAsNTS(const QString *);
extern int QsTokenise(const QString *source, QString *token, size_t maxTokens, const QString *separator);

/* Comparators - compare and search: */

extern int QsCompare(const QString *, const QString *);
extern bool QsEqual(const QString *, const QString *);
extern bool QsEqNoCase(const QString *, const QString *);
extern QsInternalLength QsSearch(const QString *text, const QString *key, size_t startPosition);
extern QsInternalLength QsSearchNoCase(const QString *text, const QString *key, size_t startPosition);
extern int QsCompareToChar(const QString *qs, QsChar);
extern int QsCompareToNTS(const QString *qs, const QsChar *cs);
extern const QsChar *QsSearchForChar(const QString *s, QsChar);
extern const QsChar *QsSearchForAny(const QString *s, const QString *find);

/* Output: */

extern bool QsWrite(const QString *, FILE *);

/* Debugging/diagnostic: */

DIAGNOSTIC_FN_DECL(void QsPrintMemInfo(void)); /* Show memory allocation count. */
DIAGNOSTIC_FN_DECL(void QsRunTests(void)); /* Run some simple sanity tests of important functions. */

/* Small functions defined as macros for performance: */

/* The length of the string in character units; variable-length encodings are not parsed. */
#if QSTRING_LONG_STRINGS
#define QsGetLength(s) ((size_t)(labs((s)->length)))
#else
#define QsGetLength(s) ((size_t)(abs((s)->length)))
#endif

/* True iff the string is null (empty). */
#define QsIsNull(s) ((s)->length == 0)

/* The first character of the string. Fails if the string is empty. */
#define QsGetFirst(s) QsGetCharAt((s), 0)

#endif /* ndef QSTRING_H_INCLUDED */
