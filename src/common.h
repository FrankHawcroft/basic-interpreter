/****** common.h ******/

/* 	
	$VER: common.h 0.16A (5.12.2015)

	Frequently used definitions which don't have anything to do with BASIC as such.
*/

#ifndef BAS_COMMON_H_INCLUDED
#define BAS_COMMON_H_INCLUDED

/*** Compiler standard ***/

/* MSC doesn't consider itself C99-compliant, but for my purposes, it's close enough. */

#if (defined(__STDC_VERSION__) && __STDC_VERSION__ >= 199901L) || defined(_MSC_VER)
#define REASONABLY_C99_COMPLIANT 1
#else
#define REASONABLY_C99_COMPLIANT 0
#endif

/*** Debugging - assertions and debug-only functions ***/

/* Defining NDEBUG when compiling will switch off assertions, and disable debugging support
features - such as the --verbose command line option and the XFREE and XSTACK statements. 
By default, DEBUG is assumed (see conditional definitions below), which means these extra 
checks and features are performed or available; but which incurs a performance penalty,
and means a slightly larger executable. */

#if !defined(_NDEBUG) && !defined(NDEBUG) && !defined(DEBUG)
#define DEBUG
#endif

#ifndef DEBUG
#ifndef NDEBUG
#define NDEBUG
#endif
#ifndef _NDEBUG
#define _NDEBUG
#endif
#endif /* ndef DEBUG */

#ifdef DEBUG
#define DIAGNOSTIC_FN_DECL(fn) extern fn
#else
#define DIAGNOSTIC_FN_DECL(fn) extern int main(int argc, char **argv) /* Because VBCC doesn't like 'empty declarations'. */
#endif

/* The standard assert() macro - */
#include <assert.h>

/*** The size_t type ***/

#include <stddef.h>

/*** Null constants ***/

#ifndef NUL
#define NUL	'\0'
#endif
#ifndef NULL
#define NULL 0
#endif

/*** Boolean type ***/

/* Assume that if macros 'bool', 'TRUE' or 'FALSE' have already been defined,
	these definitions are sensible. */

#ifndef __cplusplus
#if !defined(VBCC) && REASONABLY_C99_COMPLIANT
#include <stdbool.h>
#else
#ifndef bool
typedef char bool;
#endif /* bool not already defined as macro */
#endif /* C99-compliant compiler, or MSVC */
#endif /* not C++ */

#ifndef TRUE
#if defined(__cplusplus) || defined(__bool_true_false_are_defined)
#define TRUE true
#define FALSE false
#else
#define TRUE 1
#define FALSE 0
#endif /* C99-compliant or C++ compiler */
#endif /* ndef TRUE */

/*** Small bit fields ***/

/* Most compilers will accept a non-int bit field type, but VBCC does not. */

#ifdef VBCC
typedef int SmallBitField;
#else
typedef char SmallBitField;
#endif

/*** Inline ***/

#ifndef INLINE
#if defined(__cplusplus) || REASONABLY_C99_COMPLIANT
#define INLINE static inline
#else
#define INLINE static
#endif /* not C++ or C99-compliant compiler */
#endif /* ndef INLINE */

#endif /* BAS_COMMON_H_INCLUDED */
