/****** platform.h ******/

/*
	$VER: platform.h 0.16A (4.30.2014)

	Platform-specific types, limits, and features.
*/

#ifndef BAS_PLATFORM_H_INCLUDED
#define BAS_PLATFORM_H_INCLUDED

#include "common.h"

#ifdef _MSC_VER
/*#define _CRT_SECURE_NO_WARNINGS*/ /* Must define in the project properties (or /D on command line) in order for this to take effect. */
#define _CRT_NONSTDC_NO_DEPRECATE

#define strnicmp _strnicmp
#define strdup _strdup

#else

#if !defined(_WIN32)
/* In MSVC, __cdecl is required on functions called via pointers when building with frame pointers omitted - */
#define __cdecl
#endif

#endif /* _MSC_VER */

#ifdef AMIGA
#include <exec/semaphores.h>
#endif

#include <stdio.h>
#include <limits.h>

#ifndef AMIGA
#include <time.h> /* time_t, clock_t */
#endif

#ifdef _WIN32
#include <windows.h>
#endif

#ifdef __linux__
#include <linux/limits.h>
#endif

#ifdef VBCC /* Assume AMIGA target. */
extern char *strdup(const char *);
#endif

/* intptr_t - standard type sufficient to store a pointer as an integer. */

#if REASONABLY_C99_COMPLIANT
#include <stdint.h>
#else
typedef unsigned long intptr_t;
#endif

/* Sufficiently restrictive type for alignment of any object in memory.
   See heap.c. */

typedef long PfAlignmentType;

/* The 'opposite' of PfAlignmentType - the smallest or most 'granular' type.
  By definition in C this is always char, so that should be appropriate for all (?) platforms. */

typedef char PfGranularType;

/* Whether the platform has strict alignment requirements - 
	i.e. requires long or quadword alignment for any primitive types. */

#define PF_REQUIRES_STRICT_ALIGNMENT 0

/* Whether all-zero-bytes represents zero for floating point types, and pointers. */

#define PF_INTUITIVE_CONCEPT_OF_ZERO_VALUES_HOLDS 1

/* Format string for size_t. */

#if !defined(VBCC) && REASONABLY_C99_COMPLIANT
#define SIZE_FMT "%zu"
#else
#define SIZE_FMT "%lu"
#endif

/* Whether the platform discriminates between 'text' and 'binary' files. */

#ifdef _WIN32
#define PF_BINARY_TEXT_DISTINCTION 1
#else
#define PF_BINARY_TEXT_DISTINCTION 0
#endif

/* Whether the filesystem allows an empty string, "", as a path name.
   On the Amiga, this empty path means the current directory. As far as I
   know, it isn't allowed on any other OS. */

#ifdef AMIGA
#define PF_EMPTY_PATH_ALLOWED 1
#else
#define PF_EMPTY_PATH_ALLOWED 0
#endif

/* The system path separator, and current, parent, and root directory specifiers: */

extern const char PF_PATH_SEP[];
extern const char PF_CUR_DIR[];
extern const char PF_PARENT_DIR[];
extern const char PF_ROOT_DIR[];

/* Length of the system line-terminating sequence: */

#ifdef _WIN32
#define PF_LINE_TERMINATOR_LENGTH 2
#else
#define PF_LINE_TERMINATOR_LENGTH 1
#endif

/* Whether the clib will properly standardise the system line-terminating sequence
	for text-mode files: */
	
#ifdef VBCC
#define PF_CLIB_PROPERLY_TRANSLATES_LINE_ENDING_SEQUENCE 0
#else
#define PF_CLIB_PROPERLY_TRANSLATES_LINE_ENDING_SEQUENCE 1
#endif

/* Any initialisation required for the system - this is called once at program startup. */
extern void PfStart(void);

/* Any clean up required for the system - this is called once before program exit. */
extern void PfFinish(void);

/* Allocate memory. */
extern void *PfAllocMem(size_t);

/* Return memory to the system. The size parameter is ignored on most platforms. */
extern void PfFreeMem(void *, size_t);

/* Memory available in system: */
extern size_t PfAvailMem(void);

/* Whether the path is an idiomatic way of referring to the current, root, or parent directory: */
extern bool PfIsSpecialDirSpec(const char *path);

/* Whether the path seems to refer to a directory:
   This should be a lexical check only - doesn't look at the filesystem ... */
extern bool PfIsDirSpec(const char *path);

/* ... Cf: whether the path is actually an existing directory in the filesystem: */
extern bool PfIsDirectory(const char *name);

/* Whether the given character separates components of a path: */
extern bool PfIsPathSep(char c);

/* Whether the given path is an absolute path: */
extern bool PfIsAbsolutePath(const char *name);

/* Whether the given path is an explicitly current-directory-relative path: */
extern bool PfIsExplicitlyCwdRelativePath(const char *name);

/* Change the current working directory, returning zero for success: */
extern int __cdecl PfChangeWorkingDirectory(const char *path);

/* Non-recursively do something with the names of all the files in a directory in 
an arbitrary (system-dependent) order. 
	The visitor/'action' function will be passed the file name, the supplied
data parameter, and whether the entry is a subdirectory. It should return true while
directory scanning should continue; false when it should stop.
	Returns 1 on success; 0 if the limit is reached; a negative value on error
(which may be the negative of an errno value, or INT_MIN to indicate a general
or unknown error). */
extern int PfVisitFilesAt(const char *directoryName, 
			    bool (*act)(const char *name, bool subDir, void *data),
			    void *dataParam,
			    int objectCountLimit);

/* Check for existence of a file (not a directory!): */
extern bool PfFileExists(const char *name);

/* Join path specifiers: */
extern void PfJoinPath(char *base, const char *append);

/* Textually compare paths or filenames, returning as for strcmp: */
extern int PfFilenameCmp(const char *, const char *);

/* Attempt to get length of file from OS, returning -1 if not possible:

Note that for files with CR+LF linefeeds read on systems such as Windows, the
reported length of the file will not reflect the actual number of characters
which can be read from it, because the C library transparently converts
CR+LFs into single LF characters. */
/* TODO should also handle long files (i.e. use 'long long' or 'off_t' or whatever)
	on systems where files can be longer than 'long' can represent */ 
extern long PfFileLength(const char *name);

/* Whether a stream (FILE *) is 'interactive', i.e. attached to some kind of 
   virtual terminal. */
extern bool PfIsInteractive(FILE *fh);

/* Change whether a file is in 'binary mode'. */
extern bool PfChangeFileBinaryMode(FILE *fh, bool binary);

/* Get the conventional home directory path for the system for the current user;
	if the system doesn't have this concept, returns NULL: */
extern const char *PfHomeDirectory(void);

/* Whether the given path is a conventional way of referring to stdin. */
extern bool PfRepresentsStdin(const char *name);

/* Sleep for a period of time: */
extern void PfSleep(unsigned long micros);

/* Event notification: */

#ifdef AMIGA
typedef ULONG PfEventNotificationHandle;
#define PfCombineEventNotificationHandles(signalBreak, ui, audio) ((signalBreak) | (ui) | (audio))
extern PfEventNotificationHandle PfGetBreakEventNotificationHandle(void);
extern void PfSleepUntilEvent(PfEventNotificationHandle);
#else
/* TODO implement for other systems */
typedef int PfEventNotificationHandle;
#define PfCombineEventNotificationHandles(signalBreak, ui, audio) 0
#define PfGetBreakEventNotificationHandle() 0
#define PfSleepUntilEvent(handle) PfSleep(10 * 1000) /* Just wait for 10 milliseconds */
#endif

#ifdef AMIGA

#define PF_POLL_FOR_SIGNALS 1

/* signal() doesn't seem to work with VBCC, so a polling function needs to be used. */
extern bool PfTestAndClearBreakSignal(void);

#else
#define PF_POLL_FOR_SIGNALS 0
#endif

/* Timestamps: */

#if defined(AMIGA) && defined(__GNUC__) /* time() always returns 0 */
#define USE_AMIGA_TIMER_CALLS 1
#else
#define USE_AMIGA_TIMER_CALLS 0
#endif

struct PfSystemTimeStamp {
#if USE_AMIGA_TIMER_CALLS
	unsigned long secs, mics;
#else
	time_t secs; /* System time. */
	clock_t clocks;	/* Execution time. */
#endif
};

extern void PfInitSystemTimeStamp(struct PfSystemTimeStamp *timeStamp);
#ifdef DEBUG
extern void PfPrintSystemTimeStamp(const struct PfSystemTimeStamp *timeStamp);
#endif
extern void PfGetSystemTimeStamp(struct PfSystemTimeStamp *timeStamp);
extern bool PfTimeHasElapsed(const struct PfSystemTimeStamp *start,
	const struct PfSystemTimeStamp *end, float interval);

/* Execution timing for profiling: */

#ifdef _WIN32 /* actually only available in Windows 2000 and up */
typedef LARGE_INTEGER PfHighResolutionTimeStamp;
#else
typedef struct PfSystemTimeStamp PfHighResolutionTimeStamp; /* not really high resolution at all */
#endif

extern void PfRecordTime(PfHighResolutionTimeStamp *);
extern float PfGetElapsedTimeSince(const PfHighResolutionTimeStamp *);

/* Multitasking: */

#ifdef AMIGA
#define PF_REENTRANT 1
typedef struct SignalSemaphore PfMutex;
typedef const struct Task *PfTaskIdentifier;
#define PF_NULL_TASK_ID NULL
#else
#define PF_REENTRANT 0
typedef int PfMutex;
typedef int PfTaskIdentifier;
#define PF_NULL_TASK_ID 0
#endif

extern void PfInitialiseMutex(PfMutex *mutex);
extern void PfBeginExclusiveExecution(PfMutex *);
extern void PfEndExclusiveExecution(PfMutex *);
extern PfTaskIdentifier PfGetCurrentTaskIdentifier(void);

#endif /* BAS_PLATFORM_H_INCLUDED */
