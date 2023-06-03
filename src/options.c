/****** options.c ******/

/*
	$VER: options.c 0.16A (5.6.2015)
	
	Handles options (aka configuration) for the interpreter. These are all provided 
	on the command line.
*/

#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include "common.h"
#include "platform.h"
#include "heap.h"
#include "qstring.h"
#include "options.h"

/* Default program text size in bytes, if file size isn't available.
   Must be larger than the default prelude! */
#define EXPECTED_PROGRAM_SIZE 8192

#define EXPECTED_PRELUDE_SIZE 3072

/* Minimum sized buffer that will be allocated: */
#define MIN_BUFFER_SIZE (EXPECTED_PROGRAM_SIZE + 1024)

/* Scale factor allowed for MERGEd modules for automatic buffer size calculation: */
#define EXPECTED_MERGE_MODULE_SCALE_FACTOR 2

/* Default initial size of the heap, in bytes. The stacks, the buffer, and built-in
   symbol definitions are all allocated on the heap, as well as objects created
   during program execution. The size of the heap can be fixed at startup by specifying
   a value using the --heap option. The CLEAR statement allows the heap size to be increased,
   but not decreased. */
/* #if MEMORY_CONSTRAINED
#define DEFAULT_INITIAL_HEAP_SIZE (48 * 1024)
#else */
#define DEFAULT_INITIAL_HEAP_SIZE (128 * 1024)
/* #endif */

static const char DEFAULT_PRELUDE[] = "prelude.bas";

static const char USAGE_MESSAGE[] =
	"An extended BASIC interpreter.  Version 0.16A (19-Jun-2021)\n"
	"Copyright 1996 - 2021 Frank Hawcroft\n\n"
	"Usage: %s\n\t[--buf[fer] <size>] [--heap <size>] [--profile <output-file>]\n"
	"\t[--prelude <prelude-file> | --noprelude] [--module-path <paths>]\n"
	"\t[--script-mode | --noscript-mode] [--low-memory | --nolow-memory]\n"
	"\t[--immediate] [--optimise] [--debug-prelude] [--unsafe]\n"
#ifdef DEBUG
	"\t[--verbose [<level>]] [--self-test]\n"
#endif
	"\t{[--file] <program-file> [<args> ...] | --exec[ute] <statements>}\n\n"
	"All options specified after the program-file will be treated as arguments to\n"
	"the program, not the interpreter.\n"
	"Short names for the options are:\n"
	"\t-b<size> -h<size> -p<output-file>\n"
	"\t-r<prelude-file>|-n -m<paths>\n"
	"\t-s|-c -l|-g\n"
	"\t-i -o -d -u\n"
#ifdef DEBUG
	"\t-v[<level>] -t\n"
#endif
	"\t-f<file> -e<statements>\n\n"
	"This executable has been compiled with these settings: "
#ifdef DEBUG
	"DEBUG "
#else
	"NDEBUG "
#endif
#if QSTRING_USE_WCHAR
	"WIDE-CHAR-STRINGS "
#endif
#if QSTRING_LONG_STRINGS
	"LONG-STRINGS "
#endif
	"%s "
	"\nHeap options: %s\n";

/* TODO move option processing to platform-specific functions, to allow use of
	local conventions: <getopt.h> on GNU-supporting systems, for example, or
	the Amiga conventions without - and --. */

enum OptionAbbreviation
{
	OA_BUFFER_SIZE = 'b',
	OA_FIXED_MEM_SIZE = 'h',
	OA_PROFILE = 'p',
	OA_PRELUDE = 'r',
	OA_NO_PRELUDE = 'n',
	OA_MODULE_PATH = 'm',
	OA_FILE_TO_RUN = 'f',
	OA_ONE_LINER = 'e',
	OA_SCRIPT_MODE = 's',
	OA_PRE_CHECKED_MODE = 'c', /* opposite of 's' */
	OA_LOW_MEMORY = 'l',
	OA_ORDINARY_MEMORY = 'g', /* opposite of 'l' */
	OA_IMMEDIATE = 'i',
	OA_OPTIMISE = 'o',
	OA_DEBUG_PRELUDE = 'd',
	OA_UNSAFE = 'u',
	OA_VERBOSE = 'v', /* DEBUG only */
	OA_SELF_TEST = 't', /* DEBUG only */
	OA_SENTINEL = NUL
};

struct OptionDefinition
{
	const char *fullName;
	enum OptionAbbreviation abbreviation;
	bool hasParameter;
};

static const struct OptionDefinition m_Definition[] =
{
	{ "buf", OA_BUFFER_SIZE, TRUE },
	{ "buffer", OA_BUFFER_SIZE, TRUE },
	{ "heap", OA_FIXED_MEM_SIZE, TRUE },
	{ "profile", OA_PROFILE, TRUE },
	{ "prelude", OA_PRELUDE, TRUE },
	{ "noprelude", OA_NO_PRELUDE, FALSE },
	{ "module-path", OA_MODULE_PATH, TRUE },
	{ "file", OA_FILE_TO_RUN, TRUE },
	{ "execute", OA_ONE_LINER, TRUE },
	{ "exec", OA_ONE_LINER, TRUE },
	{ "script-mode", OA_SCRIPT_MODE, FALSE },
	{ "noscript-mode", OA_PRE_CHECKED_MODE, FALSE },
	{ "low-memory", OA_LOW_MEMORY, FALSE },
	{ "nolow-memory", OA_ORDINARY_MEMORY, FALSE },
	{ "immediate", OA_IMMEDIATE, FALSE },
	{ "optimise", OA_OPTIMISE, FALSE },
	{ "debug-prelude", OA_DEBUG_PRELUDE, FALSE },
	{ "unsafe", OA_UNSAFE, FALSE },
	{ "verbose", OA_VERBOSE, FALSE },
	{ "self-test", OA_SELF_TEST, FALSE },
	{ NULL, OA_SENTINEL, FALSE }
};

static long ParseSize(const char *val)
{
	if(strchr(val, 'k') != NULL)
		return atol(val) * 1024;
	else if(strchr(val, 'm') != NULL)
		return atol(val) * 1024 * 1024;
	else
		return atol(val);
}

/* Checks the parameter and stores in options structure if OK.
	Returns TRUE if OK, FALSE if not (an unknown option or missing parameter). */
static bool ProcessOption(char option, const char *val, struct Options *options)
{
	switch(option) {
		case OA_BUFFER_SIZE:
			if(!isdigit(*val))
				return FALSE;
			else
				options->bufferLen = ParseSize(val);
			break;
		case OA_FIXED_MEM_SIZE:
			if(!isdigit(*val))
				return FALSE;
			else
				options->heapSize = ParseSize(val);
			break;
		case OA_PROFILE:
			if(options->profileDest != NULL || strlen(val) == 0)
				return FALSE;
			else
				options->profileDest = val;
			break;
		case OA_PRELUDE:
			if(options->prelude == NULL 
			|| PfFilenameCmp(options->prelude, DEFAULT_PRELUDE) != 0
			|| strlen(val) == 0)
				return FALSE;
			else
				options->prelude = val;
			break;
		case OA_NO_PRELUDE:
			if(options->prelude == NULL
			|| PfFilenameCmp(options->prelude, DEFAULT_PRELUDE) != 0)
				return FALSE;
			else
				options->prelude = NULL;
			break;
		case OA_MODULE_PATH:
			if(options->modPath != NULL)
				return FALSE;
			else
				options->modPath = val;
			break;
		case OA_FILE_TO_RUN:
			if(options->fileName != NULL || options->execute != NULL)
				return FALSE;
			else
				options->fileName = val;
			break;
		case OA_ONE_LINER:
			if(options->fileName != NULL || options->execute != NULL)
				return FALSE;
			else
				options->execute = val;
			break;
		case OA_SCRIPT_MODE:
		case OA_PRE_CHECKED_MODE:
			options->initialSyntaxCheck = option == OA_PRE_CHECKED_MODE;
			break;
		case OA_LOW_MEMORY:
		case OA_ORDINARY_MEMORY:
			options->lowMemory = option == OA_LOW_MEMORY;
			break;
		case OA_IMMEDIATE:
			options->immediate = TRUE;
			break;
		case OA_OPTIMISE:
			options->optimise = TRUE;
			break;
		case OA_DEBUG_PRELUDE:
			options->preludeDebugging = TRUE;
			break;
		case OA_UNSAFE:
			options->unsafe = TRUE;
			break;
#ifdef DEBUG
		case OA_VERBOSE:
			options->verbose = isdigit(*val) ? (unsigned)atoi(val) : 1;
			break;
		case OA_SELF_TEST:
			options->runModuleTests = TRUE;
			break;
#endif
		default:
			return FALSE;
	}
	
	return TRUE;
}

extern const char *ArraySizeDescription(void);

static long EstimatedProgramSize(const char *sourceFile)
{
	long length = sourceFile == NULL ? 0 : PfFileLength(sourceFile);
	return length < 0 ? EXPECTED_PROGRAM_SIZE : length;
}

static long EstimatedBufferSizeRequired(const char *prelude, const char *oneLiner, const char *file, bool lowMemory)
{
	long preludeSize = prelude == NULL ? 0 : EXPECTED_PRELUDE_SIZE; /* EstimatedProgramSize(Prelude()), */
	long programSize = EstimatedProgramSize(oneLiner != NULL ? NULL : file);
	long totalSize = preludeSize + EXPECTED_MERGE_MODULE_SCALE_FACTOR * programSize;
	long minSizeToAllocate = MIN_BUFFER_SIZE / (lowMemory ? 2 : 1);
	return totalSize > minSizeToAllocate ? totalSize : minSizeToAllocate;
}

bool ProcessCommandLineArgs(int argCount, const char *argVector[], struct Options *options)
{
	int count;
	bool argumentsOK = TRUE, caseMatters = PfFilenameCmp("x", "X") != 0;

	memset(options, NUL, sizeof(*options));

	/* Initialise options which aren't 0/FALSE/NULL by default: */
	
	options->interpreterCommand = argVector[0];
	options->prelude = DEFAULT_PRELUDE;
	options->initialSyntaxCheck = TRUE;
	
	for(count = 1; count < argCount && argumentsOK; count++) {
		char option;
		const char *val = NULL;

		/* Extract the option and any parameter value: */

		if(argVector[count][0] == '-' && isalpha(argVector[count][1])) {
			option = caseMatters ? argVector[count][1] : tolower(argVector[count][1]);
			val = &argVector[count][2];
		}
		else if(strncmp(argVector[count], "--", 2) == 0) {
			const struct OptionDefinition *defn;

			for(defn = &m_Definition[0];
			  defn->abbreviation != OA_SENTINEL && PfFilenameCmp(defn->fullName, &argVector[count][2]) != 0;
			  defn++)
				;

			option = defn->abbreviation;
			if(defn->hasParameter) {
				if(count < argCount - 1)
					val = argVector[++count];
				else {
					argumentsOK = FALSE;
					break;
				}
			}
		}
		else {
			/* Assume the program to execute. */
			option = OA_FILE_TO_RUN;
			val = argVector[count];
		}
		
		argumentsOK = ProcessOption(option, val, options);
		
		/* Get arguments passed to BASIC program - */
		if(argumentsOK && option == OA_FILE_TO_RUN) {
			options->argCount = argCount - count - 1;
			if(count < argCount - 1)
				options->argument = &argVector[count + 1];
			count = argCount;
		}
	}

	/* Check for compulsory parameters: */
	
	argumentsOK &= (options->fileName != NULL || options->execute != NULL || options->immediate);
	 
	/* Display informational error message if necessary: */
	 
	if(!argumentsOK)
		fprintf(stderr, USAGE_MESSAGE, argVector[0], ArraySizeDescription(), HeapOptionDescription());
	else {
		/* Calculate derived options: */
		
		options->fixedSizeHeap = options->heapSize > 0;
		options->initialHeapSize = options->fixedSizeHeap 
			? options->heapSize : DEFAULT_INITIAL_HEAP_SIZE / (options->lowMemory ? 2 : 1);
		options->initialBufferSize = options->bufferLen <= 0
			? EstimatedBufferSizeRequired(options->prelude, options->execute, options->fileName, options->lowMemory) : options->bufferLen;
	}
	
	return argumentsOK;
}
