/****** options.h ******/

/*
	$VER: options.h 0.16 (11.22.2014)

	Options (aka configuration) for the interpreter.
*/

#ifndef BAS_OPTIONS_H_INCLUDED
#define BAS_OPTIONS_H_INCLUDED

struct Options {
	const char *interpreterCommand; /* argv[0] */
	const char *fileName; /* Also directly from argv; memory isn't copied. */
	const char **argument; /* Arguments to program, also from argv. */
	unsigned argCount;
	const char *profileDest; /* Generate profiling data and write to this file. */
	long bufferLen; /* If <= 0, auto-size; otherwise use the fixed size specified. */
	long initialBufferSize;
	long heapSize;
	size_t initialHeapSize;
	bool fixedSizeHeap;
	const char *modPath; /* Module search path(s) (semicolon-separated). */
	const char *prelude; /* Prelude module. If null, suppress prelude. */
	const char *execute; /* Code to run, supplied on command line instead of in file. */
	bool immediate; /* Start in immediate mode. */
	bool initialSyntaxCheck; /* Do a lexical and syntactical checking pass before running. */
	bool lowMemory; /* Attempt to minimise memory use - favour frugality of memory use over speed. */
	bool optimise;
	bool preludeDebugging; /* Check prelude syntax and display errors in prelude? */
	bool unsafe; /* Allow PEEK, POKE and other statements and functions allowing access to arbitrary memory. */
#ifdef DEBUG
	unsigned verbose; /* Verbose debugging output level, 1 - 3. */
	bool runModuleTests;
#endif
};

/* Parse command line. Options to the interpreter must appear before the program name;
anything after the program name is an argument passed to the program itself.
Unix (GNU) style syntax is required for the command line.
TRUE is returned if the command line options are valid, FALSE if not. */
extern bool ProcessCommandLineArgs(int argc, const char *argv[], struct Options *opts);

#endif /* BAS_OPTIONS_H_INCLUDED */
