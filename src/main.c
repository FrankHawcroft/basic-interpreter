/****** main.c ******/

/*
	$VER: main.c 0.16A (5.8.2014)
	
	Start up and clean up.
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "interpreter.h"
#include "process.h"
#include "platform.h"
#include "options.h"
#include "heap.h"
#include "buffer.h"
#ifdef DEBUG
#include "cache.h"
#include "hashtable.h"
#include "stack.h"
#include "bitvector.h"
#endif

extern Error LoadPrelude(void);
extern int Loop(void);

static void ReportIfErrorExit(const char *, int);
static Error InitialiseInterpreter(const struct Options *);

int main(int argc, const char *argv[])
{
	struct Options options;
	
	PfStart();

	if(!ProcessCommandLineArgs(argc, argv, &options)) {
		/* Cannot use ExitWithError here because no Proc(), hence no Opts(), yet. */
		ReportError(BADCOMMANDLINEARGS, NULL, -1, NULL, NULL);
		ReportIfErrorExit(argv[0], EXIT_FAILURE);
		PfFinish();
		exit(EXIT_FAILURE);
	}

	RequireSuccess(InitialiseInterpreter(&options));

	RequireSuccess(LoadPrelude());
	
	if(options.fileName != NULL || options.execute != NULL)
	{
		bool oneLiner = options.execute != NULL;
		const char *codeOrFile = oneLiner ? options.execute : options.fileName;
		RequireSuccess(LoadProgram(codeOrFile, oneLiner, FALSE));
	}

	{
		int rc = Loop();
		
		DisposeProcess();
		DisposeHeap();
		ReportIfErrorExit(argv[0], rc);	
		PfFinish();
		
		return rc;
	}
}

static Error InitialiseInterpreter(const struct Options *options)
{
#ifdef DEBUG
	if(options->runModuleTests) {
		fprintf(stderr, "---- Running heap self-tests ...\n");
		RunBitVectorTests();
		RunHeapTests();
		fprintf(stderr, "---- Heap self-tests finished.\n");
	}
#endif
	
	CreateHeap(options->initialHeapSize, options->fixedSizeHeap);
	
#ifdef DEBUG
	if(options->runModuleTests) {
		fprintf(stderr, "---- Running other self-tests ...\n");
		StkRunTests();
		QsRunTests();
		HtRunTests();
		RunCacheTests();
		RunProgramBufferTests();
		fprintf(stderr, "---- Self-tests finished.\n");
	}
#endif

	InitConstants();

	return CreateNewProcess(options);
}

static void ReportIfErrorExit(const char *interpreterCommandName, int rc)
{
	if(rc != EXIT_SUCCESS)
		fprintf(stderr, "%s failed returncode %d\n",
			interpreterCommandName == NULL || *interpreterCommandName == NUL ? "BASIC interpreter" : interpreterCommandName, 
			rc);
}

static void ExitWithError(Error errNumber)
{
	ReportError(errNumber, NULL, -1, NULL, NULL);
	ReportIfErrorExit(Opts()->interpreterCommand, EXIT_FAILURE);
	DisposeProcess();
	DisposeHeap();
	PfFinish();
	exit(EXIT_FAILURE);
}

void RequireSuccess(Error result)
{
	if(result != SUCCESS)
		ExitWithError(result);
}
