/****** process.h ******/

/*
	$VER: process.h 0.16A (7.24.2017)

	Execution state for a BASIC program.
*/

#ifndef BAS_PROCESS_H_INCLUDED
#define BAS_PROCESS_H_INCLUDED

#include "interpreter.h"

struct Options;
struct Buffer;
struct Stack;
struct CircularQueue;
struct HashTable;
struct Cache;
struct Stream;
struct Trap;
struct Event;
struct Statistics;
struct UserInterface;
struct Audio;

enum InterpreterMode {
	MODE_RUNNING, /* Executing a program non-interactively. */
	MODE_INTERACTIVE, /* Immediate mode. */
	MODE_HALTED_FOR_EXIT /* Pending exit. */
};

#define MAX_ADDITIONAL_ERROR_MSG_LEN 128

struct Process {
	enum InterpreterMode mode;

	/* Access the global options structure populated by ProcessCommandLineArgs (not owned!) - */
	const struct Options *opts;

	/* Program text - */
	struct Buffer *buffer;
	const char *initialTextExtent; /* Limit of initially loaded program text - not including MERGEd files. */

	/* Definitions - */
	/* The table of environments. Kept separately from the control flow stack because it's marginally
		more space and time efficient not to include them there, due to the c.f. stack's double duty - 
		tracking subprogram calls as well as block control structures and GOSUBs. */
	struct HashTable **environment;
	short envCount;
	/* Beyond this call nest level limit, the entire environment will be deleted on sub/function exit,
		to save memory in case of deeply-nested call sequences. Keeping some active saves time allocating
		the hash tables. */
	short retainedWarmEnvs;
	
	/* Static function parameters are not unique per function.
		This is quicker and saves memory, but means STATIC has to be applied to functions with care. */
	struct HashTable *staticFunctionParams;

	/* The execution location - */
	const char *currentPosition; /* Next statement to execute. */
	const char *currentStatementStart; /* Statement currently being executed. */
	const char *currentFileName; /* Name of program file currently executing. Only updated if trace == TRUE. */

	/* Control flow - */
	struct Stack *controlFlowStack;
	unsigned currentContext;
	
	/* Errors - */
	Error pendingError;
	char additionalErrorInfo[MAX_ADDITIONAL_ERROR_MSG_LEN];	
	int returnCode;
		
	/* Current level of nesting in subprogram or function calls - */
	short callNestLevel;
	short functionCallNesting; /* specifically, call nesting in DEFined functions */
	short staticSubCallNesting; /* specifically, call nesting in STATIC SUBs */
	
	/* Stream I/O - */
	struct Stream *streams;
	char *currentObjectName;
	
	/* GUI and sound - */
	struct UserInterface *gui;
	struct Audio *audio;

	/* Event handling - */
	struct Trap *trap;
	struct CircularQueue *q;
	/* The active event is the source of information accessed by functions such as ERR, INKEY, etc. */
	struct Event *activeEvent;
	bool breakFlag;
	
	/* Tracing and profiling - */
	bool trace;
	struct Statistics *stats;
	
	/* Caches - */
	struct Cache *statementCache;
	struct Cache *emptyStmtCache;
	struct Cache *untakenBranchCache;
	
	/* OPTION BASE - */
	unsigned arrayIndexBase;

	/* Position for READ - */
	const char *readPosition;
	int tokenIndex;
	
	/* Support for the DEFtype statements - map initial letter of variable names (A-Z) to types - */
	SimpleType defaultTypeForLetter[26];

#ifdef DEBUG
	/* Various statistics - */
	short maxNestLevel;
	unsigned long definitions, hashTableSearches, lookUps;
#endif
};

/* Create and access the global (per task!) process - */
extern Error CreateNewProcess(const struct Options *);
extern struct Process *Proc(void);
extern void DisposeProcess(void);

/* Access the global (per task!) options; only works once the process has been created - */
extern const struct Options *Opts(void);

/* Cache management - */
extern struct TokenSequence *GetFromCache(struct Process *, const char *position);
extern void AttemptToCache(const struct TokenSequence *);
extern void ClearStatementCache(void);
DIAGNOSTIC_FN_DECL(void PrintStatementCacheStatus(FILE *f, const char *stmt));
extern void CreateUntakenBranchCache(void);

/* Error reporting - */
extern void SetAdditionalErrorMessage(const char *fmt, const char *obj, size_t len);

#endif /* BAS_PROCESS_H_INCLUDED */
