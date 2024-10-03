/****** process.c ******/

/*
	$VER: process.h 0.16A (9.22.2017)

	Execution state for a BASIC program.
*/

#include <string.h>
#include <stdio.h>
#include "process.h"
#include "heap.h"
#include "options.h"
#include "cache.h"
#include "buffer.h"
#include "hashtable.h"
#include "platform.h"

extern void CleanUpUI(void);
extern void CleanUpAudio(void);

static void DeleteStatementCache(void);

#if PF_REENTRANT
#define MAX_PROCESSES 20
#else
#define MAX_PROCESSES 1
#endif

struct ProcessTableEntry {
	PfTaskIdentifier owner;
	struct Process *proc;
};

static struct ProcessTableEntry m_PT[MAX_PROCESSES];
static int m_PTUsedCount = 0;

#define PROCESS_SIZE sizeof(struct Process)

INLINE struct Process *FindCurrent(void)
{
	int i;
	PfTaskIdentifier self = PfGetCurrentTaskIdentifier();
		
	/* For performance, doesn't lock in PF_REENTRANT mode. This is in practice, if not in theory, safe,
	because of the way the process table is accessed - m_PTUsedCount is never decreased. */
	for(i = 0; i < m_PTUsedCount; i++) {
		if(m_PT[i].owner == self) {
			assert(m_PT[i].proc != NULL);
			return m_PT[i].proc;
		}
	}
	return NULL;
}

struct Process *Proc(void)
{
	if(m_PTUsedCount == 1) {
		/* If not running the same code from more than one task, avoid getting the task identifier,
		which may be slow. */
		assert(m_PT[0].proc != NULL);
		return m_PT[0].proc;
	}
	else {
		struct Process *proc = FindCurrent();
		assert(proc != NULL);
		return proc;
	}
}

Error CreateNewProcess(const struct Options *options)
{
	/* The magic ops number 0x100 must match OP_MACRO in repl.c. */
	static const struct TokenSequence emptyProto
		= {NULL, NULL, QS_NULL, QS_NULL, {(QsChar *)KW_EMPTY_STMT, 2}, NULL, 0, 0, 0x100, NULL, NULL};

	int slot;
	struct Process *p;
	PfMutex mutex;
	
	assert(options != NULL);
	assert(PF_REENTRANT || m_PTUsedCount == 0);

	PfInitialiseMutex(&mutex);
	PfBeginExclusiveExecution(&mutex);
	if(m_PTUsedCount == 0)	
		memset(m_PT, 0, sizeof(m_PT));
	PfEndExclusiveExecution(&mutex);
	
	{
		Error error;
		PfTaskIdentifier myTaskId = PfGetCurrentTaskIdentifier();
	
		PfBeginExclusiveExecution(&mutex);
		
		for(slot = 0; slot < m_PTUsedCount && m_PT[slot].proc != NULL; slot++)
			;
		
		error = slot >= MAX_PROCESSES ? ER_TOO_MANY_INSTANCES : SUCCESS;
		
		if(error == SUCCESS) {
			if(slot >= m_PTUsedCount)
				++m_PTUsedCount;
			m_PT[slot].owner = myTaskId;
		}
		
		PfEndExclusiveExecution(&mutex);
		
		if(error != SUCCESS)
			return error;
	}
	
	m_PT[slot].proc = p = New(PROCESS_SIZE);
	memset(p, NUL, PROCESS_SIZE);
	
	p->mode = MODE_HALTED_FOR_EXIT;
	
	p->opts = options;
	
	p->pendingError = SUCCESS;
	p->returnCode = EXIT_SUCCESS;
	
	p->buffer = CreateFileBuffer(options->initialBufferSize);
	p->initialTextExtent = NULL;
	
	p->currentPosition = NULL;
	p->currentStatementStart = NULL;
	p->currentFileName = NULL;
	
	p->trace = FALSE;
	
	InitStreams();
	
	p->gui = NULL;
	p->audio = NULL;
	
	CreateControlFlowStack(0);
	
	InitProfile(&p->stats);
	
	p->breakFlag = p->abortFlag = FALSE;
	InitEventTraps();

	DefineBuiltIns();
	
#ifdef DEBUG
	if(Opts()->verbose >= 3)
		PrintSymTab();
		
	p->maxNestLevel = SCOPE_MAIN;
	p->definitions = p->hashTableSearches = p->lookUps = 0;
#endif

	p->statementCache = NULL;
	p->untakenBranchCache = NULL;
	p->empty = emptyProto;
	
	p->callNestLevel 
		= p->functionCallNesting
		= p->staticSubCallNesting = SCOPE_MAIN;
		
	p->staticFunctionParams = NULL;
	
	p->arrayIndexBase = 0;
	p->readPosition = NULL;
	p->tokenIndex = -1;
	
	{
		int i;
		for(i = 0; i < 26; i++) /* TODO ASCII ordering only ... */
			p->defaultTypeForLetter[i] = DEFAULT_IMPLIED_TYPE;
	}
	
	return SUCCESS;
}

void DisposeProcess(void)
{
	struct Process *p = FindCurrent();
	
	if(p == NULL)
		return;
	
	if(Opts()->profileDest != NULL) {
		FILE *profileDump = fopen(p->opts->profileDest, "w");
		if(profileDump == NULL) {
			sprintf(p->additionalErrorInfo, "File: %.*s", MAX_ADDITIONAL_ERROR_MSG_LEN - 10, p->opts->profileDest);
			ReportError(CANTOPENPROFILE, NULL, -1, NULL, p->additionalErrorInfo);
		}
		else {
			PrintProfile(p->stats, p->buffer, profileDump);
			fclose(profileDump);
		}
	}
	
	DisposeProfilingData(&p->stats);
	
	DeleteStatementCache();
	
	if(p->untakenBranchCache != NULL) {
		DisposeCache(p->untakenBranchCache);
		p->untakenBranchCache = NULL;
	}

	DisposeControlFlowStack();

	if(p->staticFunctionParams != NULL) {
		HtDispose(p->staticFunctionParams);
		p->staticFunctionParams = NULL;
	}

	DisposeSymbolTable();

	DisposeEventTraps();
	
	CloseAllStreams();
	Dispose(p->streams);
	p->streams = NULL;
	
	if(p->buffer != NULL) {
		DisposeFileBuffer(p->buffer);
		p->buffer = NULL;
	}
	
	CleanUpUI();
	CleanUpAudio();
	
	Dispose(p);
	
	{
		PfMutex mutex;
		PfTaskIdentifier self = PfGetCurrentTaskIdentifier();
		int slot;
		
		PfInitialiseMutex(&mutex);
		PfBeginExclusiveExecution(&mutex);
		
		for(slot = 0; slot < m_PTUsedCount && m_PT[slot].owner != self; slot++)
			;	
		if(slot < m_PTUsedCount) {
			m_PT[slot].owner = PF_NULL_TASK_ID;
			m_PT[slot].proc = NULL;
		}
				
		PfEndExclusiveExecution(&mutex);
	}
}

const struct Options *Opts(void)
{
	return Proc()->opts;
}

/* Because the location in the source code is used as the key when caching, the cache must
	be cleared if there's the potential for memory to be reused to hold a different program;
	e.g. for immediate mode code which is stored in a potentially temporarily allocated
	buffer. */
struct TokenSequence *GetFromCache(struct Process *proc, const char *position)
{
	void *entry = RetrieveFromCache(proc->statementCache, position);
	if(entry != NULL && WithinFileBuffer(proc->buffer, (const char *)entry)) {
		/* This reuse of a single structure is quick, and saves memory in the stmt cache since
		   only one pointer (the next stmt location) needs to be stored, but care must be taken
		   with its lifetime during each statement execution round. */
		proc->empty.start = position;
		proc->empty.next = (const char *)entry;
		return &proc->empty;
	}
	return (struct TokenSequence *)entry;
}

static void DisposeEntry(void *entry)
{
	if(!WithinFileBuffer(Proc()->buffer, (const char *)entry)) {
		DisposeTokenSequence((struct TokenSequence *)entry);
		Dispose(entry);
	}
}

void CreateStatementCache(void)
{
	const unsigned approxPreludeSize = 3500;
	
	struct Process *p = Proc();

	/* If not operating in memory restricted mode (-l), create a generously sized cache -
		aim is to avoid thrashing. */

	/* mandel.bas - 900 chs, 27 cacheable stmts
	   lisp.bas - 22924 chs, 487 cacheable stmts
	   fractal_mountains.bas - 5513 chs, 145 cacheable stmts
	   life.bas - 1794 chs, 38 cacheable stmts */
	
	unsigned expectedCacheableStatements = (FileBufferExtent(p->buffer) - FileBufferBase(p->buffer) - approxPreludeSize) / 40;
	
	if(expectedCacheableStatements < 10)
		expectedCacheableStatements = 10;

	GetStatement(&p->empty.statementName, &p->empty.command);

	if(p->opts->lowMemory)
		expectedCacheableStatements = 1;
	
	p->statementCache = CreateCache(expectedCacheableStatements, 9 * expectedCacheableStatements + 1, &DisposeEntry);
}

void AttemptToCache(const struct TokenSequence *tokSeq)
{
	struct Process *p = Proc();
	if(StatementIsEmpty(tokSeq->command))
		SetInCache(p->statementCache, tokSeq->start, (void *)tokSeq->next);
	else {
		struct TokenSequence *cached = Duplicate(tokSeq);
		/* Don't end the program due to failing to alloc a cache entry. */
		if(cached != NULL)
			SetInCache(p->statementCache, tokSeq->start, cached);
	}
}

static void DeleteStatementCache(void)
{
	if(Proc()->statementCache != NULL) {
		DisposeCache(Proc()->statementCache);
		Proc()->statementCache = NULL;
	}
}

void ClearStatementCache(void)
{
	if(Proc()->statementCache != NULL)
		ClearCache(Proc()->statementCache);
}

#ifdef DEBUG

void PrintStatementCacheStatus(FILE *f, const char *stmt)
{
	const struct TokenSequence *ts = GetFromCache(Proc(), stmt);
	fprintf(f, "<%c%x> ", ts == NULL ? ' ' : (ts->preconverted != NULL ? 'P' : 'T'),
		ts == NULL ? 0 : ts->ops);
}

#endif

static void NoNeedToDisposeValue(void *v) {	}

void CreateUntakenBranchCache(void)
{
	if(Proc()->untakenBranchCache == NULL && !Opts()->lowMemory)
		Proc()->untakenBranchCache = CreateCache(389, 389, &NoNeedToDisposeValue);
}

/* Length is passed separately so this can easily be used for QStrings. */
void SetAdditionalErrorMessage(const char *fmt, const char *obj, size_t len)
{
	struct Process *p = Proc();
	int maxLen;

	assert(fmt != NULL);
	assert((obj != NULL) == (strstr(fmt, "%.*") != NULL));
	
	maxLen = strlen(fmt) + len >= MAX_ADDITIONAL_ERROR_MSG_LEN ? 0 : (int)len;
	if(obj != NULL)
		sprintf(p->additionalErrorInfo, fmt, maxLen, obj);
	else if(strlen(fmt) < MAX_ADDITIONAL_ERROR_MSG_LEN)
		strcpy(p->additionalErrorInfo, fmt);
	else
		p->additionalErrorInfo[0] = NUL;
}
