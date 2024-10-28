/****** profile.c ******/

/*
	$VER: profile.c 0.16A (4.5.2015)

	'Profiling' for BASIC programs. The profiler is rudimentary -
	for each statement (line) in the program, it counts how many times it
	was executed, and the total time spent in the statement.
*/

#include <stdio.h>
#include "common.h"
#include "platform.h"
#include "heap.h"
#include "buffer.h"
#include "hashtable.h"

/* Controls whether counts are displayed for prelude. */
#define PROFILE_PRELUDE TRUE

/* Controls whether timing is displayed. */
#define PROFILE_SHOWS_TIMING TRUE

/* Hash table size. */
#define PROFILE_HT_BINS 227

struct Profile {
	struct HashTable *table;
};

/* Statistics for a statement. */
struct Statistics {
	unsigned long executionCount;
	float totalTime; /* Of indeterminate unit. */
};

struct Profile *CreateProfile()
{
	struct Profile *profile = New(sizeof(struct Profile));
	profile->table = NULL;
	return profile;
}

void DisposeProfile(struct Profile *profile)
{
	assert(profile != NULL);
	
	if(profile->table != NULL)
		HtDispose(profile->table);
	Dispose(profile);
}

static void CreateKey(QString *actualKey, const void *rawKey)
{
	/* Initialise the string from the pointer's raw bytes, rather than printing it
		into a string (commented out below) - this avoids a memory allocation. */
	unsigned intKey = (unsigned)((intptr_t)rawKey & UINT_MAX);
	QsCopyData(actualKey, (const QsChar *)&intKey, sizeof(intKey) / sizeof(QsChar));
	
	/* Better quality key creation, but entails a memory allocation - */
	/*char ptrBuf[32];
	sprintf(ptrBuf, "%p", rawKey);
	QsCopyNTS(actualKey, ptrBuf);*/
}

/* Increment the execution count for the statement, and time spent. */
void RecordExecution(struct Profile *profile, const struct Buffer *buffer, const char *stmt, float timeTaken)
{
	struct Statistics *stats;
	const char *base = FileBufferBase(buffer);
	QString key;
	
#if !PROFILE_SHOWS_TIMING
	timeTaken = 0;
#endif

	/* Find the start of the line - multi-statement lines are aggregated, to make displaying them easier. */
	while(stmt > base && stmt[-1] != '\n')
		--stmt;
	
	if(profile->table == NULL)
		profile->table = HtCreate(PROFILE_HT_BINS, &Dispose, NULL);
	
	CreateKey(&key, stmt);
	
	stats = HtLookUp(profile->table, &key);
	
	if(stats != NULL) {
		++stats->executionCount;
		stats->totalTime += timeTaken;
	}
	else {
		stats = New(sizeof(struct Statistics));
		stats->executionCount = 1;
		stats->totalTime = timeTaken;
		HtAdd(profile->table, &key, stats);
	}
	
	QsDispose(&key);
}

/* Print a statement line for a profiling report.
	Continuations (_) are treated as separate statements.
	Multiple separated statements on one line (|) are treated as one statement.
This keeps a 1-1 correspondence of source and program lines in the profiling
report. It also makes this function very simple: it prints all characters until
a newline or NUL is encountered. 

The reallyPrint parameter controls whether the text is actually printed, or this 
function is just used to skip over the statement. This is desirable for hidden 
modules (i.e. the prelude). */
static void PrintStatementLine(FILE *f, const char **sp, bool reallyPrint)
{
	while(**sp != NUL && **sp != '\n') {
		if(reallyPrint)
			fputc(**sp, f);
		++*sp;
	}
	if(reallyPrint)
		fputc('\n', f);
}

#ifdef DEBUG
extern void PrintStatementCacheStatus(FILE *, const char *);
extern void PrintUntakenBranchCacheInfo(FILE *, const char *);
#endif

/* Prints a profiling report, which is the program text with execution count and total
time prepended to each line. */
void PrintProfile(struct Profile *profile, const struct Buffer *buffer, FILE *f)
{
	const char *stmt = FileBufferBase(buffer), *module = PrimaryBufferFileName(buffer);
	
	fprintf(f, "---- Profile of %s ----\n", module);

	while(*stmt != NUL && WithinFileBuffer(buffer, stmt)) {
		const char *thisModule;
		int line;
		bool displayIt;

		GetLocationInfo(buffer, stmt, &line, &thisModule);
		displayIt = line != -1 || PROFILE_PRELUDE; 

		/* Write a header if now in a different module: */

		if(displayIt && module != thisModule) {
			module = thisModule;
			fprintf(f, "---- Module %s ----\n", module);
		}

		/* Write the execution count and time: */
		
		if(displayIt) { 
			QString key;
			const struct Statistics *st; 
			
			CreateKey(&key, stmt);	
			st = profile->table != NULL ? HtLookUp(profile->table, &key) : NULL;
			QsDispose(&key);
			
			fprintf(f, "[%10lu %16.6f] ", st == NULL ? 0 : st->executionCount,
				st == NULL ? 0 : st->totalTime);

#ifdef DEBUG
			PrintStatementCacheStatus(f, stmt);
			PrintUntakenBranchCacheInfo(f, stmt);
#endif
		}
	
		/* Write the statement and move to the next one: */

		PrintStatementLine(f, &stmt, displayIt);

		if(*stmt != NUL)
			++stmt;
	}
	
	fprintf(f, "----\n");
}
