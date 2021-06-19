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

/* Controls whether counts are displayed for prelude. */
#define PROFILE_PRELUDE TRUE

/* Controls whether timing is displayed. */
#define PROFILE_SHOWS_TIMING TRUE

/* Statistics for a range of statements. */
struct Statistics {
	struct Statistics *next;
	unsigned long executionCount;
	float seconds;
	const char *low, *high;
		/* Bounds of the range of statements (inclusive) for these statistics - 
		the beginning of the first statement, and the end of the last statement,
		to which these stats apply. */
};

void InitProfile(struct Statistics **stats)
{
	assert(stats != NULL);
	
	*stats = NULL;
}

void DisposeProfilingData(struct Statistics **stats)
{
	struct Statistics *current, *savedNext;

	assert(stats != NULL);
	
	for(current = *stats; current != NULL; current = savedNext) {
		savedNext = current->next;
		Dispose(current);
	}
	
	InitProfile(stats);
}

static struct Statistics *CreateStats(
	const char *low,
	const char *high, 
	unsigned long count,
	float secs,
	struct Statistics *next,
	struct Statistics **link)
{
	struct Statistics *st = New(sizeof(*st));
	st->low = low;
	st->high = high;
	st->executionCount = count;
	st->seconds = secs;
	st->next = next;
	if(link != NULL)
		*link = st;
	return st;
}

/* Increment the count of the number of times the given statement has been executed. */
void IncrExecutionCount(struct Statistics **stats, const struct Buffer *buffer,
	const char *stmt, const char *end, float seconds)
{
	struct Statistics *before, *containing, *after;
	
	assert(stats != NULL);
	
#if !PROFILE_SHOWS_TIMING
	seconds = 0;
#endif

	/* Create sentinel list members if this is the first call: */

	if(*stats == NULL) {
		struct Statistics *highest
			= CreateStats(FileBufferExtent(buffer) + 1, FileBufferExtent(buffer) + 1, 0, 0, NULL, NULL);
		CreateStats(FileBufferBase(buffer) - 1, FileBufferBase(buffer) - 1, 0, 0, highest, stats); /* 'lowest' */
	}

	/* Find the range in which the given stmt lies ('containing'), or, if 
	none, the two ranges which are closest on either side ('before' and
	'after'). The list is maintained in ascending order of stmt addr. */

	before = *stats;
	containing = NULL;
	after = before->next;

	while(stmt >= after->low) {
		if(stmt <= after->high)
			containing = after;
		else
			before = after;
		after = after->next;
	}

	/* Update/add the execution count.
	There are two main cases:
		1. stmt fell into an existing range (containing != NULL).
		2. stmt wasn't in an existing range (containing == NULL). */

	if(containing != NULL) {
		/* Two subcases are handled:
			1. Might be able to 'give the statement away' to
		an adjacent range. This might leave the containing range empty,
		if it was only 1 stmt wide. (This case will tend to apply when
		executing WHILE loops, tail-recursive subprograms, etc.)
			2. The containing range needs to be split. (This tends
		to be caused by GOTOs and (less often) GOSUBs.) */

		if(stmt - 1 == before->high
		&& before->executionCount > 0
		&& (before->seconds == 0 || seconds == 0)
		&& containing->executionCount == before->executionCount - 1) {
			/* Give to preceding range. */

			before->high = end;
			before->seconds += seconds;
			containing->low = end + 1;
		}
		else if(end + 1 == after->low
		&& after->executionCount > 0
		&& (after->seconds == 0 || seconds == 0)
		&& containing->executionCount == after->executionCount - 1) {
			/* Give to following range. This subcase is actually
			much less commmon than the above one, due to the 
			forward flow of program control. It might occasionally
			be caused by a direct jump though. */

			after->low = stmt;
			after->seconds += seconds;
			containing->high = stmt - 1;
		}
		else {
			/* Split the containing range into two or three new ranges: */

			if(stmt > containing->low) {
				CreateStats(containing->low, stmt - 1, containing->executionCount, containing->seconds, containing, &before->next);
				containing->seconds = 0;
			}
			
			if(end < containing->high) {
				CreateStats(end + 1, containing->high, containing->executionCount, containing->seconds, after, &containing->next);
				containing->seconds = 0;
			}
	
			containing->low = stmt;
			containing->high = end;
			++containing->executionCount;
			containing->seconds += seconds;
		} /* if must split range */

		/* If the line was given away (and therefore containing->low
		increased or containing->high decreased), may be able to 
		'garbage collect' the containing range: */

		if(containing->low > containing->high) {
			before->next = after;
			Dispose(containing);
		}
	}
	else { /* containing == NULL */
		/* Need to create a new range, unless can integrate the line
		into the preceding or following range (rare except near the
		beginning of a program). */

		if(before->high == stmt - 1 && before->executionCount == 1 && before->seconds == 0 && seconds == 0)
			before->high = end;
		else if(after->low == end + 1 && after->executionCount == 1 && after->seconds == 0 && seconds == 0)
			after->low = stmt;
		else 
			CreateStats(stmt, end, 1, seconds, after, &before->next);
	} /* if no containing range */
}

/* Print a statement line for a profiling report.
	Continuations (_) are treated as separate statements.
	Multiple separated statements on one line (|) are treated as one statement.
This keeps a 1-1 correspondence of source and program lines in the profiling
report. It also makes this function very simple: it prints all characters until
a newline or NUL is encountered. 

The reallyPrint parameter controls whether the code is actually printed, or this 
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

/* Finds the range for the position in code supplied, using a linear search.
	If not profiling, will always return NULL. */
static struct Statistics *StatsFor(struct Statistics *stats, const char *stmt)
{
	struct Statistics *st;
	for(st = stats; st != NULL && stmt > st->high; st = st->next)
		;
	return st == NULL || stmt < st->low ? NULL : st;
}

#ifdef DEBUG
extern void PrintStatementCacheStatus(FILE *, const char *);
extern void PrintUntakenBranchCacheInfo(FILE *, const char *);
#endif

/* Prints a profiling report, consisting of the program text with execution 
counts prepended to each statement. */
void PrintProfile(struct Statistics *stats, const struct Buffer *buffer, FILE *f)
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
			fprintf(f, "---- Merged module %s ----\n", module);
		}

		/* Write the execution count and time: */
		
		if(displayIt) {
			const struct Statistics *st = StatsFor(stats, stmt);
			fprintf(f, "[%10lu %8.6f] ", st == NULL ? 0 : st->executionCount,
				st == NULL ? 0 : st->seconds);

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

#ifdef DEBUG

/* Debugging help - print out the list of statistics. */

#define NUM_RANGES_PER_LINE 4

void PrintProfilerStatisticsList(struct Statistics *stats, const struct Buffer *buffer)
{
	const struct Statistics *st;

	fprintf(stderr, "---- Profiler statistics entries:\n");
	for(st = stats; st != NULL; st = st->next) {
		const char *file;
		int numOnLine = 0, lowLine, highLine;

		GetLocationInfo(buffer, st->low, &lowLine, &file);
		GetLocationInfo(buffer, st->high - 1, &highLine, &file);

		fprintf(stderr, "[%d,%d] : %lu, %f; ", lowLine, highLine, st->executionCount, st->seconds);

		if(++numOnLine == NUM_RANGES_PER_LINE) {
			fprintf(stderr, "\n");
			numOnLine = 0;
		}
	}
	fprintf(stderr, "***\n");
}

#endif /* DEBUG */
