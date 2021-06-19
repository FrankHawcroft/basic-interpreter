/****** platform_generic.c ******/

/*
	$VER: platform_generic.c 0.16A (5.1.2014)

	The 'generic' platform. Macros allow different implementations to be used for other platforms where desired.
*/

#include <string.h>
#include <math.h>
#include <time.h>
#include <stdlib.h>
#include "common.h"
#include "platform.h"
#include "platform_mingw_private.h"
#include "platform_amiga_private.h"
#include "platform_msvc_private.h"
#include "platform_generic_private.h" /* Must be included last! */

#if defined(_POSIX_SOURCE) || defined(_GNU_SOURCE) || defined(__GNUC__)
#include <sys/time.h>
/* #include <sys/resource.h> */
#endif

#if PF_USE_GENERIC_DIRECTORY_SCANNING
#include <dirent.h>
#include <errno.h>
#endif

#if PF_USE_GENERIC_FILESYSTEM_INFO_ACCESS
#include <sys/stat.h>
#include <fcntl.h>
#endif

#if PF_USE_GENERIC_MISCELLANEOUS_FUNCTIONS || PF_USE_GENERIC_MEMORY_ALLOCATION
#include <unistd.h>
#ifndef __GNUC__
#include <ulimit.h>	
#endif /* ndef __GNUC__ */
#endif /* PF_USE_GENERIC_MISCELLANEOUS_FUNCTIONS */

#if PF_USE_GENERIC_PATH_CONVENTIONS

const char PF_PATH_SEP[] = "/";
const char PF_CUR_DIR[] = ".";
const char PF_PARENT_DIR[] = "..";
const char PF_ROOT_DIR[] = "/";

#endif /* PF_USE_GENERIC_PATH_CONVENTIONS */

/****** Functions that apply to all currently supported platforms ******/

bool PfIsSpecialDirSpec(const char *path)
{
  return path != NULL 
    && (PfFilenameCmp(path, PF_CUR_DIR) == 0
	 || PfFilenameCmp(path, PF_PARENT_DIR) == 0
	 || PfFilenameCmp(path, PF_ROOT_DIR) == 0);
}

void PfJoinPath(char *path, const char *append)
{
	if(strlen(path) > 0 && !PfIsPathSep(path[strlen(path) - 1]) && !PfIsPathSep(append[0]))
		strcat(path, PF_PATH_SEP);
	strcat(path, append);
}

/****** Functions that may or may not be used, depending on the target platform ******/

#if defined(VBCC) || defined(__clang__)

#include <ctype.h>
#include <stdlib.h>

int stricmp(const char *s, const char *t)
{
	if(s == NULL || t == NULL)
		return s == t ? 0 : (s != NULL ? 1 : -1);
	for( ; toupper(*s) == toupper(*t) && *s != NUL && *t != NUL; s++, t++)
		;
	return toupper(*s) - toupper(*t);
}

int strnicmp(const char *s, const char *t, size_t n)
{
	size_t count; 
	if(s == NULL || t == NULL)
		return s == t ? 0 : (s != NULL ? 1 : -1);
	for(count = 0; count < n && toupper(*s) == toupper(*t) && *s != NUL && *t != NUL; s++, t++, count++)
		;
	return count == n ? 0 : toupper(*s) - toupper(*t);
}

char *strdup(const char *s)
{
	if(s == NULL)
		return NULL;
	else {
		size_t length = strlen(s) + 1;
		char *cpy = malloc(length);
		return cpy != NULL ? strcpy(cpy, s) : NULL;
	}
}

#endif /* VBCC || __clang__ */

#if PF_USE_GENERIC_INITIALISATION

void PfStart(void) { }
void PfFinish(void) { }

#endif /* PF_USE_GENERIC_INITIALISATION */

#if PF_USE_GENERIC_MEMORY_ALLOCATION

void *PfAllocMem(size_t size)
{
	void *p = sbrk(size);
	return p == (void *)-1 ? NULL : p;
}

void PfFreeMem(void *ptr, size_t size)
{
	/* Never give it back. */
}

size_t PfAvailMem() 
{
	size_t avail = 32 * 1024 * 1024;
	
#if !(__APPLE__ && __MACH__) && (defined(_POSIX_SOURCE) || defined(_GNU_SOURCE) || defined(__GNUC__))
	struct rlimit rlim;

	if(getrlimit(RLIMIT_AS, &rlim) == 0)
		avail = rlim.rlim_cur;
#elif !defined(__GNUC__) && !(__APPLE__ && __MACH__)
	/* Assume an older Unix system. */
	avail = ulimit(UL_GMEMLIM, 0) - sbrk(0);
#endif

	return avail;
}

#endif /* PF_USE_GENERIC_MEMORY_ALLOCATION */

#if PF_USE_GENERIC_DIRECTORY_SCANNING

/* See comment in header on return value. */
int PfVisitFilesAt(const char *directoryName,
		     bool (*act)(const char *name, bool subDir, void *data),
		     void *dataParam,
		     int objectCountLimit)
{
	int result = 1;
	DIR *dh;
	
	if((dh = opendir(directoryName))) {
		struct dirent *de;
		bool keepScanning = TRUE;
		
		/* http://pubs.opengroup.org/onlinepubs/009695399/functions/readdir_r.html:
		"Applications wishing to check for error situations should set errno to 0 before calling readdir().
		If errno is set to non-zero on return, an error occurred." */
		errno = 0;
		while(--objectCountLimit >= 0 && keepScanning && (de = readdir(dh))) {
			/* This will not work properly if the caller recursively scans directories without ensuring
			the directoryName is the full concatenated path, but there isn't a standard way to tell if
			something's a directory or not based on the dirent. */
			char *entryPath = malloc(strlen(directoryName) + 2 * strlen(PF_PATH_SEP) + strlen(de->d_name) + 1);
			
			if(entryPath == NULL)
				keepScanning = FALSE;
			else {
				strcpy(entryPath, directoryName);
				if(!PfIsDirSpec(entryPath))
					strcat(entryPath, PF_PATH_SEP);
				strcat(entryPath, de->d_name);
				
				if((keepScanning = (*act)(de->d_name, PfIsDirectory(entryPath), dataParam)))
					errno = 0;
					
				free(entryPath);
			}
		}
		
		/* 1 --> success; 0 --> stopped because limit exceeded or error */
		result = objectCountLimit >= 0 || !keepScanning;

		if(result == 0) {
			if(errno != 0
#ifdef ENOFILE
			&& errno != ENOFILE
#endif
#if defined(ENOENT) && ENOENT != ENOFILE /* Added for GCC/MinGW compatibility */
			&& errno != ENOENT
#endif
			)
				result = -errno;
		}

		closedir(dh);
	}
	else
		result = -errno;

	return result;
}

#endif /* PF_USE_GENERIC_DIRECTORY_SCANNING */

#if PF_USE_GENERIC_PATH_CONVENTIONS

bool PfIsAbsolutePath(const char *name)
{
	return name != NULL && PfIsPathSep(name[0]);
}

bool PfIsExplicitlyCwdRelativePath(const char *name)
{
	return name != NULL && name[0] == PF_CUR_DIR[0] && PfIsPathSep(name[1]);
}

bool PfIsDirSpec(const char *path) 
{
	char last = path == NULL || strlen(path) == 0 ? NUL : path[strlen(path) - 1];
	return last == PF_PATH_SEP[0];
}

bool PfIsPathSep(char c)
{
	return c == PF_PATH_SEP[0];
}

/* Assume case matters. */
int PfFilenameCmp(const char *s, const char *t)
{
	return strcmp(s, t);
}

#endif /* PF_USE_GENERIC_PATH_CONVENTIONS */

#if PF_USE_GENERIC_FILESYSTEM_INFO_ACCESS

bool PfIsDirectory(const char *name)
{
	struct stat st;
	return stat(name, &st) == 0 && S_ISDIR(st.st_mode);
}

int PfChangeWorkingDirectory(const char *path)
{
	return chdir(path);
}

bool PfFileExists(const char *name)
{
	struct stat st;
	return stat(name, &st) == 0 && (S_ISBLK(st.st_mode) || S_ISCHR(st.st_mode) || S_ISREG(st.st_mode));
}

long PfFileLength(const char *name)
{
	struct stat st;
	return stat(name, &st) == 0 ? st.st_size : -1;
}

bool PfChangeFileBinaryMode(FILE *fh, bool binary)
{
	return TRUE;
}

bool PfIsInteractive(FILE *fh)
{
	return isatty(fileno(fh));
}

#endif /* PF_USE_GENERIC_FILESYSTEM_INFO_ACCESS */

#if PF_USE_GENERIC_EVENT_TIMING_FUNCTIONS

void PfInitSystemTimeStamp(struct PfSystemTimeStamp *timeStamp)
{
	memset(timeStamp, 0, sizeof(*timeStamp));
}

#ifdef DEBUG
void PfPrintSystemTimeStamp(const struct PfSystemTimeStamp *timeStamp)
{
	fprintf(stderr, "secs = %ld, clocks = %ld\n", timeStamp->secs, timeStamp->clocks);
}
#endif

void PfGetSystemTimeStamp(struct PfSystemTimeStamp *timeStamp)
{
	static struct PfSystemTimeStamp prev = {0, 0};

	timeStamp->secs = time(NULL);
	timeStamp->clocks = clock() % CLOCKS_PER_SEC;
		/* clock() returns the processor time used
		 by the process so far. The check below is
		 there in case less than a second has elapsed
		 between calls, so time() has not advanced,
		 but clock() has been pushed over a 'second
		 division' within the execution interval. 
		 This also ensures the 'timestamp' concept
		 has some validity on platforms where clock()
		 doesn't work properly. */
	if(timeStamp->secs == prev.secs && timeStamp->clocks < prev.clocks)
		timeStamp->clocks = prev.clocks + 1;
	
	prev = *timeStamp;

	/*fprintf(stderr, "Polling: ");
	PrintSystemTimeStamp(timeStamp);*/
}

/* If the clock runs backwards for some reason - e.g. it rolls over, or there's a time zone or daylight
	saving change - then this will result in waiting for a long time - but not sure how to deal with
	this in a simple way. */
bool PfTimeHasElapsed(
	const struct PfSystemTimeStamp *start,
	const struct PfSystemTimeStamp *end,
	float interval)
{
	double dummy;
	return difftime(end->secs, start->secs) >= interval
		|| (difftime(end->secs, start->secs) >= floor(interval)
			&& (double)(end->clocks - start->clocks) / CLOCKS_PER_SEC >= modf(interval, &dummy));
}

#endif /* PF_USE_GENERIC_EVENT_TIMING_FUNCTIONS */

#if PF_USE_GENERIC_EXECUTION_TIMING_FUNCTIONS

void PfRecordTime(PfHighResolutionTimeStamp *t)
{
	PfGetSystemTimeStamp(t);
}

float PfGetElapsedTimeSince(const PfHighResolutionTimeStamp *start)
{
	PfHighResolutionTimeStamp end;
	PfRecordTime(&end);
	/* TODO not very sensical - */
	return difftime(end->secs, start->secs) + (float)(end->clocks - start->clocks) / CLOCKS_PER_SEC;
}

#endif /* PF_USE_GENERIC_EXECUTION_TIMING_FUNCTIONS */

#if PF_USE_GENERIC_TASKING_FUNCTIONS

/* 'Multitasking' in the sense that the interpreter can be run from more than
	one thread or task at once. These tasks will share the loaded code and
	data sections - i.e. all global and static variables can potentially be
	accessed from more than one task at once, so are not safe to use unless
	they are read-only, or their access is protected by mutexes etc.
	
	The default assumption is that the runtime does not need to support
	this. Currently, only the Amiga variant does, for when the interpreter
	is made 'resident' using the AmigaDOS command of that name. */

void PfInitialiseMutex(PfMutex *mutex) { *mutex = 0; }

void PfBeginExclusiveExecution(PfMutex *mutex) { }

void PfEndExclusiveExecution(PfMutex *mutex) { }

PfTaskIdentifier PfGetCurrentTaskIdentifier(void) { return 1; }

#endif /* PF_USE_GENERIC_TASKING_FUNCTIONS */

#if PF_USE_GENERIC_MISCELLANEOUS_FUNCTIONS

const char *PfHomeDirectory()
{
	return "~";
}

bool PfRepresentsStdin(const char *name)
{
	return strcmp(name, "-") == 0;
}

void PfSleep(unsigned micros)
{
	usleep(micros);
}

#endif /* PF_USE_GENERIC_MISCELLANEOUS_FUNCTIONS */
