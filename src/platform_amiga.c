/****** platform_amiga.c ******/

/*
	$VER: platform_amiga.c 0.16A (4.30.2014)

	Platform-specific functions for AmigaOS.
*/

#ifdef AMIGA

#include <clib/exec_protos.h>
#include <clib/dos_protos.h>
#include <clib/alib_protos.h>
#include <clib/timer_protos.h>
#include <exec/memory.h>
#include <exec/tasks.h>
#include <exec/semaphores.h>
#include <dos/dos.h>
#include <devices/timer.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include "common.h"
#include "platform.h"
#include "platform_amiga_private.h"

#ifdef VBCC
extern int stricmp(const char *, const char *);
extern int strnicmp(const char *, const char *, size_t);
#endif

static const char INTERPRETER_VERSION[] = "\0$VER: BASIC 0.16A (7.9.2021)";

const char PF_PATH_SEP[] = "/";
const char PF_CUR_DIR[] = "";
const char PF_PARENT_DIR[] = "/";
const char PF_ROOT_DIR[] = ":";

#ifndef VBCC
struct Library *DOSBase, *IntuitionBase, *GfxBase, *LayersBase, *DiskFontBase;
#endif

 /* To avoid allocating/freeing globals twice if resident, and reusing
	the data segment. Problem is if the interpreter crashes or otherwise
	exits without calling PfFinish. Also this is not multitasking-safe
	(see where it is used in PfStart) but on the 68000 I think we will
	get away with it. */
static int m_InstanceCount = 0;

static BPTR m_OriginalCD = -1L; /* TODO reentrant */

struct Library *TimerBase = NULL;
struct timerequest *TimerIO = NULL;

static void delete_timer(struct timerequest *tr )
{
	struct MsgPort *tp;

	if(tr != NULL ) {
		tp = tr->tr_node.io_Message.mn_ReplyPort;

		if (tp != 0)
			DeletePort(tp);

		CloseDevice( (struct IORequest *) tr );
		DeleteExtIO( (struct IORequest *) tr );
    }
}

static struct timerequest *create_timer( ULONG unit )
{
	/* return a pointer to a timer request.  If any problem, return NULL */
	LONG error;
	struct MsgPort *timerport;

	timerport = CreatePort( 0, 0 );
	if (timerport == NULL )
		return( NULL );

	TimerIO = (struct timerequest *)
		CreateExtIO( timerport, sizeof( struct timerequest ) );
	if (TimerIO == NULL ) {
		DeletePort(timerport);   /* Delete message port */
		return( NULL );
	}

	error = OpenDevice( TIMERNAME, unit,(struct IORequest *) TimerIO, 0L );
	if (error != 0 ) {
		delete_timer( TimerIO );
		return( NULL );
	}
	return( TimerIO );
}

void GetAmigaClock(unsigned long *secs, unsigned long *mics)
{
	*secs = *mics = 0;
	
	if(TimerIO == NULL) {
		if(create_timer(UNIT_MICROHZ) != NULL)
			TimerBase = (struct Library *)TimerIO->tr_node.io_Device;
	}
	
	if(TimerBase != NULL) {
		struct timeval tv;
		GetSysTime(&tv);
		*secs = tv.tv_secs;
		*mics = tv.tv_micro;
	}
}

float SubtractAmigaClock(unsigned long secA, unsigned long micA, unsigned long secB, unsigned long micB)
{
	struct timeval a, b;
	a.tv_secs = secA; a.tv_micro = micA;
	b.tv_secs = secB; b.tv_micro = micB;
	SubTime(&a, &b);
	return a.tv_secs + a.tv_micro / 1000000.0;
}

void PfStart()
{
	if(++m_InstanceCount == 1) {
#ifndef VBCC
		if((DOSBase = OpenLibrary("dos.library", 0L)) == NULL
		|| (IntuitionBase = OpenLibrary("intuition.library", 0L)) == NULL
		|| (GfxBase = OpenLibrary("graphics.library", 0L)) == NULL
		|| (LayersBase = OpenLibrary("layers.library", 0L)) == NULL
		|| (DiskFontBase = OpenLibrary("diskfont.library", 0L)) == NULL) {
			/* Assume the C lib is available at this point but nothing else. If dos.library failed to open,
			   chances are this error message will fail anyway ... */
			fputs("Unable to open a needed Amiga system library  - need dos, intuition, graphics, layers, diskfont. Exiting.\n", stderr);
			exit(21);
		}
#endif
		TimerBase = NULL;
		TimerIO = NULL;
	}

#ifdef __GNUC__
	/* Try to make console I/O slightly quicker than treacle flowing over a glacier. */
	setvbuf(stderr, NULL, _IOLBF, 128);
	setvbuf(stdout, NULL, PfIsInteractive(stdout) ? _IOLBF : _IOFBF, PfIsInteractive(stdout) ? 128 : 512);
#endif
}

void PfFinish()
{
	if(m_OriginalCD != -1L)
		CurrentDir(m_OriginalCD);
	
	if(--m_InstanceCount == 0) {
#ifndef VBCC
		if(DOSBase != NULL)
			CloseLibrary(DOSBase);
		if(IntuitionBase != NULL)
			CloseLibrary(IntuitionBase);
		if(GfxBase != NULL)
			CloseLibrary(GfxBase);
		if(LayersBase != NULL)
			CloseLibrary(LayersBase);
#endif

		TimerBase = (struct Library *)(-1);
		delete_timer(TimerIO);
	}
}

void *PfAllocMem(size_t size)
{
	return AllocMem(size, MEMF_ANY);
}

void PfFreeMem(void *ptr, size_t size)
{
	FreeMem(ptr, size);
}

size_t PfAvailMem() 
{
	return AvailMem(MEMF_ANY);
}

#ifndef E_OK
#define E_OK 0
#endif

static int MapAmigaErrorToErrNo(LONG ioErr)
{
	switch(ioErr)
	{
		case 0: return E_OK;
		case ERROR_BAD_NUMBER: return EBADF;
		case ERROR_DIR_NOT_FOUND: return ENOTDIR;
		case ERROR_OBJECT_NOT_FOUND: return ENOENT;
		case ERROR_OBJECT_IN_USE: return EBUSY;
		case ERROR_OBJECT_EXISTS: return EEXIST;
		case ERROR_DISK_WRITE_PROTECTED: return EROFS;
		case ERROR_RENAME_ACROSS_DEVICES: return EXDEV;
		case ERROR_DEVICE_NOT_MOUNTED: return ENODEV;
		case ERROR_WRITE_PROTECTED: return EACCES;
		case ERROR_READ_PROTECTED: return EACCES;
		case ERROR_DISK_FULL: return ENOSPC;
		default: return EIO; /* Assumes context is file operations. */
	}
}

#if !PF_USE_GENERIC_DIRECTORY_SCANNING

/* See comment in header on return value. */
int PfVisitFilesAt(const char *directoryName,
		     bool (*act)(const char *name, bool isDir, void *data),
		     void *dataParam,
		     int objectCountLimit)
{
	int result = 1;
	BPTR lock;
	struct FileInfoBlock info;
	
	lock = Lock(directoryName, ACCESS_READ);
	if(lock != 0L && Examine(lock, &info)) {
		bool keepScanning = TRUE;

		while(--objectCountLimit >= 0 && keepScanning && ExNext(lock, &info))
			keepScanning = (*act)(info.fib_FileName, info.fib_DirEntryType > 0, dataParam);
		
		/* 1 --> success; 0 --> stopped because limit exceeded or error */
		result = objectCountLimit >= 0 || !keepScanning;
		
		if(result == 0) {
			LONG ioErr;
			if(ExNext(lock, &info) == 0L && (ioErr = IoErr()) != ERROR_NO_MORE_ENTRIES)
				result = -MapAmigaErrorToErrNo(ioErr);
		}
	}
	else
		result = -MapAmigaErrorToErrNo(IoErr());

	if(lock != 0)
		UnLock(lock);
	
	return result;
}

#endif /* !PF_USE_GENERIC_DIRECTORY_SCANNING */

bool PfIsAbsolutePath(const char *name)
{
	return strchr(name, ':') != NULL;
}

/* A slightly tricky concept with Amiga paths. Simple approach suffices for now. */
bool PfIsExplicitlyCwdRelativePath(const char *name)
{
	return FALSE;
}

bool PfIsDirSpec(const char *path) 
{
	char last = path == NULL || strlen(path) == 0 ? NUL : path[strlen(path) - 1];
	return path != NULL && (last == NUL || PfIsPathSep(last));
}

bool PfIsPathSep(char c)
{
	return c == '/' || c == ':';
}

int PfFilenameCmp(const char *s, const char *t)
{
	return stricmp(s, t);
}

bool PfIsDirectory(const char *name)
{
	BPTR lock;
	struct FileInfoBlock info;
	bool isDir = FALSE;
	
	if(lock = Lock(name, ACCESS_READ)) {
		isDir = Examine(lock, &info) && info.fib_DirEntryType > 0;
		UnLock(lock);
	}
	
	return isDir;
}

int PfChangeWorkingDirectory(const char *path)
{
	BPTR newCD = Lock(path, ACCESS_READ);
	if(newCD == 0L) {
		errno = MapAmigaErrorToErrNo(IoErr());
		return 1;
	}
	else {
		BPTR oldCD = CurrentDir(newCD);
		if(m_OriginalCD == -1L)
			m_OriginalCD = oldCD;
		else
			UnLock(oldCD);
		return 0;
	}
}

bool PfFileExists(const char *name)
{
	BPTR lock = Lock(name, ACCESS_READ);
	bool exists = lock != 0L;
	if(exists)
		UnLock(lock);
	return exists;
}

long PfFileLength(const char *name)
{
	BPTR lock;
	long length = -1;
	struct FileInfoBlock info;
	
	lock = Lock(name, ACCESS_READ);
	if(lock != 0L && Examine(lock, &info) && info.fib_DirEntryType < 0)
		length = info.fib_Size;
	if(lock != 0L)
		UnLock(lock);
	
	return length;
}

bool PfChangeFileBinaryMode(FILE *fh, bool binary)
{
	return TRUE;
}

bool PfIsInteractive(FILE *fh)
{
#ifdef VBCC
	return IsInteractive((BPTR)fh->filehandle);
#else
	return fh == stdin || fh == stdout || fh == stderr;
#endif
}

void PfInitSystemTimeStamp(struct PfSystemTimeStamp *timeStamp)
{
	memset(timeStamp, 0, sizeof(*timeStamp));
}

#ifdef DEBUG
void PfPrintSystemTimeStamp(const struct PfSystemTimeStamp *timeStamp)
{
	fprintf(stderr, "secs = %ld, mics = %ld\n", timeStamp->secs, timeStamp->mics);
}
#endif

void PfGetSystemTimeStamp(struct PfSystemTimeStamp *timeStamp)
{
	GetAmigaClock(&timeStamp->secs, &timeStamp->mics);
}

bool PfTimeHasElapsed(
	const struct PfSystemTimeStamp *start,
	const struct PfSystemTimeStamp *end,
	float interval)
{
	return SubtractAmigaClock(end->secs, end->mics, start->secs, start->mics) >= interval;
}

time_t PfConvertToTimeTTickCount(const struct PfSystemTimeStamp *timeStamp)
{
	return timeStamp->secs + 252460800; /* seconds between Unix and Amiga epochs - 1/1/1970 and 1/1/1978 */
}

void PfRecordTime(PfHighResolutionTimeStamp *t)
{
	PfGetSystemTimeStamp(t);
}

float PfGetElapsedTimeSince(const PfHighResolutionTimeStamp *start)
{
	PfHighResolutionTimeStamp end;
	PfRecordTime(&end);
	return SubtractAmigaClock(end.secs, end.mics, start->secs, start->mics);
}

#if !PF_USE_GENERIC_TASKING_FUNCTIONS

void PfInitialiseMutex(PfMutex *mutex)
{	
	memset(mutex, 0, sizeof(*mutex));
	InitSemaphore(mutex);
}

void PfBeginExclusiveExecution(PfMutex *mutex)
{
	ObtainSemaphore(mutex);
}

void PfEndExclusiveExecution(PfMutex *mutex)
{
	ReleaseSemaphore(mutex);
}

PfTaskIdentifier PfGetCurrentTaskIdentifier(void)
{
	return FindTask(NULL);
}

#endif /* PF_USE_GENERIC_TASKING_FUNCTIONS */

const char *PfHomeDirectory()
{
	return NULL;
}

bool PfRepresentsStdin(const char *name)
{
	/* TODO not quite right; these naming conventions are specifically for an interactive console */
	return strcmp(name, "*") == 0 || strnicmp(name, "con:", 4) == 0;
}

void PfSleep(unsigned long micros)
{
	Delay(micros / (1000000 / 50));
}

void PfSleepUntilEvent(PfEventNotificationHandle mask)
{
	Wait(mask);
}

bool PfTestAndClearBreakSignal()
{
	if(SetSignal(0L, 0L) & SIGBREAKF_CTRL_C) {
		SetSignal(0L, SIGBREAKF_CTRL_C);
		return TRUE;
	}
	else
		return FALSE;
}

PfEventNotificationHandle PfGetBreakEventNotificationHandle(void)
{
	return FindTask(NULL)->tc_SigAlloc;
}

#endif /* AMIGA */
