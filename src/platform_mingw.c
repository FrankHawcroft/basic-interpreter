/****** platform_mingw.c ******/

/*
	$VER: platform_mingw.c 0.16A (5.1.2013)

	Platform-specific functions for the Minimal GNU System for Windows.
*/

#ifdef __MINGW32__

#include <string.h>
#include <ctype.h>
#include <stdint.h>
#include <io.h>
#include <windows.h>
#include <shlobj.h>
#include "common.h"
#include "platform.h"

/* _WIN32 is now defined in MinGW - I guess because it now includes Windows header files. */

/* TODO could be argued that the path sep should be / instead of \ for MinGW */
const char PF_PATH_SEP[] = "\\";
const char PF_CUR_DIR[] = ".";
const char PF_PARENT_DIR[] = "..";
const char PF_ROOT_DIR[] = "\\";

void *PfAllocMem(size_t size)
{
	return HeapAlloc(GetProcessHeap(), 0, size);
}

void PfFreeMem(void *ptr, size_t size)
{
	HeapFree(GetProcessHeap(), 0, ptr);
}

size_t PfAvailMem() 
{
	size_t avail = 32 * 1024 * 1024;
#if _WIN32_WINNT >= 0x0500
	MEMORYSTATUSEX memStatus;

	if(GlobalMemoryStatusEx(&memStatus))
		avail = memStatus.ullAvailVirtual > SIZE_MAX ? SIZE_MAX : (size_t)memStatus.ullAvailVirtual;
#else
	MEMORYSTATUS memStatus;

	GlobalMemoryStatus(&memStatus);
	avail = memStatus.dwAvailVirtual;
#endif
	return avail; 
}

/* Supports both Windows and Unix style directory separators. */
bool PfIsDirSpec(const char *path) 
{
	char last = path == NULL || strlen(path) == 0 ? NUL : path[strlen(path) - 1];
	return last == '/' || last == '\\';
}

/* ':' is in a somewhat ambiguous category on Windows ... not quite sure how to handle it. */
bool PfIsPathSep(char c)
{
	return c == '/' || c == '\\';
}

int PfFilenameCmp(const char *s, const char *t)
{
	return _stricmp(s, t);
}

/* Also accept the Unix '/' root dir convention on Windows. */
bool PfIsAbsolutePath(const char *name)
{
	return PfIsPathSep(name[0]) /* Also covers UNC paths - '\\server\xyz' etc. */
		|| (isalpha(name[0]) && name[1] == ':' && PfIsPathSep(name[2])); /* Only single-letter drives. */
}

bool PfIsExplicitlyCwdRelativePath(const char *name)
{
	return name[0] == PF_CUR_DIR[0] && PfIsPathSep(name[1]);
}

void PfRecordTime(PfHighResolutionTimeStamp *t)
{
	QueryPerformanceCounter(t);
}

/* From https://docs.microsoft.com/en-us/windows/win32/sysinfo/acquiring-high-resolution-time-stamps#using-qpc-in-native-code */
float PfGetElapsedTimeSince(const PfHighResolutionTimeStamp *start)
{
	LARGE_INTEGER frequency, endingTime, elapsedMicroseconds;

	QueryPerformanceFrequency(&frequency); 
	QueryPerformanceCounter(&endingTime);
	elapsedMicroseconds.QuadPart = endingTime.QuadPart - start->QuadPart;
	elapsedMicroseconds.QuadPart *= 1000000;
	return (float)elapsedMicroseconds.QuadPart / frequency.QuadPart;
}

const char *PfHomeDirectory()
{
	static char path[MAX_PATH] = "";

	if(SHGetFolderPathA(NULL, CSIDL_APPDATA, NULL, 0, path) == S_OK)
		return path;
	else
		return NULL;
}

bool PfRepresentsStdin(const char *name)
{
	return strcmp(name, "-") == 0; /* Arguably, also 'CON' etc. */
}

void PfSleep(unsigned micros)
{
	Sleep((DWORD)micros / 1000);
}

#endif /* __MINGW32__ */
