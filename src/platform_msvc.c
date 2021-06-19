/****** platform_msvc.c ******/

/*
	$VER: platform_msvc.c 0.16A (7.22.2013)

	Platform-specific functions for Microsoft Visual C++ - assumes a Windows executable target.
*/

 /* Test _MSC_VER because _WIN32 is now defined in MinGW. */
#ifdef _MSC_VER

#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <stdint.h>
#include <fcntl.h>
#include <io.h>
#include <windows.h>
#include <shlobj.h>
#include <sys/types.h> /* Needs to be included before stat.h */
#include <sys/stat.h>
#include <direct.h>
#include "common.h"
#include "platform.h"

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

size_t PfAvailMem(void) 
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

/* See comment in header on return value. */
int PfVisitFilesAt(const char *directoryName,
			 bool (*act)(const char *name, bool subDir, void *data),
			 void *dataParam,
			 int objectCountLimit)
{
	int result = INT_MIN;
	WIN32_FIND_DATAA fd;
	HANDLE handle = NULL;
	char path[2048];

	sprintf(path, "%s\\*.*", directoryName);

	if((handle = FindFirstFileA(path, &fd)) != INVALID_HANDLE_VALUE) {
		bool keepScanning = TRUE;

		do {
			if(strcmp(fd.cFileName, PF_CUR_DIR) != 0
				&& strcmp(fd.cFileName, PF_PARENT_DIR) != 0) {
				strcpy(path, fd.cFileName);
				keepScanning = (*act)(path, (fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) != 0, dataParam);
			}
		}
		while(--objectCountLimit >= -1
		&& keepScanning
		&& FindNextFileA(handle, &fd));

		FindClose(handle);

		/* 1 --> success; 0 --> stopped because limit exceeded */
		result = objectCountLimit >= -1 || !keepScanning;
	}
	
	return result;
}

/* Support both Windows and Unix style directory separators, because '/' is so 
commonly accepted on Windows now by utilities originating on Unix-like systems. */
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

/* Not particularly robust, but suffices for current uses - */
int PfFilenameCmp(const char *s, const char *t)
{
	return _stricmp(s, t);
}

/* TODO strictly, should support long filename escape at start */
bool PfIsAbsolutePath(const char *name)
{
	return PfIsPathSep(name[0]) /* Also covers UNC paths - '\\server\xyz' etc. */
		|| (isalpha(name[0]) && name[1] == ':' && PfIsPathSep(name[2]));
		/* In MS-DOS/Windows, "C:foo.txt" refers to foo.txt in the current directory of C: drive.
			"C:\foo.txt" is an absolute path referring to foo.txt in the root of C: drive. */
}

bool PfIsExplicitlyCwdRelativePath(const char *name)
{
	return (name[0] == PF_CUR_DIR[0] && PfIsPathSep(name[1]))
		|| (isalpha(name[0]) && name[1] == ':' && !PfIsPathSep(name[2])); /* Per above. */
}

bool PfIsDirectory(const char *name)
{
	struct _stat st;
	return _stat(name, &st) == 0 && (_S_IFDIR & st.st_mode);
}

int __cdecl PfChangeWorkingDirectory(const char *path)
{
	return _chdir(path);
}

bool PfFileExists(const char *name)
{
	struct _stat st;
	return _stat(name, &st) == 0 && (_S_IFMT & st.st_mode);
}

long PfFileLength(const char *name)
{
	struct _stat st;
	return _stat(name, &st) == 0 ? st.st_size : -1;
}

bool PfChangeFileBinaryMode(FILE *fh, bool binary)
{
	return _setmode(_fileno(fh), binary ? _O_BINARY : _O_TEXT) != -1;
}

bool PfIsInteractive(FILE *fh)
{
	return _isatty(_fileno(fh)) != 0;
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

#define WINDOWS_DEFAULT_MAX_PATH_LEN 262

const char *PfHomeDirectory(void)
{
	static char path[WINDOWS_DEFAULT_MAX_PATH_LEN + 1] = "";

	if(strlen(path) == 0) {
		if(getenv("USERPROFILE") != NULL) {
			path[WINDOWS_DEFAULT_MAX_PATH_LEN] = NUL;
			strncpy(path, getenv("USERPROFILE"), WINDOWS_DEFAULT_MAX_PATH_LEN);
		}
	}

	return path[0] != NUL ? path : NULL;
}

bool PfRepresentsStdin(const char *name)
{
	return _stricmp(name, "con") == 0;
}

void PfSleep(unsigned micros)
{
	Sleep((DWORD)micros / 1000);
}

#endif /* _MSC_VER */
