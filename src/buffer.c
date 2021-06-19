/****** buffer.c ******/

/* 
	$VER: buffer.c 0.16A (5.7.2014)

	Implementation of the text buffer.
*/

#include <string.h>
#include <stdio.h>
#include "common.h"
#include "platform.h"
#include "heap.h"
#include "buffer.h"

/* Because the buffer is simply an array of characters, an auxiliary structure is needed to
associate each loaded file or fragment with its region. */
struct File {
	struct File	*next;
	char *name;
	const char *start;
	const char *end; /* Last character included in the region. */
	bool hidden;
};

struct Buffer {
	struct File *firstFile; /* An unordered list. */
	char *text;
	char *endOfUsedRegion; /* Last valid character - always points to the terminating NUL. */
	size_t length; /* Total size of the text array, including unused space. */
	size_t spaceLeft; /* Space available following endOfUsedRegion. */
};

struct Buffer *CreateFileBuffer(size_t size)
{
	struct Buffer *buf = New(sizeof(*buf));
	buf->text = New(size);
	buf->length = size;
	buf->firstFile = NULL;
	ResetFileBuffer(buf);
	return buf;
}

static void DisposeFileNames(struct Buffer *buf)
{
	struct File *p, *savedNext;

	for(p = buf->firstFile; p != NULL; p = savedNext) {
		savedNext = p->next;
		if(p->name != NULL)
			Dispose(p->name);
		Dispose(p);
	}
	buf->firstFile = NULL;
}

/* 'Discards' anything in the buffer, and all buffer name entries. 
Does not free buffer memory (see DisposeFileBuffer). */
void ResetFileBuffer(struct Buffer *buf)
{
	DisposeFileNames(buf);
	buf->endOfUsedRegion = buf->text;
	buf->text[0] = NUL;
	buf->spaceLeft = buf->length - 1;
}

void DisposeFileBuffer(struct Buffer *buf)
{
	if(buf != NULL) {
		DisposeFileNames(buf);
		if(buf->text != NULL) {
			Dispose(buf->text);
			buf->text = NULL;
		}
		Dispose(buf);
	}
}

/* Gets the filename for the text nearest the start of the buffer which isn't hidden -
i.e. the 'main program' (although the buffer shouldn't know anything about BASIC programs,
so this function is something of a swiz). */
static const struct File *PrimaryBuffer(const struct Buffer *buf)
{
	struct File *firstNonHidden = NULL, *p;

	for(p = buf->firstFile; p != NULL; p = p->next)
		if(!p->hidden && (firstNonHidden == NULL || p->start < firstNonHidden->start))
			firstNonHidden = p;
	return firstNonHidden;
}
		
const char *PrimaryBufferFileName(const struct Buffer *buf)
{
	const struct File *firstNonHidden = PrimaryBuffer(buf);
	return firstNonHidden != NULL ? firstNonHidden->name : NULL;
}

const char *PrimaryBufferBase(const struct Buffer *buf)
{
	const struct File *firstNonHidden = PrimaryBuffer(buf);
	return firstNonHidden != NULL ? firstNonHidden->start : NULL;
}

bool WithinFileBuffer(const struct Buffer *buf, const char *p)
{
	return p >= FileBufferBase(buf) && p < buf->endOfUsedRegion;
}

const char *FileBufferBase(const struct Buffer *buf)
{
	return buf->text;
}

const char *EndOfUsedRegion(const struct Buffer *buf)
{
	return buf->endOfUsedRegion;
}

const char *FileBufferExtent(const struct Buffer *buf)
{
	return FileBufferBase(buf) + buf->length;
}

static bool EnsureNewline(struct Buffer *buf)
{
	assert(*buf->endOfUsedRegion == NUL);
	
	if(buf->firstFile != NULL) {
		if(buf->spaceLeft <= 1)
			return FALSE;
		
		if(buf->endOfUsedRegion > buf->text && buf->endOfUsedRegion[-1] != '\n') {
			*buf->endOfUsedRegion++ = '\n';
			*buf->endOfUsedRegion = NUL;
			--buf->spaceLeft;
		}
	}
	
	return TRUE;
}

static void AddRegionDescriptor(struct Buffer *buf, const char *name, const char *start, bool isHidden)
{
	struct File *newEntry = (struct File *)New(sizeof(struct File));
	newEntry->next = buf->firstFile;
	newEntry->name = New(strlen(name) + 1);
	memcpy(newEntry->name, name, strlen(name));
	newEntry->name[strlen(name)] = NUL;
	newEntry->start = start;
	newEntry->end = buf->endOfUsedRegion - 1;
	newEntry->hidden = isHidden;
	buf->firstFile = newEntry;
}

bool AppendToBuffer(struct Buffer *buf, const char *text)
{
	char *start = buf->endOfUsedRegion;
	size_t length = strlen(text);

	if(!EnsureNewline(buf))
		return FALSE;

	if(length < buf->spaceLeft) {
		strcpy(buf->endOfUsedRegion, text);
		buf->endOfUsedRegion += length;
		buf->spaceLeft -= length;
	}
	else
		return FALSE;

	assert(*buf->endOfUsedRegion == NUL);
	
	AddRegionDescriptor(buf, "<console input>", start, FALSE);
		
	return TRUE;
}

bool AppendFileToBuffer(struct Buffer *buf, const char *filename, bool isHidden)
{
	char *start = buf->endOfUsedRegion;
	FILE *stream;
	int fileError, fileEOF;
	
	if(!EnsureNewline(buf))
		return FALSE;

	stream = fopen(filename, "r");
	if(stream == NULL)
		return FALSE;

	/* > 1 because a NUL is appended. */
	while(buf->spaceLeft > 1 && fgets(buf->endOfUsedRegion, (int)buf->spaceLeft, stream) != NULL) {
		size_t nread = strlen(buf->endOfUsedRegion);

#if !PF_CLIB_PROPERLY_TRANSLATES_LINE_ENDING_SEQUENCE
		/* Deal with results from clibs which don't standardise newlines -
			abc\r\n (DOS/Windows style) --> abc\n
			abc\r (Old Macintosh style) --> abc\n */
		if(nread >= 2 && buf->endOfUsedRegion[nread - 2] == '\r' && buf->endOfUsedRegion[nread - 1] == '\n') {
			buf->endOfUsedRegion[nread - 2] = '\n';
			buf->endOfUsedRegion[nread - 1] = NUL;
			--nread;
		}
		else if(nread >= 1 && buf->endOfUsedRegion[nread - 1] == '\r')
			buf->endOfUsedRegion[nread - 1] = '\n';
#endif

		buf->endOfUsedRegion += nread;
		buf->spaceLeft -= nread;
	}

	fileError = ferror(stream);
	fileEOF = feof(stream);

	fclose(stream);

	if(fileError || !fileEOF)
		return FALSE;

	assert(*buf->endOfUsedRegion == NUL);
	
	AddRegionDescriptor(buf, filename, start, isHidden);
	
	return TRUE;
}

/* Returns the number of bytes free in the buffer. */
size_t GetFreeFileBufferSpace(const struct Buffer *buf)
{
	return buf->spaceLeft;
}

/* Returns the line number and file name of the given location. If the location
is not within the used portion of the buffer, the line number will be set to
-1 and the name to "". */
void GetLocationInfo(const struct Buffer *buf, const char *location, int *lineNum, const char **fileName)
{
	const char *scan;
	const struct File *c;

	/* Find the File entry for location: */

	for(c = buf->firstFile;
	    c != NULL && (c->start > location || c->end < location); 
	    c = c->next)
		;

	if(c == NULL /*|| c->hidden*/) {
		*lineNum = -1;
		*fileName = "";
	}
	else {
		*fileName = c->name;
		*lineNum = 1;

		/* Inefficient, but this is only used for error messages - */
		for(scan = c->start; scan != location; scan++)
			*lineNum += *scan == '\n';
	}
}

#ifdef DEBUG

void PrintFileBufferInfo(const struct Buffer *buf)
{
	const struct File *c;
	int numFilesLoaded = 0, numHidden = 0;
	
	fprintf(stderr, "The buffer is " SIZE_FMT " bytes long; " SIZE_FMT " bytes in use.\n",
		buf->length, buf->length - buf->spaceLeft);
	for(c = buf->firstFile; c != NULL; c = c->next) {
		++numFilesLoaded;
		numHidden += c->hidden;
	}
	fprintf(stderr, "%d file(s) loaded; %d of them marked hidden.\n",
		numFilesLoaded, numHidden);
}

void RunProgramBufferTests(void)
{
	size_t freeSpace;
	int line;
	const char *dummyFilename;
	struct Buffer *buf;

	fprintf(stderr, "-- Buffer self-tests running ...\n");
	
	buf = CreateFileBuffer(150);
	
	/* To avoid the complication of reading files, only 'one liners' are tested - */

	AppendToBuffer(buf, "The first file.\nA hidden piece of text.\n");
	buf->firstFile->hidden = TRUE;
	
	freeSpace = GetFreeFileBufferSpace(buf);
	
	AppendToBuffer(buf, "A second, non-hidden piece of text.\nSecond line of this.");
	
	assert(GetFreeFileBufferSpace(buf) < freeSpace);
	
	assert(WithinFileBuffer(buf, FileBufferBase(buf) + 5));
	
	GetLocationInfo(buf, FileBufferBase(buf) + 5, &line, &dummyFilename);
	
	assert(line == 1 /*-1*/); /* In the hidden file. */
	
	GetLocationInfo(buf, FileBufferBase(buf) + 60, &line, &dummyFilename);
	
	assert(line == 1);
	
	GetLocationInfo(buf, FileBufferBase(buf) + 80, &line, &dummyFilename);
	
	assert(line == 2);
	
	DisposeFileBuffer(buf);
	
	fprintf(stderr, "-- Buffer self-tests finished.\n");
}

#endif /* DEBUG */
