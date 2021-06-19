/****** basicio.c ******/

/*
	$VER: basicio.c 0.16 (12.10.2012)

	I/O support functions.
*/

#include <limits.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include "common.h"
#include "errors.h"
#include "platform.h"
#include "qstring.h"
#include "basicio.h"

/* Convert a system error number (errno) value to a BASIC error code. 
 
Arguably, this should reside in platform.c, but there isn't any explicitly
platform-specific decision making here at the moment - only implicit, on the
existence of different system error codes. */
Error SysIOErrorToError(int sysErrNo)
{
	switch(sysErrNo) {
		case 0: /* Added for GCC/MinGW */
			return SUCCESS;
		case EBUSY:
			return FILELOCKED;
#ifdef ENOFILE
		case ENOFILE:
#endif
#if defined(ENOENT) && (!defined(ENOFILE) || ENOENT != ENOFILE) /* Added for GCC/MinGW */
		case ENOENT:
#endif
		case EXDEV:
			return FILENONEXISTENT;
		case EPERM:
		case EEXIST:
		case EROFS:
		case EACCES:
			return FILEPROTECTED;
		case ENOMEM:
		case EFBIG:
		case ENOSPC:
		case ENODEV:
			return FILEDEVICE;
		default:
			return FILEBADOP;
	}
}

/* Call this after a file operation returns a failure (usually a negative or zero value). */
Error LastIOError(void)
{
	return SysIOErrorToError(errno);
}

/* Returns 0 if EOF encountered, -1 on error, otherwise number of bytes 
   actually read, like dos.library/Read() on the Amiga. */
long ReadFrom(FILE *stream, long numBytesDesired, char *buffer)
{
	long count;

	for(count = 0; count < numBytesDesired; count++) {
		int c = fgetc(stream);
		if(c == EOF)
			/* Either error or EOF. */
			return ferror(stream) ? -1 : count;
		*buffer++ = (char)c;
	}
	return count; /* which is numBytesDesired ... */
}

/* Read a line from the input stream and put it into the string. 

Pre: the stream is open. The string is unused and uninitialised.
Post: the string will contain a line of text, if available.
Returns: any error resulting from file input. */
Error ReadLine(FILE *stream, QString *line)
{
	Error error = SUCCESS;
	int ch, alreadyAtEOF = feof(stream);

	QsInitNull(line);
	while((ch = fgetc(stream)) != EOF && ch != '\n')
#if !PF_CLIB_PROPERLY_TRANSLATES_LINE_ENDING_SEQUENCE
		/* TODO compensate for buggy old C libraries -
			but this is not a reliable fix; this loop will
			fail with old Mac-format files if the C lib
			doesn't deal with converting '\r' to '\n' */
		if(ch != '\r')
#endif
			QsAppendChar(line, (char)ch);

	if(ch == EOF) {
		if(ferror(stream))
			error = LastIOError();
		else if(alreadyAtEOF)
			error = READPASTEOF;
	}

	return error;
}

/* Attempts to write a single character to the stream.
If successful, returns SUCCESS, otherwise an error indication. */
Error WriteChar(FILE *stream, char c)
{
	assert(stream != NULL);
	return fputc(c, stream) == EOF ? LastIOError() : SUCCESS;
}

Error FlushInteractive(FILE *stream) 
{
	return PfIsInteractive(stream) && fflush(stream) ? LastIOError() : SUCCESS;
}
