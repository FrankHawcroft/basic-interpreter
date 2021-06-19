/****** basicio.h ******/

/*
	$VER: basicio.h 0.16

	Simple I/O support - functions wrapping FILE * operations, returning Error.
*/

#ifndef BAS_BASICIO_H_INCLUDED
#define BAS_BASICIO_H_INCLUDED

#include <stdio.h>
#include "errors.h"
#include "qstring.h"

extern Error SysIOErrorToError(int sysIoErr);
extern Error LastIOError(void);
extern long ReadFrom(FILE *stream, long numBytesDesired, char *buffer);
extern Error ReadLine(FILE *, QString *);
extern Error WriteChar(FILE *, char);
extern Error FlushInteractive(FILE *);

#endif /* BAS_BASICIO_H_INCLUDED */
