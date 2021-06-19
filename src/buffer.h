/****** buffer.h ******/

/* 
	$VER: buffer.h 0.16 (1.21.2013)

	A simple buffer implementation; 'buffer' in the sense of a memory region
	used to store text loaded from files.
	
	Loading a program is simple - the entire text of the program is read into
	this buffer, and kept there intact throughout execution. The interpreter
	works by building additional data structures that annotate the code text,
	without ever discarding or transforming it.
*/

#ifndef BAS_BUFFER_H_INCLUDED
#define BAS_BUFFER_H_INCLUDED

struct Buffer;

extern struct Buffer *CreateFileBuffer(size_t size);
extern void ResetFileBuffer(struct Buffer *);
extern void DisposeFileBuffer(struct Buffer *);
extern const char *PrimaryBufferFileName(const struct Buffer *);
extern const char *PrimaryBufferBase(const struct Buffer *);
/* Is address within the currently used region? */
extern bool WithinFileBuffer(const struct Buffer *, const char *);
extern const char *FileBufferBase(const struct Buffer *);
extern const char *EndOfUsedRegion(const struct Buffer *);
extern bool AppendToBuffer(struct Buffer *, const char *text);
extern bool AppendFileToBuffer(struct Buffer *, const char *filename, bool isHidden);
extern size_t GetFreeFileBufferSpace(const struct Buffer *);
/* Limit of the entire buffer space - including unused space. */
extern const char *FileBufferExtent(const struct Buffer *);
extern void GetLocationInfo(const struct Buffer *, const char *pos, int *line, const char **filename);
DIAGNOSTIC_FN_DECL(void PrintFileBufferInfo(const struct Buffer *));
DIAGNOSTIC_FN_DECL(void RunProgramBufferTests(void));

#endif /* BAS_BUFFER_H_INCLUDED */
