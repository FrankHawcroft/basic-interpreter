/****** io.c ******/

/*
	$VER: io.c 0.16 (6.19.2013)

	Statements and functions for stream input and output,
	and other filesystem operations such as listing directories.
*/

#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <ctype.h>
#include <stdlib.h>
#include "interpreter.h"
#include "builtin.h"
#include "process.h"
#include "platform.h"
#include "heap.h"
#include "basicio.h"

/* Random access files automatically write into the string variables 
specified in a FIELD statement whenever a record is read, and read from them 
when one is to be written. This structure records a binding between buffer
and variable. */
struct FieldedVariable {
	/* String within variable. */
	QString *dataLocation;
	
	/* So it can be checked to make sure it's in scope. */
	short definitionCallNestLevel;

	/* Number of bytes to read or write, defined by the length of
	the string when the FIELD statement is executed. This value is
	checked against the current length of the string on each read
	or write: if it doesn't match, an error occurs. */
	size_t fieldLength;
};

struct RandomAccessFileBuffer {
	struct FieldedVariable *fields;
	unsigned numFields;
	size_t bufSize; /* In bytes, not records. Fixed once the buffer has been created. */
	char *buffer; /* Pointer to buffer area. Fixed once buffer created. */	
};

struct Stream {
	/* 'I', 'O', 'A', or 'R', or 0 if file not open. */
	int mode;

	/* The C lib stream underlying this BASIC construct. */
	FILE *fp;
	
	/* The path used to open the file, stored as a NUL terminated string. */
	const char *fileName;
	
	/* Number of record currently in buffer (random access files).
	Count of bytes read from/written to stream (sequential).
	-1 if unknown, data invalid, or no record yet got/put. */
	long loc;
	
	/* Size of file: in records (random access) or bytes (sequential).
	-1 if unknown. */
	long lof; /* TODO this should be size_t (or off_t?) to support long files */
	
	/* Line terminating sequence length: 1 if '\r' or '\n', 2 if both. */
	size_t lineTerminatorLength;
	
	/* Buffer and field info, only used for random access files. */
	struct RandomAccessFileBuffer r;
	
	/* File was opened in a 'b'(inary) mode. */
	bool binaryMode;
	
	/* Currently at/have reached end of file. */
	bool atEOF;
	
	/* Handle is attached to a console etc. */
	bool interactive;
};

/* Passed to AccessItem to get the indicated field from the Stream structure. */
enum FileMetadataType {
	NO_DATA_REQUIRED, /* Just checks validity. */
	GET_EOF_STATUS,
	GET_LOCATION,
	GET_FILE_LENGTH
};

enum FieldTransferDirection {
	BUFFER_TO_FIELDS,
	FIELDS_TO_BUFFER
};

/* On the Amiga with VBCC, FOPEN_MAX is huge ... */
#if !defined(FOPEN_MAX) || FOPEN_MAX >= 23
#define MAX_OPEN_FILES 20
#else
#define MAX_OPEN_FILES (FOPEN_MAX - 3) /* -3 for stdin, stdout, stderr */
#endif

/* The key thing is to set all the 'mode' fields to 0, indicating closed. */
void InitStreams(void)
{
	assert(Proc()->streams == NULL);

	Proc()->streams = New(MAX_OPEN_FILES * sizeof(struct Stream));
	memset(Proc()->streams, NUL, MAX_OPEN_FILES * sizeof(struct Stream));
	
	Proc()->currentObjectName = NULL;
}

/* If modeRequired is -1, asserts that file is open. */
static struct Stream *GetStream(int id, int modeRequired)
{
	struct Stream *stream = NULL;

	if(id >= 0 && id < MAX_OPEN_FILES) {
		stream = &Proc()->streams[id];
		if((modeRequired == -1 && stream->mode == 0)
		|| (modeRequired != -1 && stream->mode != modeRequired)) {
			stream = NULL;
			CauseError(BADFILEMODE);
		}
	}
	else
		CauseError(BADFILENUMBER);
	
	return stream;
}

static struct Stream *GetStreamFromHandle(FILE *fp)
{
	int id;
	for(id = 0; id < MAX_OPEN_FILES && (Proc()->streams[id].mode == 0 || Proc()->streams[id].fp != fp); id++)
		;
	return id >= MAX_OPEN_FILES ? NULL : &Proc()->streams[id];
}

/* This tolerates partially initialised streams. */
static void CloseStream(struct Stream *stream)
{
	if(stream->fp != stdin
	&& stream->fp != stdout
	&& stream->fp != stderr
	&& stream->fp != NULL)
		fclose(stream->fp);
	
	stream->fp = NULL;

	if(stream->fileName != NULL) {
		Dispose((void *)stream->fileName);
		stream->fileName = NULL;
	}
	
	if(stream->mode == 'R') {
		if(stream->r.fields != NULL)
			Dispose(stream->r.fields);
		if(stream->r.buffer != NULL)
			Dispose(stream->r.buffer);
		stream->r.fields = NULL;
		stream->r.buffer = NULL;
	}
	
	stream->mode = 0;
}

void CloseAllStreams(void)
{
	if(Proc()->streams != NULL) {
		int i;
		for(i = 0; i < MAX_OPEN_FILES; i++)
			if(Proc()->streams[i].mode != 0)
				CloseStream(&Proc()->streams[i]);
	}
}

static long AccessItem(int id, enum FileMetadataType item)
{
	const struct Stream *stream = GetStream(id, -1);

	if(stream == NULL)
		return -1;
	else if(item == GET_EOF_STATUS)
		return (long)stream->atEOF;
	else if(item == GET_LOCATION)
		return stream->loc;
	else if(item == GET_FILE_LENGTH)
		if(stream->mode == 'R' && stream->r.fields == NULL) {
			CauseError(BADFILEMODE);
			return 0;
		}
		else
			return stream->lof;
	else
		assert(FALSE);
	
	return 0;
}

static bool AtEOF(int fileNumber) { return (bool)AccessItem(fileNumber, GET_EOF_STATUS); }

static long CurrentFileLocation(int fileNumber) { return AccessItem(fileNumber, GET_LOCATION); }

static long LengthOfFile(int fileNumber) { return AccessItem(fileNumber, GET_FILE_LENGTH); }

void Eof_(Scalar *result, const BObject *arg, unsigned count)
{
	SetBoolean(result, AtEOF(arg[0].value.scalar.value.number.s));
}

void Loc_(Scalar *result, const BObject *arg, unsigned count)
{
	SetFromLong(result, CurrentFileLocation(arg[0].value.scalar.value.number.s), T_LONG);	
}

void Lof_(Scalar *result, const BObject *arg, unsigned count)
{
	SetFromLong(result, LengthOfFile(arg[0].value.scalar.value.number.s), T_LONG);	
}

void Status_(Scalar *result, const BObject *arg, unsigned count)
{
	long status = -1;
	short id = arg[0].value.scalar.value.number.s, i;

	if(id == -1) {
		for(i = 0; i < MAX_OPEN_FILES && status == -1; i++)
			if(Proc()->streams[i].mode == 0)
				status = i;
		SetFromLong(result, status, T_INT);
	}
	else if(id >= 0 && id < MAX_OPEN_FILES) {
		if(Proc()->streams[id].mode == 0)
			status = 0;
		else if(!Proc()->streams[id].interactive)
			status = 1;
		else
			status = 2;
		SetFromLong(result, status, T_INT);
	}
	else
		SetError(result, BADFILENUMBER);	
}

/* Transfer data between fielded string variables and the file buffer itself.
	The buffer is assumed to be large enough to hold all the fields.  
However, the length of each string variable is checked to make sure it is the
same length as it was when FIELDed. */
static Error TransferFields(
	enum FieldTransferDirection direction, 
	char *buffer,
	const struct FieldedVariable *fields, 
	unsigned numFields)
{
	const struct FieldedVariable *f;

	for(f = fields; f < fields + numFields; f++) {
		if(f->definitionCallNestLevel > Proc()->callNestLevel
		|| f->fieldLength != QsGetLength(f->dataLocation))
			return BADFIELD;

		/* This is not tested with sizeof(QsChar) != 1 */
		if(direction == FIELDS_TO_BUFFER)
			memcpy(buffer, QsGetData(f->dataLocation), f->fieldLength);
		else { /* BUFFER_TO_FIELDS */
			QsDispose(f->dataLocation);
			QsCopyData(f->dataLocation, buffer, f->fieldLength);
		}
		buffer += f->fieldLength;
	}
	
	return SUCCESS;
}

/* Helper to work out if a file is at EOF, according to BASIC's definition of
EOF, which is more stringent than the C library's. */
static bool EOFStatus(struct Stream *stream)
{
	return stream->mode == 'R'
		/* For a random-access file, apart from certain situations when atEOF can
		be set directly, the test is simple. */
		? stream->loc == stream->lof - 1
		/* Else a sequential file. 'O' and 'A' mode files are always at EOF by definition. */
		: stream->mode == 'O'
		  || stream->mode == 'A'
		  || feof(stream->fp)
		  || (stream->lof != -1 && stream->loc >= stream->lof);
}

static void UpdatePositionPostSequentialRead(struct Stream *stream, long actuallyRead, long desired)
{
	stream->loc += actuallyRead > 0 ? actuallyRead : 0;
	stream->atEOF = (desired > 0 && actuallyRead < desired) || EOFStatus(stream);
		/* The first part of this disjunction makes an assumption which may not always hold. */
}

/* Read a chunk of data from an 'I' file into buffer. Returns length actually read, or -1 if error. */
static long ReadBlockFrom(int id, long count, char *buffer)
{
	struct Stream *stream = GetStream(id, 'I');
	long result = -1;
	if(stream != NULL) {
		result = stream->atEOF ? 0 : ReadFrom(stream->fp, count, buffer);
		UpdatePositionPostSequentialRead(stream, result, count);
	}
	return result;
}

void FRead_(Scalar *result, const BObject *arg, unsigned count)
{
	int fileNumber = arg[0].value.scalar.value.number.s;
	long desired = arg[1].value.scalar.value.number.l, actual;
	char *buffer = NULL;

	if(desired < 0) {
		SetError(result, OUTSIDEDOMAIN);
		return;
	}

	if(desired > 0)
		buffer = New(desired);
	actual = ReadBlockFrom(fileNumber, desired, buffer);
	if(actual < 0 || (desired > 0 && actual == 0)) {
		Error error = LastIOError();
		SetError(result, error == SUCCESS ? READPASTEOF : error);
	}
	else {
		InitScalarAsString(result);
		QsCopyData(&result->value.string, buffer, actual);
	}
	if(buffer != NULL)
		Dispose(buffer);
}

/* Common implementation for Input_ and FInput_ */
static void InputFrom(FILE *stream, unsigned nItems, BObject *args)
{
	struct Stream *basicStream = GetStreamFromHandle(stream);
	int position;
	Error error = SUCCESS;
	QString line;
	unsigned varIndex;

	/* Read a line: */
	error = ReadLine(stream, &line);

	/* This is a bit unpleasant - update the current location, if it's a BASIC stream. */
	if(basicStream != NULL)
		UpdatePositionPostSequentialRead(basicStream, QsGetLength(&line) + basicStream->lineTerminatorLength, -1);

	/* Parse string, converting comma-separated substrings into scalars.
	If an error occurs, stop looping immediately, so the values
	of the variables can't be treated as significant by the program. */
	position = 0;
	for(varIndex = 0; varIndex != nItems && error == SUCCESS; varIndex++) {
		Scalar *variable = VarData(&args[varIndex]);
		int start, finish; /* extent of token */
		Scalar value;

		/* Parse token.  The way in which this is done is somewhat
		ad-hoc and inconsistent with the way parsing of tokens is done
		by the interpreter itself. */
		finish = start = position;
		while(finish < QsGetLength(&line) && QsGetCharAt(&line, finish) != ',')
			++finish;
		position = finish;
		/* Save ptr to just past end of token. */
		--finish;
		/* Move off comma or inside string. */

		if(finish >= start) {
			QString token;

			/* A value was provided.  Convert token into value of
			type wanted.  Further format checking is done by
			ParseAs() */

			QsGetSubstring(&token, &line, start, finish - start + 1);
			error = ParseAs(&token, &value, NonPointer(variable->type));
			QsDispose(&token);
		}
		else
			/* No value provided, use default null/0 value. */
			InitScalar(&value, NonPointer(variable->type), FALSE);

		/* Put value into var. */
		if(error == SUCCESS) {
			CopyDereferencingBoth(variable, &value);
			DisposeScalar(&value);
		}

		/* Advance to next field. */
		if(position < QsGetLength(&line))
			++position;
	} /* for each variable */

	if(position < QsGetLength(&line))
		error = INPUTFORMAT;

	QsDispose(&line);

	if(error != SUCCESS)
		CauseError(error);
}

static void IncrementPosition(FILE *stream, Error error, size_t size)
{
	struct Stream *basicStream = GetStreamFromHandle(stream);
	/* Not always correct assumption that failure to write meant nothing was written - */
	if(basicStream != NULL && error == SUCCESS && size != 0) {
		if(basicStream->lof != -1)
			basicStream->lof += (long)size; /* TODO if too big, overflows */
		basicStream->loc += (long)size;
	}
}

static Error WriteCharToTextStream(FILE *stream, char c)
{
	/* Inefficient due to the search that happens through all streams on each call to IncrementPosition
		(see GetStreamFromHandle), but most writes are at a less granular level, where the BASIC
		overhead will be miniscule compared to the time the system write call will take. */
	Error error = WriteChar(stream, c);
	IncrementPosition(stream, error, c == '\n' ? PF_LINE_TERMINATOR_LENGTH : sizeof(char));
	return error;
}

static void CountLineTerminator(const char *buffer, size_t size, size_t pos, unsigned *cr, unsigned *lf, unsigned *crlf)
{
	char next = pos >= size - 1 ? NUL : buffer[pos + 1];
	char prev = pos == 0 ? NUL : buffer[pos - 1];
	*cr += buffer[pos] == '\r' && next != '\n';
	*lf += buffer[pos] == '\n' && prev != '\r';
	*crlf += buffer[pos] == '\r' && next == '\n';
}

static void CountAllLineTerminators(const char *buffer, size_t size, unsigned *cr, unsigned *lf, unsigned *crlf)
{
	size_t i;

	*cr = *lf = *crlf = 0;

	for(i = 0; i != size; i++)
		CountLineTerminator(buffer, size, i, cr, lf, crlf);
}

static size_t TextualLengthOfString(const QString *s)
{
	unsigned cr, lf, crlf;
	CountAllLineTerminators(QsGetData(s), QsGetLength(s), &cr, &lf, &crlf);
	return (cr + lf + crlf) * PF_LINE_TERMINATOR_LENGTH + (QsGetLength(s) - cr - lf - 2 * crlf);
}

static size_t TextualLength(const Scalar *v)
{
	QString str;
	size_t length = ScalarToString(v, &str) == SUCCESS ? TextualLengthOfString(&str) : 0;
	QsDispose(&str);
	return length;
}

static Error WriteStringToTextStream(FILE *stream, const QString *s)
{
	Error error = QsWrite(s, stream) ? SUCCESS : LastIOError();
	IncrementPosition(stream, error, TextualLengthOfString(s));
	return error;
}

static Error WriteScalarToTextStream(FILE *stream, const Scalar *v)
{
	Error error = WriteScalar(stream, v);
	IncrementPosition(stream, error, TextualLength(v));
	return error;
}

void Input_(BObject *arg, unsigned count)
{
	QString *prompt = &arg[0].value.scalar.value.string;
	Error error = SUCCESS;
	
	/* Show prompt only if non-null. This test is required to avoid frustration
	 if the program wants to work even if the standard output stream isn't
	 attached to the same terminal etc. as the input stream. */
	if(QsIsNull(prompt) || (error = WriteStringToTextStream(stdout, prompt)) == SUCCESS)
		error = FlushInteractive(stdout);
		
	if(error == SUCCESS)
		InputFrom(stdin, count - 1, arg + 1);
	else
		CauseError(error);
}

static void PrintTo(FILE *stream, unsigned nItems, BObject *args)
{
	Error error = SUCCESS;
	unsigned curItem;

	for(curItem = 0; curItem < nItems && error == SUCCESS; curItem++)
		error = WriteScalarToTextStream(stream, &args[curItem].value.scalar);

	/* Output to a console is done straight away. */
	if(error == SUCCESS)
		error = FlushInteractive(stream);

	if(error != SUCCESS)
		CauseError(error);
}

void Print_(BObject *arg, unsigned count)	
{
	PrintTo(stdout, count, arg);
}

/* Common implementation for LineInput_ and FLineInput_ */
static void LineInputFrom(FILE *stream, unsigned nItems, BObject *args)
{
	struct Stream *basicStream = GetStreamFromHandle(stream);
	Scalar *var = VarData(&args[0]);
	Error error = SUCCESS;
	Scalar holder;

	InitScalarAsString(&holder);

	error = ReadLine(stream, &holder.value.string);

	/* This is a bit unpleasant - update the current location, if it's a BASIC stream. */
	if(basicStream != NULL)
		UpdatePositionPostSequentialRead(basicStream, QsGetLength(&holder.value.string) + basicStream->lineTerminatorLength, -1);

	if(error == SUCCESS)
		CopyDereferencingBoth(var, &holder);
	else
		CauseError(error);

	DisposeScalar(&holder);
}

void LineInput_(BObject *arg, unsigned count)
{
	QString *prompt = &arg[0].value.scalar.value.string;
	Error error = SUCCESS;

	if(QsIsNull(prompt) || (error = WriteStringToTextStream(stdout, prompt)) == SUCCESS)
		error = FlushInteractive(stdout);

	if(error == SUCCESS)
		LineInputFrom(stdin, count - 1, arg + 1);
	else
		CauseError(error);
}

#ifdef AMIGA
#define SAMPLE_BUFFER_SIZE 488 /* common disk block size, less header */
#else
#define SAMPLE_BUFFER_SIZE 1024 /* could be 4096, but the larger the buffer, the longer it takes to scan */
#endif

/* Helper to make a possibly more informed guess about the length of the line-
terminating sequence used in a sample of text; defaults to the system's standard. */
static size_t GetLineTerminatorLength(const char *buffer, size_t count)
{
	unsigned cr = 0, lf = 0, crlf = 0;
	size_t length = PF_LINE_TERMINATOR_LENGTH;

	CountAllLineTerminators(buffer, count, &cr, &lf, &crlf);

	if(cr != 0 || lf != 0 || crlf != 0)
		length = crlf > cr && crlf > lf ? 2 : 1;

	return length;
}

#if PF_BINARY_TEXT_DISTINCTION
static bool TreatAsBinaryFile(const char *buffer, size_t count)
{
	size_t seemNonTextual = 0, i;
	
	/* There's a pro-UTF-8 or -ASCII-7 bias here. */
	for(i = 0; i != count; i++)
		seemNonTextual += buffer[i] < 32 || (unsigned char)buffer[i] > 244;
	
	return seemNonTextual > count / 2;
}
#endif

void Open_(BObject *arg, unsigned count)
{
	char mode = toupper(arg[0].value.scalar.value.character);
	int id = arg[1].value.scalar.value.number.s;
	const QString *name = &arg[2].value.scalar.value.string;
	long bufSize = arg[3].value.scalar.value.number.l;
	struct Stream *stream;
	char cMode[4];

	/* Check validity of parameters: */

	if(id < 0 || MAX_OPEN_FILES <= id) {
		CauseError(BADFILENUMBER);
		return;
	}
	
	if(bufSize < 0) {
		/* Only (theoretically) relevant to 'R' files currently, but check in all cases
			for future compatibility. */
		CauseError(BADBUFFERSIZE);
		return;
	}

	stream = &Proc()->streams[id];

	if(stream->mode != 0) {
		CauseError(FILENUMBERINUSE);
		return;
	}

	/* Store file name as a C-style string: */

	stream->fileName = QsDupAsNTS(name);

	/* Determine file length: */

	stream->lof = mode == 'O' ? 0 : PfFileLength(stream->fileName);
		/* Note that PfFileLength() may also return -1, if it can't
		find out the file size (e.g. file opened to a device). 
		Note that this value is NOT the correct lof value for a 'R'
		file; it needs to be divided by the record size, which is done
		later by Field_(). */

	/* Assume default line terminator sequence length for the platform: */
	
	stream->lineTerminatorLength = PF_LINE_TERMINATOR_LENGTH;
	
	/* Assume text mode - this may be changed for 'R' and I' mode files: */
	
	stream->binaryMode = FALSE;
	
	/* Determine C lib file mode corresponding to BASIC one: */
	
	cMode[0] = NUL;
	if(mode == 'I') {
		stream->binaryMode = PF_BINARY_TEXT_DISTINCTION && stream->lof > 0;
		strcpy(cMode, "r");
	}
	else if(mode == 'O')
		strcpy(cMode, "w");
	else if(mode == 'A')
		strcpy(cMode, "a");
	else if(mode == 'R') {
		/* 'R' mode files should ALWAYS be opened in binary mode if the system makes
		this distinction, because CR+LF translation must not be done inside fixed-length
		records. (Although this shouldn't happen anyway if the stream isn't accessed
		using line-oriented functions.) */	
		stream->binaryMode = PF_BINARY_TEXT_DISTINCTION;
		strcpy(cMode, PfFileExists(stream->fileName) ? "r+" : "w+");
	}

	if(stream->binaryMode)
		strcat(cMode, "b");
	
	/* Open file using C lib: */

	if(QsIsNull(name) && mode == 'I')
		stream->fp = stdin;
	else if(QsIsNull(name) && mode == 'O')
		stream->fp = stdout;
	else if(cMode[0] != NUL)
		stream->fp = fopen(stream->fileName, cMode);
	else {
		Dispose((void *)stream->fileName);
		CauseError(BADFILEMODE);
		return;
	}

	/* For an existing file opened for reading, determine the line-terminating
		sequence used in the file, and whether it should be opened in binary mode. 
		For 'O' and 'A' modes, assume text output - program will have to manage line terminators
		itself if it wants the sequence to differ from the platform's standard. */

	if(stream->fp != NULL && mode == 'I' && stream->lof > 0) {
		char buffer[SAMPLE_BUFFER_SIZE];
		size_t readCount = fread(buffer, sizeof(char), SAMPLE_BUFFER_SIZE, stream->fp);

		stream->lineTerminatorLength = GetLineTerminatorLength(buffer, readCount);
		
		if(fseek(stream->fp, 0, SEEK_SET) != 0) {
			Dispose((void *)stream->fileName);
			CauseError(LastIOError());
			return;
		}
		
#if PF_BINARY_TEXT_DISTINCTION
		stream->binaryMode = TreatAsBinaryFile(buffer, readCount > 256 ? 256 : readCount);
		if(!stream->binaryMode && freopen(stream->fileName, "rt", stream->fp) == NULL) {
			fclose(stream->fp);
			stream->fp = fopen(stream->fileName, "rt");
		}
#endif
	}
	
	/* If couldn't open file, clean up and record error: */

	if(stream->fp == NULL) {
		SetAdditionalErrorMessage("Could not open: %.*s", stream->fileName, strlen(stream->fileName));
		Dispose((void *)stream->fileName);
		CauseError(LastIOError());
		return;
	}
	
	/* Initialise remaining fields of stream structure.  
	Note that the buffer used by a random access file is not allocated
	until a FIELD statement is executed. This allows random access
	fields to be defined after the file has been opened. */

	stream->mode = mode;
	stream->loc = mode == 'R' ? -1 : 0;
	stream->atEOF = mode == 'A' || mode == 'O' || stream->lof == 0;
	stream->interactive = PfIsInteractive(stream->fp);
	stream->r.fields = NULL;
	stream->r.numFields = 0;
	stream->r.bufSize = 0; /* Buffer size argument is ignored! May not always be the case. */
	stream->r.buffer = NULL;
}

void Close_(BObject *arg, unsigned count)
{
	if(count == 1 && arg[0].value.scalar.value.number.s == SHRT_MIN)
		CloseAllStreams();
	else {
		unsigned i;
		for(i = 0; i != count; i++) {
			struct Stream *stream = GetStream(arg[i].value.scalar.value.number.s, -1);
			if(stream == NULL) {
				CauseError(BADFILEMODE);
				return;
			}
			CloseStream(stream);
		}
	}
}

void FInput_(BObject *arg, unsigned count)
{
	struct Stream *stream = GetStream(arg[0].value.scalar.value.number.s, 'I');

	if(stream == NULL)
		return;
	else if(stream->atEOF)
		CauseError(READPASTEOF);
	else
		InputFrom(stream->fp, count - 1, arg + 1);
}

void FLineInput_(BObject *arg, unsigned count)
{
	struct Stream *stream = GetStream(arg[0].value.scalar.value.number.s, 'I');

	if(stream == NULL)
		return;
	else if(stream->atEOF)
		CauseError(READPASTEOF);
	else
		LineInputFrom(stream->fp, count - 1, arg + 1);
}

void FPrint_(BObject *arg, unsigned count)
{
	struct Stream *stream = GetStream(arg[0].value.scalar.value.number.s, -1);
	
	if(stream == NULL || (stream->mode != 'O' && stream->mode != 'A'))
		CauseError(BADFILEMODE);
	else
		PrintTo(stream->fp, count - 1, arg + 1);
}

void Write_(BObject *arg, unsigned count)
{
	unsigned item;
	struct Stream *stream = GetStream(arg[0].value.scalar.value.number.s, -1);
	Error error = stream->mode == 'O' || stream->mode == 'A' ? SUCCESS : BADFILEMODE;

	for(item = 1; item != count && error == SUCCESS; item++) {
		/* if(value->type == T_STRING)
			error = WriteChar('\"'); */
		error = WriteScalarToTextStream(stream->fp, &arg[item].value.scalar);
		/* if(error == SUCCESS && value->type == T_STRING)
			error = WriteChar('\"'); */
		if(error == SUCCESS && item < count - 1)
			error = WriteCharToTextStream(stream->fp, ',');
	}
	
	if(error == SUCCESS)
		error = WriteCharToTextStream(stream->fp, '\n');

	if(error != SUCCESS)
		CauseError(error);
}

void Field_(BObject *arg, unsigned count)
{
	struct Stream *stream = GetStream(arg[0].value.scalar.value.number.s, 'R');
	struct FieldedVariable *fieldDefn;
	unsigned field;

	if(stream == NULL || stream->r.fields != NULL) {
		CauseError(BADFILEMODE); /* FIELDed twice. */
		return;
	}
	
	/* Define the fields: */

	stream->r.numFields = count - 1;
	stream->r.fields = (struct FieldedVariable *)New(sizeof(struct FieldedVariable) * stream->r.numFields);

	fieldDefn = stream->r.fields;
	for(field = 1; field != count; field++) {
		Scalar *var = VarData(&arg[field]);
		QString *str = (QString *)GetPointer(var);
		
		if(NonPointer(var->type) != T_STRING || QsIsNull(str)) {
			CauseError(BADFIELD);
			return;
		}

		fieldDefn->dataLocation = str;
		fieldDefn->definitionCallNestLevel = (arg[field].category & VARIABLE_IS_SHARED) ? SCOPE_MAIN :  Proc()->callNestLevel;
		fieldDefn->fieldLength = QsGetLength(str);

		stream->r.bufSize += fieldDefn->fieldLength;
		++fieldDefn;
	}

	/* Adjust lof to give size of file in records rather than bytes, and allocate the buffer: */

	if(stream->lof == -1)
		/* Assume couldn't find it out because the file didn't exist yet. */
		stream->lof = 0;
		
	if(stream->lof % stream->r.bufSize != 0) {
		Dispose(stream->r.fields);
		stream->r.fields = NULL;
		stream->r.numFields = 0;
		stream->r.bufSize = 0;
		CauseError(BADFILESIZE);
	}
	else {
		stream->lof /= stream->r.bufSize;
		stream->r.buffer = New(stream->r.bufSize);
	}
}

void Get_(BObject *arg, unsigned count)
{
	struct Stream *stream = GetStream(arg[0].value.scalar.value.number.s, 'R');
	long record = arg[1].value.scalar.value.number.l, result;
	Error error;

	if(stream->r.buffer == NULL) {
		CauseError(BADFILEMODE);
		return;
	}

	if(record == SHRT_MIN)
		record = stream->loc + 1;
		/* When a 'R' file is first opened, loc is always -1, as there is no record yet in the buffer. */
	
	if(record < 0) {
		CauseError(RECNUMTOOBIG);
		return;
	}

	/* Lots of return statements here, but I think this is the clearest way of expressing the logic. */
	
	/* Check that record is in file. */
	if(record >= stream->lof) {
		CauseError(READPASTEOF);
		return;
	}
	
	/* If record already in buffer, don't need to read it from disk. */
	if(record == stream->loc)
		return;
		
	if(fseek(stream->fp, record * stream->r.bufSize, SEEK_SET) == -1) {
		CauseError(LastIOError());
		return;
	}
	
	stream->loc = record;
	result = fread(stream->r.buffer, sizeof(char), (size_t)stream->r.bufSize, stream->fp);
	if(result < stream->r.bufSize) {
		stream->loc = -1L;
		stream->atEOF |= result == 0;
		CauseError(result == 0 ? READPASTEOF : LastIOError());
		return;
	}

	/* Read was successful. Update EOF status. */
	stream->atEOF = EOFStatus(stream);
	
	/* Transfer data from the buffer into the fielded string variables. */
	if((error = TransferFields(BUFFER_TO_FIELDS, stream->r.buffer, stream->r.fields, stream->r.numFields)) != SUCCESS)
		CauseError(error);
}

void Put_(BObject *arg, unsigned count)
{
	struct Stream *stream = GetStream(arg[0].value.scalar.value.number.s, 'R');
	long record = arg[1].value.scalar.value.number.l, result;
	Error error;
	
	if(stream->r.buffer == NULL) {
		CauseError(BADFILEMODE);
		return;
	}

	if(record == SHRT_MIN)
		record = stream->loc + 1;

	/* Check that record is in file, or if not, is one off the end of the file. */

	if(record < 0 || record > stream->lof) {
		CauseError(RECNUMTOOBIG);
		return;
	}

	/* Transfer data from the fielded string variables into the buffer. */

	if((error = TransferFields(FIELDS_TO_BUFFER, stream->r.buffer, stream->r.fields, stream->r.numFields)) != SUCCESS) {
		CauseError(error);
		return;
	}

	/* Write the record to disk. */

	if(fseek(stream->fp, record * stream->r.bufSize, SEEK_SET) == -1) {
		CauseError(LastIOError());
		return;
	}

	stream->loc = record;	
	result = fwrite(stream->r.buffer, sizeof(char), (size_t)stream->r.bufSize, stream->fp);
	
	if(result < stream->r.bufSize) {
		/* If, as is common, the system line-buffers interactive streams, 'R' file
		access will not be reliable for such streams. */

		stream->loc = -1;

		/* There doesn't seem to be any point to changing atEOF.
		GET and PUT don't care about atEOF when checking their preconds,
		and have effective heuristics for setting atEOF if the read or 
		write succeeds. */

		CauseError(LastIOError());
		return;
	}

	/* The write was successful. */

	stream->lof += record == stream->lof;
	stream->atEOF = stream->loc == stream->lof - 1;
}

#define MkXImpl(name, member, t) \
	void name(Scalar *result, const BObject *arg, unsigned count) \
	{ \
		InitScalarAsString(result); \
		QsCopyData(&result->value.string, (const QsChar *)&arg[0].value.scalar.value.member, sizeof(t) / sizeof(QsChar)); \
	}

MkXImpl(Mkb_, boolean, bool)
MkXImpl(Mki_, number.s, short)
MkXImpl(Mkl_, number.l, long)
MkXImpl(Mks_, number.f, float)
MkXImpl(Mkd_, number.d, double)

#define CvXImpl(name, setter, t, typeCode) \
	void name(Scalar *result, const BObject *arg, unsigned count) \
	{ \
		const QString *s = &arg[0].value.scalar.value.string; \
		if(QsGetLength(s) != (int)sizeof(t)) \
			SetError(result, BADSUBSTRING); \
		else { \
			t value; \
			memcpy((char *)&value, QsGetData(s), sizeof(t)); \
			setter(result, value, typeCode); \
		} \
	}

CvXImpl(Cvb_, SetFromLong, bool, T_BOOL)
CvXImpl(Cvi_, SetFromLong, short, T_INT)
CvXImpl(Cvl_, SetFromLong, long, T_LONG)
CvXImpl(Cvs_, SetFromDouble, float, T_SINGLE)
CvXImpl(Cvd_, SetFromDouble, double, T_DOUBLE)

static void OperateOnObject(BObject *arg, int (__cdecl *func)(const char *))
{
	const QString *object = &arg[0].value.scalar.value.string;
	char *cObject = QsDupAsNTS(object);
	if(func(cObject))
		CauseError(LastIOError());
	Dispose(cObject);
}

void ChDir_(BObject *arg, unsigned count) { OperateOnObject(arg, PfChangeWorkingDirectory); }

void Kill_(BObject *arg, unsigned count) { OperateOnObject(arg, remove); }

static bool PrintDirEntry(const char *name, bool subDir, void *data)
{
	FILE *stream = data;
	QString qs;

	assert(Proc()->currentObjectName != NULL);

	/* . and .. are skipped, because they will cause infinite loops in existing
	BASIC programs which traverse directory trees assuming they aren't returned. */
	if(PfIsSpecialDirSpec(name))
		return TRUE;

	/* Otherwise, write the current entry ... */
	QsInitStaticNTS(&qs, name);
	WriteStringToTextStream(stream, &qs);

	/* ... followed by a path separator if it's a directory. */
	if(subDir) {
		QsInitStaticNTS(&qs, PF_PATH_SEP);
		WriteStringToTextStream(stream, &qs);
	}

	/* Assume that if an error occurred with one of the earlier writes, it'll happen again ... */
	return WriteCharToTextStream(stream, '\n') == SUCCESS;
}

void Files_(BObject *arg, unsigned count)
{
	const QString *directory = &arg[0].value.scalar.value.string;
	short maxListed = arg[1].value.scalar.value.number.s;
	short id = arg[2].value.scalar.value.number.s;
	int rawResult;
	Error error = SUCCESS;
	FILE *stream;
	
	if(!PF_EMPTY_PATH_ALLOWED && QsIsNull(directory)) {
		CauseError(FILENONEXISTENT);
		return;
	}

	if(id < -1 || MAX_OPEN_FILES <= id) {
		CauseError(BADFILENUMBER);
		return;
	}
	
	if(id >= 0) {
		if(Proc()->streams[id].mode != 'O' && Proc()->streams[id].mode != 'A') {
			CauseError(BADFILEMODE);
			return;
		}
		stream = Proc()->streams[id].fp;
	}
	else
		stream = stdout;

	Proc()->currentObjectName = QsDupAsNTS(directory);

	rawResult = PfVisitFilesAt(Proc()->currentObjectName, PrintDirEntry, stream, maxListed);
	
	Dispose(Proc()->currentObjectName);
	Proc()->currentObjectName = NULL;
	
	if(rawResult == INT_MIN) /* general error accessing the directory etc. */
		error = FILEBADOP;
	else if(rawResult == 0)
		error = ER_F_MORE_FILES;
	else if(rawResult != 1)
		error = SysIOErrorToError(-rawResult);
	
	if(error == ER_F_MORE_FILES) {
		QString more;
		QsInitStaticNTS(&more, "<<more>>\n");
		error = WriteStringToTextStream(stream, &more);
	}

	if(error != SUCCESS)
		CauseError(error);
}

void Name_(BObject *arg, unsigned count)
{
	const QString *oldName = &arg[0].value.scalar.value.string;
	const QString *newName = &arg[1].value.scalar.value.string;
	char *oldNameAsCString = QsDupAsNTS(oldName), *newNameAsCString = QsDupAsNTS(newName);

	if(rename(oldNameAsCString, newNameAsCString))
		CauseError(LastIOError());
		
	Dispose(oldNameAsCString);
	Dispose(newNameAsCString);
}
