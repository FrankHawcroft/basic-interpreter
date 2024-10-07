/****** scalar.c ******/

/*
	$VER: scalar.c 0.16A (6.18.2015)

	Operations on 'Scalar', and its constituent types - the 'Scalar' structure is
	essentially the primitive type for the higher semantic layers of the interpreter
	because it can store any value used in calculations.
*/

#include <limits.h>
#include <float.h>
#include <math.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include "interpreter.h"
#include "platform.h"
#include "basicio.h"

#ifdef SHRT_BIT
#define SHORT_BITS SHRT_BIT
#else
#define SHORT_BITS (sizeof(short) * CHAR_BIT)
#endif

/* Declared as consts rather than macros to avoid MSVC code analysis warnings - */
static const double ShortMinAsDouble = (double)SHRT_MIN;
static const double ShortMaxAsDouble = (double)SHRT_MAX;
static const double LongMinAsDouble = (double)LONG_MIN;
static const double LongMaxAsDouble = (double)LONG_MAX;

/* Size of sufficiently capacious fixed-length buffer to convert any numeric value to a string - */
#define NUM_STR_BUF_LEN 64

static union EValue m_ZeroEValue;

/* Useful Scalar constants: */
static Scalar m_Empty;
static Scalar m_ZeroInt;
static Scalar m_OneInt;
static Scalar m_NegOneInt;
static Scalar m_MinimumInt;
static Scalar m_MaximumInt;
static Scalar m_ZeroLongInt;
static Scalar m_OneLongInt;
static Scalar m_NegOneLongInt;
static Scalar m_NullString;
static Scalar m_EmptyString;
static Scalar m_CurDirString;
static Scalar m_NulChar;
static Scalar m_TrueBoolean;
static Scalar m_FalseBoolean;
static bool m_ConstantsInitialised = FALSE;

/* Not really keywords; also defined in prelude.bas - */
static const QString m_TrueKeyword = {"TRUE", 4};
static const QString m_FalseKeyword = {"FALSE", 5};

const Scalar *const g_Empty = &m_Empty;
const Scalar *const g_ZeroInt = &m_ZeroInt;
const Scalar *const g_OneInt = &m_OneInt;
const Scalar *const g_NegOneInt = &m_NegOneInt;
const Scalar *const g_MinimumInt = &m_MinimumInt;
const Scalar *const g_MaximumInt = &m_MaximumInt;
const Scalar *const g_ZeroLongInt = &m_ZeroLongInt;
const Scalar *const g_OneLongInt = &m_OneLongInt;
const Scalar *const g_NegOneLongInt = &m_NegOneLongInt;
const Scalar *const g_NullString = &m_NullString;
const Scalar *const g_EmptyString = &m_EmptyString;
const Scalar *const g_CurDirString = &m_CurDirString;
const Scalar *const g_NulChar = &m_NulChar;
const Scalar *const g_TrueBoolean = &m_TrueBoolean;
const Scalar *const g_FalseBoolean = &m_FalseBoolean;

/* Debugging support: */
#ifdef DEBUG

/* On the Amiga, the complicated assertions are too much of a drag on performance. */
#ifdef AMIGA
#define THOROUGH_SCALAR_SANITY_CHECKING 0
#else
#define THOROUGH_SCALAR_SANITY_CHECKING 1
#endif

#if THOROUGH_SCALAR_SANITY_CHECKING
static bool ScalarIsSane(const Scalar *v)
{
	SimpleType type = v == NULL ? T_MISSING : NonPointer(v->type);
	return v != NULL
		&& type >= T_EMPTY && type <= T_BOOL /* Possibly too pedantic. */
		&& !(IsPointer(v) && TypeIsInternal(type));
			/* It makes no sense to have a pointer to an error, empty, or missing value. */
}
#else
#define ScalarIsSane(s) ((s) != NULL)
#endif

#endif /* DEBUG */

/* Set an error in the destination (to) - from the source (from) if it's already one;
if no error in either place, don't set it.
	Unlike SetError, assumes the destination has been initialised. */
INLINE Error PropagateError(Scalar *to, const Scalar *from, Error newError)
{
	if(newError == SUCCESS && !ScalarIsError(from))
		return SUCCESS;
	else {
		/* Save the error first in case to == from: */
		Error propagatedError = ScalarIsError(from) ? from->value.error : newError;
		DisposeScalar(to);
		return SetError(to, propagatedError);
	}
}

/* Return an error if present. */
static Error RetrieveError(const Scalar *v)
{
	assert(ScalarIsSane(v));
	return ScalarIsError(v) ? v->value.error : SUCCESS;
}

/* Initialise the global constant values. This function need only be called once. */
void InitConstants(void)
{
	if(!m_ConstantsInitialised) {
		memset(&m_ZeroEValue, 0, sizeof(m_ZeroEValue));

		InitScalar(&m_Empty, T_EMPTY, FALSE);
		InitScalar(&m_ZeroInt, T_INT, FALSE);
		SetFromLong(&m_OneInt, 1L, T_INT);
		SetFromLong(&m_NegOneInt, -1L, T_INT);
		SetFromLong(&m_MinimumInt, SHRT_MIN, T_INT);
		SetFromLong(&m_MaximumInt, SHRT_MAX, T_INT);
		InitScalar(&m_ZeroLongInt, T_LONG, FALSE);	
		SetFromLong(&m_OneLongInt, 1L, T_LONG);
		SetFromLong(&m_NegOneLongInt, -1L, T_LONG);
		InitScalar(&m_NullString, T_STRING, FALSE);
		InitScalar(&m_EmptyString, T_STRING, FALSE);
		QsInitStatic(&m_EmptyString.value.string, "", 1);
		InitScalar(&m_CurDirString, T_STRING, FALSE);
		QsInitStatic(&m_CurDirString.value.string, PF_CUR_DIR, strlen(PF_CUR_DIR));
		InitScalar(&m_NulChar, T_CHAR, FALSE);
		SetBoolean(&m_TrueBoolean, TRUE);
		SetBoolean(&m_FalseBoolean, FALSE);

		m_ConstantsInitialised = TRUE;
	}
}

/* Initialise the scalar to null/0, with the given type. */
void InitScalar(Scalar *v, SimpleType type, bool isPointer)
{
	assert(v != NULL);

	v->type = type | (isPointer ? T_POINTER : 0);
#if PF_INTUITIVE_CONCEPT_OF_ZERO_VALUES_HOLDS
	v->value = m_ZeroEValue;
#else
	v->value.number.l = 0L;
	if(isPointer)
		v->value.pointer.lp = NULL;
	else if(type == T_SINGLE)
		v->value.number.f = 0.0;
	else if(type == T_DOUBLE)
		v->value.number.d = 0.0;
	else if(type == T_STRING)
		QsInitNull(&v->value.string);

	assert(ScalarIsSane(v));
#endif
}

/* Dereferences the source before assigning to the destination. (I.e. looks 
through the source if it's a pointer.) Owned memory (e.g. in strings) is 
_not_ copied however. Therefore, the caller must take care to ensure
the destination is not freed inappropriately. */
void SetToValue(Scalar *dest, const Scalar *src)
{
	assert(ScalarIsSane(src));

	if(!IsPointer(src))
		*dest = *src;
	else {
		dest->type = NonPointer(src->type);
		switch(dest->type) {
			case T_INT: dest->value.number.s = *src->value.pointer.sp; break;
			case T_LONG: dest->value.number.l = *src->value.pointer.lp; break;
			case T_SINGLE: dest->value.number.f = *src->value.pointer.fp; break;
			case T_DOUBLE: dest->value.number.d = *src->value.pointer.dp; break;
			case T_CHAR: SetCharacter(dest, *src->value.pointer.cp); break;
			case T_BOOL: SetBoolean(dest, *src->value.pointer.bp); break;
			case T_STRING: dest->value.string = *src->value.pointer.tp; break;
		}
		assert(ScalarIsSane(dest));
	}
}

/* Dereferences either argument if it is a pointer. If the source and destination
don't have the same type, creates a 'type mismatch' error in the destination.
	Owned memory (e.g. in strings) is _not_ copied. */
void SetDereferencingBoth(Scalar *dest, const Scalar *src)
{
	assert(ScalarIsSane(src) && ScalarIsSane(dest));

	if(NonPointer(dest->type) == NonPointer(src->type)) {
		if(IsPointer(dest)) {
			switch(src->type) {
				case T_INT: *dest->value.pointer.sp = src->value.number.s; break;
				case T_INT | T_POINTER: *dest->value.pointer.sp = *src->value.pointer.sp; break;
				case T_LONG: *dest->value.pointer.lp = src->value.number.l; break;
				case T_LONG | T_POINTER: *dest->value.pointer.lp = *src->value.pointer.lp; break;
				case T_SINGLE: *dest->value.pointer.fp = src->value.number.f; break;
				case T_SINGLE | T_POINTER: *dest->value.pointer.fp = *src->value.pointer.fp; break;
				case T_DOUBLE: *dest->value.pointer.dp = src->value.number.d; break;
				case T_DOUBLE | T_POINTER: *dest->value.pointer.dp = *src->value.pointer.dp; break;
				case T_STRING: case T_STRING | T_POINTER:
					*dest->value.pointer.tp = *(QString *)GetPointer((Scalar *)src); /* Doesn't QsCopy! */
					break;
				case T_CHAR: case T_CHAR | T_POINTER:
					*dest->value.pointer.cp = GetCharacter(src);
					break;
				case T_BOOL: case T_BOOL | T_POINTER:
					*dest->value.pointer.bp = GetBoolean(src);
					break;
			}
		}
		else
			SetToValue(dest, src);
	}
	else
		PropagateError(dest, src, BADARGTYPE);		
		
	assert(ScalarIsSane(dest));
}

/* Sets a pointer to the source in the destination; if the source is a pointer already, 
this is just a straight assignment, because BASIC does not allow pointers to pointers. */
void SetAsPointer(Scalar *dest, const Scalar *src)
{
	assert(ScalarIsSane(src) && dest != NULL);
	
	if(IsPointer(src))
		*dest = *src;
	else
		SetPointerTo(dest, GetPointer((Scalar *)src), src->type);
		
	assert(ScalarIsSane(dest));
}

/* Sets the appropriate 'pointer' field to the pointer provided, which 
is assumed to satisfy any alignment requirement. */
void SetPointerTo(Scalar *v, void *ptr, SimpleType type)
{
	v->type = type | T_POINTER;
	if(type == T_INT)
		v->value.pointer.sp = (short *)ptr;
	else if(type == T_LONG)
		v->value.pointer.lp = (long *)ptr;
	else if(type == T_STRING)
		v->value.pointer.tp = (QString *)ptr;
	else if(type == T_SINGLE)
		v->value.pointer.fp = (float *)ptr;
	else if(type == T_DOUBLE)
		v->value.pointer.dp = (double *)ptr;
	else if(type == T_CHAR)
		v->value.pointer.cp = (char *)ptr;
	else if(type == T_BOOL)
		v->value.pointer.bp = (bool *)ptr;

	assert(ScalarIsSane(v));
}

/* Creates a pointer offset from another. The vector must be a pointer,
and is assumed to refer to a sufficiently large array. No bounds checking is done. */
void SetPointerToElement(Scalar *indexer, const Scalar *vector, long offset)
{
	assert(ScalarIsSane(vector) && !TypeIsInternal(NonPointer(vector->type)));
	
	if(!IsPointer(vector))
		SetError(indexer, ARRAYEXPECTED);
	else {
		SimpleType type = NonPointer(vector->type);
		indexer->type = vector->type;
		if(type == T_INT)
			indexer->value.pointer.sp = vector->value.pointer.sp + offset;
		else if(type == T_LONG)
			indexer->value.pointer.lp = vector->value.pointer.lp + offset;
		else if(type == T_CHAR)
			indexer->value.pointer.cp = vector->value.pointer.cp + offset;
		else if(type == T_STRING)
			indexer->value.pointer.tp = vector->value.pointer.tp + offset;
		else if(type == T_SINGLE)
			indexer->value.pointer.fp = vector->value.pointer.fp + offset;
		else if(type == T_DOUBLE)
			indexer->value.pointer.dp = vector->value.pointer.dp + offset;
		else if(type == T_BOOL)
			indexer->value.pointer.bp = vector->value.pointer.bp + offset;
	}
	
	assert(ScalarIsSane(indexer));
}

/* Sets the scalar from a long, but converting to a given type. */
void SetFromLong(Scalar *dest, long value, SimpleType type)
{
	dest->type = type;
	if(type == T_INT) {
		if(value < SHRT_MIN || SHRT_MAX < value) 
			SetError(dest, OVERFLOWERR);
		else
			dest->value.number.s = (short)value;
	}
	else if(type == T_LONG)
		dest->value.number.l = value;
	else if(type == T_SINGLE)
		dest->value.number.f = (float)value;
	else if(type == T_DOUBLE)
		dest->value.number.d = (double)value;
	else if(type == T_BOOL) {
		if(value != TRUE && value != FALSE) 
			SetError(dest, OUTSIDEDOMAIN);
		else
			dest->value.boolean = (bool)value;
	}
	else 
		SetError(dest, BADARGTYPE);
	
	assert(ScalarIsSane(dest));
}

void SetFromShort(Scalar *dest, short value, SimpleType type)
{
	if(type == T_INT) {
		dest->type = T_INT;
		dest->value.number.s = value;
	}
	else
		SetFromLong(dest, value, type);
}

/* Sets the scalar from a double, but converting to a given type. */
void SetFromDouble(Scalar *dest, double value, SimpleType type)
{
	dest->type = type;
	if(type == T_DOUBLE)
		dest->value.number.d = value;
	else if(type == T_SINGLE) {
		if(fabs(value) > FLT_MAX)
			SetError(dest, OVERFLOWERR);
		else
			dest->value.number.f = (float)value;
	}
	else if(type == T_INT) {
		if(value < ShortMinAsDouble || ShortMaxAsDouble < value) 
			SetError(dest, OVERFLOWERR);
		else
			dest->value.number.s = (short)value; /* TODO should round */
	}
	else if(type == T_LONG) {
		if(value < LongMinAsDouble || LongMaxAsDouble < value)
			SetError(dest, OVERFLOWERR);
		else
			dest->value.number.l = (long)value; /* TODO should round */
	}
	else 
		SetError(dest, BADARGTYPE);
	
	assert(ScalarIsSane(dest));
}

Error SetError(Scalar *v, Error code)
{
	InitScalar(v, T_ERROR, FALSE);
	v->value.error = code;
	
	assert(ScalarIsSane(v));
	
	return code;
}

void SetCharacter(Scalar *v, char c)
{
	v->type = T_CHAR;
	v->value.character = c;
}

void SetBoolean(Scalar *v, bool b)
{
	v->type = T_BOOL;
	v->value.boolean = b;
}

/* Duplicates string values; in other respects, exactly like a straight
assignment: erases type of destination, and doesn't release any memory 
held by it. */
void CopyScalar(Scalar *dest, const Scalar *src)
{
	assert(ScalarIsSane(src) && dest != NULL);

	if(src->type != T_STRING || IsPointer(src))
		*dest = *src;
	else {
		dest->type = T_STRING;
		QsCopy(&dest->value.string, &src->value.string);
	}
	
	assert(ScalarIsSane(dest));
}

/* A value copying constructor - dereferences the source if it's a pointer. */
void CopyDereferencingSource(Scalar *dest, const Scalar *src)
{
	assert(ScalarIsSane(src));
	
	if(!(src->type & T_STRING))
		SetToValue(dest, src);
	else {
		dest->type = T_STRING;
		QsCopy(&dest->value.string, (QString *)GetPointer((Scalar *)src));
	}
}

/* Checks that types of the arguments match. Releases any memory held in
dest, including memory referenced indirectly through a pointer. Dereferences 
any pointers before copying. */
void CopyDereferencingBoth(Scalar *dest, const Scalar *src)
{
	assert(ScalarIsSane(src) && dest != NULL);

	if(NonPointer(dest->type) != T_STRING)
		SetDereferencingBoth(dest, src);
	else if(NonPointer(src->type) != T_STRING)
		SetError(dest, BADARGTYPE);
	else {
		/* Strings. */
		QString *dsp = (QString *)GetPointer(dest);
		
		/* QString doesn't inherently support a distinction between 'null' and 'empty' 
			strings - they're both just 0-length strings with null contents - so a null
			pointer to a string is disallowed. Revisit if this distinction is introduced. */
		assert(dsp != NULL);
		
		QsDispose(dsp);
		QsCopy(dsp, (QString *)GetPointer((Scalar *)src));
	}
	
	assert(ScalarIsSane(dest));
}

/* This is a 'shallow' disposal - doesn't dispose of values referenced through pointers, 
and doesn't dispose of the passed scalar structure itself! */
void DisposeScalar(Scalar *v)
{
	assert(ScalarIsSane(v));

	if(v->type == T_STRING && !IsPointer(v))
		/* IsPointer is redundant, but emphasises that referenced strings are not disposed. */
		QsDispose(&v->value.string);
	*v = m_Empty; /* Relies on constants having been initialised. */
	/*InitScalar(v, T_EMPTY, FALSE);*/
}

/* Converts any numeric type or Boolean; dereferences pointers. */
long GetLong(const Scalar *v)
{
	SimpleType type = NonPointer(v->type);
	
	assert(ScalarIsSane(v));

	if(type == T_INT)
		return (long)(IsPointer(v) ? *v->value.pointer.sp : v->value.number.s);
	else if(type == T_LONG)
		return IsPointer(v) ? *v->value.pointer.lp : v->value.number.l;
	else if(type == T_SINGLE || type == T_DOUBLE) {
		double realValue = GetDouble(v);
		if(realValue < LongMinAsDouble) {
			CauseError(OVERFLOWERR);
			return LONG_MIN;
		}
		else if(realValue > LongMaxAsDouble) {
			CauseError(OVERFLOWERR);
			return LONG_MAX;
		}
		return (long)realValue;
	}
	else if(type == T_BOOL)
		return (long)(IsPointer(v) ? *v->value.pointer.bp : v->value.boolean);
	else
		CauseError(BADARGTYPE);
	
	return 0;
}

double GetDouble(const Scalar *v)
{
	SimpleType type = NonPointer(v->type);
	
	assert(ScalarIsSane(v));

	if(type == T_SINGLE)
		return IsPointer(v) ? *v->value.pointer.fp : v->value.number.f;
	else if(type == T_DOUBLE)
		return IsPointer(v) ? *v->value.pointer.dp : v->value.number.d;
	else if(type == T_INT || type == T_LONG)
		return (double)GetLong(v);
	else
		CauseError(BADARGTYPE);
	
	return 0.0;
}

/* Treats strings and characters as being different, even if a string is one character long. 
Unlike the other ScGet... functions, this one isn't flexible regarding the input type,
but this is acceptable given the very limited uses of the function at present. */
char GetCharacter(const Scalar *v)
{
	assert(ScalarIsSane(v));

	if(NonPointer(v->type) == T_CHAR)
		return IsPointer(v) ? *v->value.pointer.cp : v->value.character;
	else
		CauseError(BADARGTYPE);
	
	return NUL;
}

/* Returns TRUE or FALSE. Type conversion is flexible: handles numbers, strings, chars, pointers also. */
bool GetBoolean(const Scalar *v)
{
	SimpleType type = NonPointer(v->type);
	
	assert(ScalarIsSane(v) && !TypeIsInternal(type));
	
	if(type == T_BOOL)
		return IsPointer(v) ? *v->value.pointer.bp : v->value.boolean;
	else if(type == T_STRING)
		return !QsIsNull((const QString *)GetPointer((Scalar *)v));
	else if(type == T_INT || type == T_LONG)
		return GetLong(v) != 0L;
	else if(type == T_SINGLE || type == T_DOUBLE)
		return GetDouble(v) != 0.0;
	else if(type == T_CHAR)
		return GetCharacter(v) != NUL;

	return FALSE; /* NOTREACHED */
}

void *GetPointer(Scalar *v)
{
	SimpleType type = NonPointer(v->type);
	
	assert(!TypeIsInternal(type));

	if(type == T_INT)
		return IsPointer(v) ? v->value.pointer.sp : &v->value.number.s;
	else if(type == T_LONG)
		return IsPointer(v) ? v->value.pointer.lp : &v->value.number.l;
	else if(type == T_SINGLE)
		return IsPointer(v) ? v->value.pointer.fp : &v->value.number.f;
	else if(type== T_DOUBLE)
		return IsPointer(v) ? v->value.pointer.dp : &v->value.number.d;
	else if(type == T_STRING)
		return IsPointer(v) ? v->value.pointer.tp : &v->value.string;
	else if(type == T_CHAR)
		return IsPointer(v) ? v->value.pointer.cp : &v->value.character;
	else if(type == T_BOOL)
		return IsPointer(v) ? v->value.pointer.bp : &v->value.boolean;

	return NULL; /* NOTREACHED */
}

static Error WriteBoolean(FILE *stream, bool value)
{
	return QsWrite(value ? &m_TrueKeyword : &m_FalseKeyword, stream)
		? SUCCESS : LastIOError();
}

/* Writes an integer, longint, or float, depending on the type.
Positive values are preceded by a single space. */
static Error WriteNumber(FILE *stream, const union NumericalValue *value, SimpleType type)
{
	char buffer[NUM_STR_BUF_LEN];
	NumberToCString(value, type, buffer, TRUE);
	return fputs(buffer, stream) >= 0 ? SUCCESS : LastIOError();
}

DIAGNOSTIC_FN_DECL(unsigned short PointerDisplayValue(const void *));

/* Prints the value to the given output stream in a readable form.
Numbers are always printed in decimal format. Boolean values are printed
as "TRUE" or "FALSE".
	Returns an error indicator, or SUCCESS.
	A pointer or internal value will only be printed if in a DEBUG build: 
otherwise BADARGTYPE is returned. */
Error WriteScalar(FILE *s, const Scalar *v)
{
	if(IsPointer(v))
#ifdef DEBUG
		return fprintf(s, "pointer=....%hX (type=%d)",
				PointerDisplayValue(v->value.pointer.lp), NonPointer(v->type)) > 0
			? SUCCESS : LastIOError();
#else
		return BADARGTYPE;
#endif

	if(TypeIsInternal(v->type)) {
#ifdef DEBUG
		if(v->type == T_EMPTY)
			return fprintf(s, "[empty]") > 0 ? SUCCESS : LastIOError();
		else if(v->type == T_MISSING)
			return fprintf(s, "[missing]") > 0 ? SUCCESS : LastIOError();
		else if(v->type == T_ERROR)
			return fprintf(s, "error=%lu", v->value.error) > 0 ? SUCCESS : LastIOError();
#else
		return BADARGTYPE;
#endif
	}

	if(v->type == T_STRING)
		return QsWrite(&v->value.string, s) ? SUCCESS : LastIOError();
	else if(v->type == T_CHAR)
		return WriteChar(s, v->value.character);
	else if(v->type == T_BOOL)
		return WriteBoolean(s, v->value.boolean);
	else if(TypeIsNumeric(v->type))
		return WriteNumber(s, &v->value.number, v->type);
	else
		return BADARGTYPE;
}

/* The buffer is assumed to be sufficiently capacious. */
void NumberToCString(const union NumericalValue *value, SimpleType type, char *buffer, bool padPositive)
{
	assert(TypeIsNumeric(type));

	if(type == T_INT)
		sprintf(buffer, padPositive ? "% hd" : "%hd", value->s);
	else if(type == T_LONG)
		sprintf(buffer, padPositive ? "% ld" : "%ld", value->l);
	else if(type == T_SINGLE)
		sprintf(buffer, padPositive ? "% G" : "%G", value->f);
	else if(type == T_DOUBLE)
		sprintf(buffer, padPositive ? "% G" : "%G", value->d); /* TODO option to print more sig. digits 
			- will be needed if/when (e.g.) PRINT USING is implemented */ 
}

#define Comparison(a, b) (((a) > (b)) - ((a) < (b)))

static int CompareDirectly(const Scalar *l, const Scalar *r)
{
	assert(l->type == r->type);
	
	switch(l->type) {
		case T_STRING: return QsCompare(&l->value.string, &r->value.string);
		/* Otherwise, follows the same pattern - */
		case T_INT: return Comparison(l->value.number.s, r->value.number.s);
		case T_LONG: return Comparison(l->value.number.l, r->value.number.l);	
		case T_CHAR: return Comparison(l->value.character, r->value.character);
		case T_SINGLE: return Comparison(l->value.number.f, r->value.number.f);
		case T_DOUBLE: return Comparison(l->value.number.d, r->value.number.d);
		case T_BOOL: return Comparison(l->value.boolean, r->value.boolean);
		default: fprintf(stderr, "Can't compare type %u\n", l->type); assert(FALSE); return -1;
	}
}

/* Compare two scalar values.
	The return value is as for strcmp().
	This function tolerates type differences where possible. If a comparable supertype
can't be found, -1 is returned and an error will be set in that parameter.
	Pointers are allowed, and will be dereferenced prior to comparison. */
int Compare(const Scalar *left, const Scalar *right, Error *error)
{
	int comparison = -1;
	
	assert(left != NULL && right != NULL);
	assert(error != NULL);
	assert(TypeIsOrderable(NonPointer(left->type)) && TypeIsOrderable(NonPointer(right->type)));
	
	*error = SUCCESS;
	
	if(left == right || memcmp(left, right, sizeof(*left)) == 0)
		comparison = 0;
	else if(left->type == right->type && !IsPointer(left))
		comparison = CompareDirectly(left, right);
	else {
		Scalar l, r;

		InitScalar(&l, NonPointer(left->type), FALSE);
		InitScalar(&r, NonPointer(right->type), FALSE);
		CopyDereferencingBoth(&l, left);
		CopyDereferencingBoth(&r, right);
		
		if(ComparableTypes(l.type, r.type)) {
			SimpleType comparisonType = LargerType(l.type, r.type);
			enum TypeRule conversion = UsualTypeConversionToProduce(comparisonType);

			if((*error = ChangeType(&l, conversion)) == SUCCESS
			&& (*error = ChangeType(&r, conversion)) == SUCCESS)
				comparison = CompareDirectly(&l, &r);
		}
		else
			*error = BADARGTYPE;
		
		DisposeScalar(&l);
		DisposeScalar(&r);
	}

	return comparison;
}

/* Attempts to convert the value to a different type according to the rule 
given. This cannot be a rule which requires a context - i.e. the type of another,
generally lexically preceding, value cannot influence the conversion. 
	If the conversion isn't possible, an error is set in value and returned.
It's acceptable for the input value to contain an error - this is left intact
in the input, and also returned. */
Error ChangeType(Scalar *value, enum TypeRule rule)
{
	Error result = SUCCESS;
	SimpleType newType = T_MISSING;
	
	assert(value != NULL);
	assert(!Contextual(rule));
	
	newType = TargetType(rule, value->type);
	if(TypeIsInternal(newType))
		result = value->type == T_EMPTY || value->type == T_MISSING ? UNDEFINEDVARORFUNC : BADARGTYPE;
	
	if(result == SUCCESS && value->type != newType) {
		/* TODO these conversions should respect TypeDiscipline options */
		switch(newType) {
			case T_INT:
				SetFromLong(value, GetLong(value), T_INT);
				break;
			case T_LONG:
				SetFromLong(value, GetLong(value), T_LONG);
				break;
			case T_SINGLE:
				SetFromDouble(value, GetDouble(value), T_SINGLE);
				break;
			case T_DOUBLE:
				SetFromDouble(value, GetDouble(value), T_DOUBLE);
				break;
			case T_STRING:
				if(value->type == T_CHAR)
					QsCopyChar(&value->value.string, value->value.character);
				else if(value->type == T_BOOL)
					QsCopy(&value->value.string, value->value.boolean ? &m_TrueKeyword : &m_FalseKeyword);
				else if(TypeIsNumeric(value->type)) {
					char buffer[NUM_STR_BUF_LEN];
					NumberToCString(&value->value.number, value->type, buffer, FALSE);
					QsCopyNTS(&value->value.string, buffer);
				}
				value->type = T_STRING;
				break;
			case T_BOOL: {
				/* Will need to be revisited if parsing supported by this function - 
				  this is asymmetrical with the Boolean -> string case above. */
				bool boolValue = GetBoolean(value);
				DisposeScalar(value);	
				SetBoolean(value, boolValue);
				break;
			}
			case T_CHAR: {
				int code = NUL;
				if(value->type == T_STRING) {
					if((rule & (TD_CHECKED | TD_PRECISE)) && QsGetLength(&value->value.string) > 1)
						result = OVERFLOWERR;
					else if(!QsIsNull(&value->value.string)) {
						code = QsGetFirst(&value->value.string);
						DisposeScalar(value);
					}
				}
				else if(value->type == T_BOOL)
					code = GetBoolean(value) ? '1' : '0';
				else if(TypeIsNumeric(value->type))
					/* If it's a f.p. value which overflows a long, GetLong will cause an overflow error. 
					Otherwise, truncate and rely on optional domain checking. */
					code = (int)GetLong(value);
				
				if((rule & (TD_CHECKED | TD_PRECISE)) && (code < CHAR_MIN || CHAR_MAX < code))
					result = OVERFLOWERR;
			
				if(result == SUCCESS)
					SetCharacter(value, (char)code);
			}
			default:
				if((newType & NUMERIC_TYPES) && value->type == T_STRING) {
					QString s;
					QsCopy(&s, &value->value.string);
					DisposeScalar(value);
					ParseNumber(&s, value);
					QsDispose(&s);
					ChangeType(value, (enum TypeRule)(newType & TD_CHECKED));
				}
				break;
		}
	}
	
	return PropagateError(value, value, result);
}

Error ScalarToString(const Scalar *value, QString *str)
{
	Scalar copy;
	Error result;
	
	QsInitNull(str);
	InitScalar(&copy, NonPointer(value->type), FALSE);
	CopyDereferencingSource(&copy, value);
	if((result = ChangeType(&copy, TR_ANY_TO_STRING)) == SUCCESS)
		QsCopy(str, &copy.value.string);
	DisposeScalar(&copy);
	return result;
}

/* Attempt to change the type of the value, also given a contextual type - e.g. the
type of a preceding parameter or operand. */
Error ChangeTypeWithContext(Scalar *value, SimpleType prevType, enum TypeRule newType)
{
	assert(value != NULL);
	return ChangeType(value, ConcreteConversionFor(value->type, prevType, newType));
}

INLINE int ChrToDigit(char d)
{
	if(d == '1') return 1;
	if(d == '2') return 2;
	if(d == '3') return 3;
	if(d == '4') return 4;
	if(d == '5') return 5;
	if(d == '6') return 6;
	if(d == '7') return 7;
	if(d == '8') return 8;
	if(d == '9') return 9;
	return 0;
}

INLINE int ChrToHexDigit(char x)
{
	int ux = toupper(x);
	if(ux == 'A') return 10;
	if(ux == 'B') return 11;
	if(ux == 'C') return 12;
	if(ux == 'D') return 13;
	if(ux == 'E') return 14;
	if(ux == 'F') return 15;
	return ChrToDigit(x);
}

INLINE SimpleType SetCorrectlySizedInteger(union NumericalValue *num, 
	long val, char typeSpec, bool isShortInteger, bool overflowsShortInteger)
{
	if(TypeFromSpecifier(typeSpec) == T_LONG 
	|| (TypeFromSpecifier(typeSpec) != T_INT 
	  && (!isShortInteger || overflowsShortInteger))) {
		num->l = val;
		return T_LONG;
	}
	else if(overflowsShortInteger) {
		num->l = OVERFLOWERR;
		return T_ERROR;
	}
	else {
		num->s = (short)val;
		return T_INT;
	}
}

INLINE bool IsIntegralTypeSpecifier(char c)
{
	SimpleType t = TypeFromSpecifier(c);
	return TypeIsNumeric(t) && TypeIsExact(t);
}

/* Converts a string representation of a decimal integer into a NumericalValue
structure and returns the type stored.
	Numbers may be preceded by an optional sign (+ or -) and may have a 
trailing type specifier.
	Leading whitespace is not permitted.

e.g.	123			-->		short 123
		123&		-->		long 123
		0			-->		short 0
		1000000		-->		long 1000000
		1000000%	-->		ERROR
		-123		-->		short -123
		+			-->		ERROR
*/
static SimpleType DecQsToIntegral(const QString *s, union NumericalValue *num)
{
	long value = 0;
	int result = EOF;
	char last = QsGetLast(s);
	
	num->l = INPUTFORMAT; /* Assume an error until proven valid. */
	
	if(QsGetLength(s) < NUM_STR_BUF_LEN) {
		char buffer[NUM_STR_BUF_LEN];
		QsCopyToBuffer(s, buffer, NUM_STR_BUF_LEN);
		/* scanf doesn't understand BASIC type specifiers - */
		if(IsIntegralTypeSpecifier(last))
			buffer[strlen(buffer) - 1] = '\0';
		result = sscanf(buffer, "%ld", &value);
	}
	
	return result <= 0 || result == EOF ? T_ERROR 
		: SetCorrectlySizedInteger(num, value, last, 
			SHRT_MIN <= value && value <= SHRT_MAX, value < SHRT_MIN || SHRT_MAX < value);
}

/* Converts a string representing a hexadecimal number into a NumericalValue
structure and returns the type (either T_INT or T_LONG) of the value stored.
The string may optionally start with the characters 0x, 0X, &h, or &H. If it 
is suffixed with an integer type specifier % or & then coercion to that type
is performed, otherwise the type is determined depending on the number of 
characters in the string. */
static SimpleType HexQsToIntegral(const QString *s, union NumericalValue *num)
{
	char last = QsGetLast(s);
	int convStart = 0, convEnd = QsGetLength(s) - 1;
	const char *start = QsGetData(s);

	num->l = INPUTFORMAT; /* Assume an error until proven valid. */
	
	if(IsIntegralTypeSpecifier(last))
		--convEnd;
	if((*start == '&' && toupper(start[1]) == 'H')
	|| (*start == '0' && toupper(start[1]) == 'X'))
		convStart = 2;
		
	if(convStart > convEnd)
		return T_ERROR;
	else {
		long value = 0;
		int i;
	
		for(i = convStart; i <= convEnd; i++) {
			if(!isxdigit(start[i]))
				return T_ERROR;
			value = (value << 4) + ChrToHexDigit(start[i]);
		}
		
		return SetCorrectlySizedInteger(num, value, last, 
			convEnd - convStart < SHORT_BITS / 4, convEnd - convStart >= SHORT_BITS / 4);
	}
}

/* Similar to HexQsToIntegral, but the string must represent an octal number.
Optional prefixes are &o, &O, &. */
static SimpleType OctQsToIntegral(const QString *s, union NumericalValue *num)
{
	char last = QsGetLast(s);
	int convStart = 0, convEnd = QsGetLength(s) - 1;
	const char *start = QsGetData(s);

	num->l = INPUTFORMAT; /* Assume an error until proven valid. */
	
	if(IsIntegralTypeSpecifier(last))
		--convEnd;
	if(*start == '&')
		convStart = 1 + (toupper(start[1]) == 'O');
		
	if(convStart > convEnd)
		return T_ERROR;
	else {
		long value = 0;
		int i;

		for(i = convStart; i <= convEnd; i++) {
			if(!isdigit(start[i]) || start[i] == '8' || start[i] == '9')
				return T_ERROR;
			value = (value << 3) + ChrToDigit(start[i]);
		}
		
		return SetCorrectlySizedInteger(num, value, last, 
			convEnd - convStart < SHORT_BITS / 3, value < SHRT_MIN || SHRT_MAX < value);
	}
}

/* Converts a string representing a decimal number with optional exponent to floating-point form.

The string must have the format
	[<sign>][<whole-part>][<fraction-part>][<exponent-part>][!]
where
	<sign>			is '+' or '-'
	<whole-part>	is a sequence of decimal digits
	<fraction-part>	is a '.' followed be a sequence of decimal digits
	<exponent-part>	is 'd', 'D', 'e', or 'E' followed by an optional sign followed by
					a sequence of decimal digits

Some examples:

	0
	42
	0.0
	1e5			represents 1 * 10^5, i.e. 100000
	.2
	3.141592654 a double precision value
	0.5E+02		represents 0.5 * 10^2, i.e. 50
	1d-3		represents 1 * 10^(-3), i.e. 1 / 1000 = 0.001
	-1.0
	-5e+2		represents -5 * 10^2, i.e. -500
*/
static SimpleType QsToFloating(const QString *s, union NumericalValue *num)
{
 	double value = 0.0;
	int result = EOF;
	SimpleType type = T_SINGLE;
	
	if(QsGetLength(s) < NUM_STR_BUF_LEN) {
		size_t offset = 0, i, len;
		char buffer[NUM_STR_BUF_LEN];
		
		/* scanf doesn't like numbers starting with '.' - */
		if(QsGetFirst(s) == '.')
			buffer[offset++] = '0';
		  
		QsCopyToBuffer(s, &buffer[offset], NUM_STR_BUF_LEN - offset);
		
		/* scanf also differs from BASIC in not liking type specifiers, or 'D' - */
		if(IsTypeSpecifier(QsGetLast(s)))
			buffer[strlen(buffer) - 1] = '\0';
		len = strlen(buffer);
		for(i = 0; i < len; i++)
			if(toupper(buffer[i]) == 'D') {
				type = T_DOUBLE;
				buffer[i] = 'e';
			}
			
		result = sscanf(buffer, "%lf", &value);
	}
	
	if(result <= 0 || result == EOF) {
		num->l = INPUTFORMAT;
		type = T_ERROR;
	}
	else {
		/* TODO dodgy - should decide based on magnitude - digit count? */
		if(type == T_SINGLE	&& fabs(value) > FLT_MAX)
			type = T_DOUBLE;
		
		if(type == T_SINGLE)
			num->f = (float)value;
		else
			num->d = value;
	}
	
	return type;
}

/* Parses a string formatted according to BASIC conventions as a number,
and returns the type of the number (T_INT, T_LONG, T_SINGLE, or T_DOUBLE).
	If the input string isn't in a recognised format, T_ERROR is returned
and an error code is set in val->l. */

#define MAX_LONG_LENGTH 9 /* Assume 32-bit longs for BASIC, even if stored as a larger type. */

static SimpleType QStringToNumericalValue(const QString *s, union NumericalValue *val)
{
	static const char floatIndicatorChars[] = "dDeE.";
	const char *data = QsGetData(s);
	int length = QsGetLength(s);
	SimpleType type = T_ERROR;
	
	if(length == 1 && isdigit(*data)) {
		/* Handle a single-digit integer quickly. */
		val->s = (short)ChrToDigit(*data);
		type = T_INT;
	}
	else if(*data == '0' && length > 1 && toupper(data[1]) == 'X')
		type = HexQsToIntegral(s, val);
	else if(*data == '&' && length > 1)
		type = toupper(data[1]) == 'H' ? HexQsToIntegral(s, val) : OctQsToIntegral(s, val);
	else {
		char last = data[length - 1];
		QString floatIndicators;
		QsInitStaticNTS(&floatIndicators, floatIndicatorChars);
			
		if(length > MAX_LONG_LENGTH /* TODO problematic! */
		|| QsSearchForAny(s, &floatIndicators) != NULL
		|| (IsTypeSpecifier(last) && !TypeIsExact(TypeFromSpecifier(last))))
			type = QsToFloating(s, val);
		else
			type = DecQsToIntegral(s, val);
	}
	
	return type;
}

/* Parses the string as an appropriate numerical type. Returns an error code if the 
string isn't a valid number, SUCCESS otherwise. Any error will also be set in the scalar. */
static Error ParseNumericToken(const QString *s, Scalar *n)
{
	n->type = QStringToNumericalValue(s, &n->value.number);
	return n->type != T_ERROR ? SUCCESS : SetError(n, (Error)n->value.number.l);
}

/* More forgiving parsing. The string may have leading whitespace, and/or trailing
non-numeric characters. */
Error ParseNumber(const QString *str, Scalar *n)
{
	const char *s = QsGetData(str);
	int count = QsGetLength(str);
	
	assert(s != NULL);
	assert(count > 0);
	assert(n != NULL);

	for( ; count > 0 && IsWhiteSpace(*s); ++s, --count)
		;
	
	if(count >= 2
	&&  ((*s == '0' && toupper(s[1]) == 'X')
	  || (*s == '&' && toupper(s[1]) == 'H')))
		while(count > 0 && !isxdigit(s[count - 1])) /* && !IsIntegralTypeSpecifier(s[count - 1]) */
			--count;
	else
		while(count > 0 && !isdigit(s[count - 1])) /* && !IsTypeSpecifier(s[count - 1) */
			--count;

	if(count <= 0)
		return SetError(n, INPUTFORMAT);
	else {
		QString t;
		QsInitStatic(&t, s, count);
		return ParseNumericToken(&t, n);
	}
}

/* Accepts any of "TRUE", "FALSE" (upper, lower case OK), "1", or "0".
	The trailing type specifier '?' may be included, for consistency with numeric types.
	Returns an error code or SUCCESS. Any error is also set in the scalar. */
static Error ParseBoolean(const QString *str, Scalar *val)
{
	const char *start = QsGetData(str);
	int length = QsGetLength(str);

	for( ; length > 0 && IsWhiteSpace(*start); ++start, --length)
		;
		
	while(length > 0 && IsWhiteSpace(start[length - 1]))
		--length;

	if(length > 0 && start[length - 1] == '?')
		--length;
	
	if(length <= 0)
		SetError(val, INPUTFORMAT);
	else {
		QString actual;
		QsInitStatic(&actual, start, length);
		if(QsEqNoCase(&actual, &m_TrueKeyword) || (length == 1 && *start == '1'))
			SetBoolean(val, TRUE);
		else if(QsEqNoCase(&actual, &m_FalseKeyword) || (length == 1 && *start == '0'))
			SetBoolean(val, FALSE);
		else
			SetError(val, INPUTFORMAT);
	}

	return RetrieveError(val);
}

/* Attempts to parse a string as a scalar of given type. Memory will be 
allocated if a string is created.

Returns SUCCESS if the value could be parsed, else an error code. If an error
occurs, the scalar will also be set to the error value. */
Error ParseAs(const QString *s, Scalar *v, SimpleType typeRequired)
{
	Error error = SUCCESS;

	assert(s != NULL && TypeIsVisible(typeRequired));
	
	InitScalar(v, typeRequired, FALSE);

	if(typeRequired == T_STRING)
		QsCopy(&v->value.string, s);
	else if(typeRequired == T_CHAR) {
		/* if(QsGetLength(s) > 1)
			error = INPUTFORMAT; */
		if(!QsIsNull(s))
			v->value.character = QsGetFirst(s);
	}
	else if(TypeIsNumeric(typeRequired)) {
		error = ParseNumber(s, v);
		if(error == SUCCESS && typeRequired != v->type) {
			enum TypeRule conversion = UsualTypeConversionToProduce(typeRequired);
			error = TypeIsExact(typeRequired) && !TypeIsExact(v->type)
				? INPUTFORMAT : ChangeType(v, conversion);
		}
	}
	else if(typeRequired == T_BOOL)
		error = ParseBoolean(s, v);
	
	PropagateError(v, v, error);
	
	assert(ScalarIsSane(v));
	
	return error;
}

/* Parses a string as a BASIC language token - i.e. strings are expected to be quoted. */
Error ParseToken(const QString *token, Scalar *val)
{
	Error e = SUCCESS;
	
	assert(!QsIsNull(token));
	assert(val != NULL);
	
	if(IsQuote(QsGetFirst(token))) {
		assert(QsGetLength(token) >= 2);
		
		InitScalarAsString(val);
		QsGetSubstring(&val->value.string, token, 1, QsGetLength(token) - 2);
	}
	else if(QsGetFirst(token) == '@') {
		int ch;
		if(QsGetLength(token) < 2 || !isxdigit(QsGetCharAt(token, 1))
		|| sscanf(QsGetData(token) + 1, "%x", &ch) != 1)
			e = BADCONSTANT;
		else {
			val->type = T_CHAR;
			val->value.character = (QsChar)ch;
		}
	}
	else
		e = ParseNumericToken(token, val);
	
	if(e != SUCCESS)
		e = ParseBoolean(token, val);
	
	/* Assume being used on program text rather than for data read from a file by i/o statements
		and functions, so translate INPUTFORMAT error - */
	return e == SUCCESS ? SUCCESS : SetError(val, e == INPUTFORMAT ? BADCONSTANT : e);
}
