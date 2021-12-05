/****** semantics.c ******/

/*
	$VER: semantics.c 0.16A (5.8.2015)

	The semantics of the interpreter - rules about types and conversion, and parameter matching.
*/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include "interpreter.h"
#include "process.h"

/* Encapsulates semantic rules for the simple types. */
struct PrimitiveTypeCharacteristics {
	SimpleType code;
	size_t storageSize; 
	unsigned canCompareWith;
		/* Bitmask of SimpleTypes to which values can be compared.
		If these aren't defined symmetrically, type conversion will behave strangely. */
	enum TypeRule usuallyProducedBy;
		/* 'Most common' type conversion that produces values of this type. Must be unique -
		definition of function types relies on this. */
	enum TypeRule strictMatch;
		/* Exact matching rule. */
	SimpleType nextMoreCapacious;
	SimpleType closestPeer;
	unsigned properties;
		/* Bitmask of various characteristics - see below. */
	char specifier; 
};

#define T_IS_INTERNAL 1
#define T_IS_DEFAULT 2 /* If the default type is changed, also change DefaultType and DEFAULT_IMPLIED_TYPE! */
#define T_IS_VISIBLE 4
#define T_IS_NUMERIC 8
#define T_IS_TEXTUAL 16
#define T_IS_COMPARABLE 32
#define T_IS_ORDERABLE 64
#define T_IS_EXACT 128

static const struct PrimitiveTypeCharacteristics m_Type[NUM_SIMPLE_TYPES] = {
	{T_INT, 
		sizeof(short), NUMERIC_TYPES | T_BOOL, TR_NUM_TO_INT, TR_INT_ONLY, T_LONG, T_LONG,
		T_IS_VISIBLE | T_IS_NUMERIC | T_IS_COMPARABLE | T_IS_ORDERABLE | T_IS_EXACT, '%'},
	{T_LONG,
		sizeof(long), NUMERIC_TYPES | T_BOOL, TR_NUM_TO_LONG, TR_LONG_ONLY, T_SINGLE, T_INT,
		T_IS_VISIBLE | T_IS_NUMERIC | T_IS_COMPARABLE | T_IS_ORDERABLE | T_IS_EXACT, '&'},
	{T_SINGLE,
		sizeof(float), NUMERIC_TYPES, TR_NUM_TO_SINGLE, TR_SINGLE_ONLY, T_DOUBLE, T_DOUBLE,
		T_IS_DEFAULT | T_IS_VISIBLE | T_IS_NUMERIC | T_IS_COMPARABLE | T_IS_ORDERABLE, '!'},
	{T_DOUBLE,
		sizeof(double), NUMERIC_TYPES, TR_SINGLE_TO_DOUBLE, TR_DOUBLE_ONLY, T_STRING, T_SINGLE,
		T_IS_VISIBLE | T_IS_NUMERIC | T_IS_COMPARABLE | T_IS_ORDERABLE, '#'},
	{T_STRING,
		sizeof(QString), TEXTUAL_TYPES, TR_CHAR_TO_STRING, TR_STRING_ONLY, T_STRING, T_CHAR,
		T_IS_VISIBLE | T_IS_TEXTUAL | T_IS_COMPARABLE | T_IS_ORDERABLE, '$'},
	{T_CHAR,
		sizeof(char), TEXTUAL_TYPES, TR_STRING_TO_CHAR, TR_CHAR_ONLY, T_STRING, T_STRING,
		T_IS_VISIBLE | T_IS_TEXTUAL | T_IS_COMPARABLE | T_IS_ORDERABLE, '@'},
	{T_BOOL,
		sizeof(bool), INTEGRAL_TYPES, TR_LOGICAL, TR_BOOL_ONLY, T_INT, T_INT,
		T_IS_VISIBLE | T_IS_COMPARABLE | T_IS_ORDERABLE | T_IS_EXACT, '?'},
	{T_MISSING, 
		0, 0, TR_IRRELEVANT, TR_IRRELEVANT, T_MISSING, T_MISSING,
		T_IS_INTERNAL, NUL},
	{T_ERROR,
		sizeof(Error), T_ERROR | T_INT | T_LONG, TR_IRRELEVANT, TR_IRRELEVANT, T_INT, T_MISSING,
		T_IS_INTERNAL | T_IS_COMPARABLE | T_IS_EXACT, NUL},
	{T_EMPTY, 
		0, 0, TR_IRRELEVANT, TR_IRRELEVANT, T_EMPTY, T_MISSING,
		T_IS_INTERNAL, NUL}
};

/* This could be encoded in the enumeration order, but that seems more fragile than isolating it here. 
	T_CHAR and T_BOOL are treated as having bigger domains than T_ERROR, which is incorrect based on
	their actual sizes, but keeps 'visible' types ahead of the 'internal' ones. */
#define STRING_CHARACTERISTICS	&m_Type[4]
#define DOUBLE_CHARACTERISTICS	&m_Type[3]
#define SINGLE_CHARACTERISTICS	&m_Type[2]
#define LONG_CHARACTERISTICS	&m_Type[1]
#define INT_CHARACTERISTICS		&m_Type[0]
#define CHAR_CHARACTERISTICS	&m_Type[5]
#define BOOL_CHARACTERISTICS	&m_Type[6]
#define ERROR_CHARACTERISTICS	&m_Type[8]
#define EMPTY_CHARACTERISTICS	&m_Type[9]
#define MISSING_CHARACTERISTICS	&m_Type[7]

static const struct PrimitiveTypeCharacteristics *const m_DomainSizeOrder[] = {
	STRING_CHARACTERISTICS, DOUBLE_CHARACTERISTICS, SINGLE_CHARACTERISTICS,
	LONG_CHARACTERISTICS, INT_CHARACTERISTICS, CHAR_CHARACTERISTICS,
	BOOL_CHARACTERISTICS, ERROR_CHARACTERISTICS, EMPTY_CHARACTERISTICS,
	MISSING_CHARACTERISTICS};

/* Special parameter definitions - */
static const struct Parameter m_ArrayIndex = {LITERAL, TR_SUBSCRIPT, NULL, NO_NAME, MAX_DIMENSIONS, FALSE};

static const struct Parameter m_AnyArgs = {LITERAL, TR_ANY, NULL, NO_NAME, MAX_TOKENS, FALSE};

INLINE const struct PrimitiveTypeCharacteristics *GetTypeCharacteristicsByCode(SimpleType t)
{
	assert((t & T_POINTER) == 0);
	assert(T_EMPTY <= t && t <= T_BOOL);
	
	switch(t) {
		case T_STRING: return STRING_CHARACTERISTICS;
		case T_DOUBLE: return DOUBLE_CHARACTERISTICS;
		case T_SINGLE: return SINGLE_CHARACTERISTICS;
		case T_LONG: return LONG_CHARACTERISTICS;
		case T_INT: return INT_CHARACTERISTICS;
		case T_CHAR: return CHAR_CHARACTERISTICS;
		case T_ERROR: return ERROR_CHARACTERISTICS;
		case T_BOOL: return BOOL_CHARACTERISTICS;
		case T_EMPTY: return EMPTY_CHARACTERISTICS;
		case T_MISSING: return MISSING_CHARACTERISTICS;
		default: assert(FALSE); return EMPTY_CHARACTERISTICS;
	}
}

/* Special-cased because called frequently. */
SimpleType TypeFromSpecifier(char c)
{
	const struct PrimitiveTypeCharacteristics *ptc;
	
	for(ptc = &m_Type[0]; ptc <= BOOL_CHARACTERISTICS; ptc++)
		if(c == ptc->specifier)
			return ptc->code;
	return T_EMPTY;
}

bool IsTypeSpecifier(char c)
{
	return !isalnum(c) && !TypeIsInternal(TypeFromSpecifier(c));
}

char SpecifierFromType(SimpleType t) { return GetTypeCharacteristicsByCode(t)->specifier; }

size_t StorageSize(SimpleType t) { return GetTypeCharacteristicsByCode(t)->storageSize; }

SimpleType NextMoreCapaciousType(SimpleType t) { return GetTypeCharacteristicsByCode(t)->nextMoreCapacious; }

bool TypeIsVisible(SimpleType t) { return (GetTypeCharacteristicsByCode(t)->properties & T_IS_VISIBLE) != 0; }

bool TypeIsNumeric(SimpleType t) { return (GetTypeCharacteristicsByCode(t)->properties & T_IS_NUMERIC) != 0; }

bool TypeIsComparable(SimpleType t) { return (GetTypeCharacteristicsByCode(t)->properties & T_IS_COMPARABLE) != 0; }

bool TypeIsOrderable(SimpleType t) { return (GetTypeCharacteristicsByCode(t)->properties & T_IS_ORDERABLE) != 0; }

bool ComparableTypes(SimpleType t1, SimpleType t2) { return (GetTypeCharacteristicsByCode(t1)->canCompareWith & (unsigned)t2) != 0; }

enum TypeRule UsualTypeConversionToProduce(SimpleType t) { return GetTypeCharacteristicsByCode(t)->usuallyProducedBy; }

enum TypeRule StrictTypeConversionFor(SimpleType t) { return GetTypeCharacteristicsByCode(t)->strictMatch; }

void SetDefaultType(char initialLetter, SimpleType t)
{
	initialLetter = (char)toupper(initialLetter);
	
	assert('A' <= initialLetter && initialLetter <= 'Z');
	assert((t & T_POINTER) == 0);
	assert(!TypeIsInternal(t));
	
	Proc()->defaultTypeForLetter[initialLetter - 'A'] = t;
}

static SimpleType DefaultType(const struct Process *proc, const QString *forName)
{
	char initial = (char)toupper(QsGetFirst(forName));
	return 'A' <= initial && initial <= 'Z'
		? proc->defaultTypeForLetter[initial - 'A'] : DEFAULT_IMPLIED_TYPE;
}

SimpleType TypeForName(const struct Process *proc, const QString *forName)
{
	SimpleType explicit = TypeFromSpecifier(QsGetLast(forName));
	return !TypeIsInternal(explicit) ? explicit : DefaultType(proc, forName);
}

SimpleType LargerType(SimpleType t1, SimpleType t2)
{
	int i;
	for(i = 0; i < NUM_SIMPLE_TYPES; i++)
		if(t1 == m_DomainSizeOrder[i]->code || t2 == m_DomainSizeOrder[i]->code)
			return m_DomainSizeOrder[i]->code;
	
	assert(FALSE);
	return t1; /* May well be wrong ... but shouldn't happen. */
}

SimpleType TypeUsuallyProducedBy(enum TypeRule tc)
{
	if(!Contextual(tc) && tc != TR_NON_TEXT) {
		int i;
		for(i = 0; i < NUM_SIMPLE_TYPES; i++)
			if(tc & m_DomainSizeOrder[i]->code) /* favour larger types */
				return m_DomainSizeOrder[i]->code;
#ifdef DEBUG
		fprintf(stderr, "[Unexpected TypeRule %d]\n", tc);
#endif
		assert(FALSE);
	}
	return DEFAULT_IMPLIED_TYPE;
}

/* Returns TRUE iff it is possible for some value of the simple type to 
satisfy the rule. Note that this can't handle rules that involve looking at a 
previous item. Note also that this function is somewhat misleading, because 
it doesn't have any information about the contents of the value: overflow
can't be taken into consideration. It can only state whether it MIGHT be 
possible to convert a value of this type to another type. */
static bool TypeMayBeOK(const struct PrimitiveTypeCharacteristics *actualType, enum TypeRule requirement)
{
	bool textual, numeric;
	
	assert(actualType != NULL);
	
	textual = (actualType->properties & T_IS_TEXTUAL) != 0, numeric = (actualType->properties & T_IS_NUMERIC) != 0;
	
	/* Some cases not expressed by current TypeRule values aren't covered here. */
	return (requirement & TD_FLEXIBLE)
		|| (requirement & actualType->code) /* All strict conversions, and some others. */
		|| ((requirement & TD_LOOSE) && (requirement & NUMERIC_TYPES)
			&& !textual)
		|| ((requirement & TD_LOOSE) && (requirement & TEXTUAL_TYPES)
			&& textual)
		|| ((requirement & TD_LOOSE) && (requirement & T_BOOL))
		|| ((requirement & TD_CHECKED) && (requirement & NUMERIC_TYPES)
			&& numeric)
		|| ((requirement & TD_CHECKED) && (requirement & INTEGRAL_TYPES)
			&& (numeric || actualType->code == T_BOOL))
		|| ((requirement & TD_CHECKED) && (requirement & TEXTUAL_TYPES)
			&& textual)
		|| ((requirement & TD_PRECISE) && (requirement & INTEGRAL_TYPES)
			&& (actualType->code & INTEGRAL_TYPES));
}

/* This function does type checking with a 'contextual' item, typically a
previous argument to a command which can take parameters of different types but
requires the parameter types to be compatible in some way. If the contextual
type isn't needed by the rule, it is ignored (see ConcreteConversionFor) and this 
is the same as TypeMayBeOK. */
static bool TypeMayBeOKWithContext(SimpleType actual, SimpleType previous, enum TypeRule required)
{
	return TypeMayBeOK(GetTypeCharacteristicsByCode(actual), ConcreteConversionFor(actual, previous, required));
}

enum TypeRule ConcreteConversionFor(SimpleType actual, SimpleType previous, enum TypeRule required)
{
	assert((actual & T_POINTER) == 0);
	assert((previous & T_POINTER) == 0);
	assert(!Contextual(required) || previous != T_MISSING);
	
	switch(required) {
		case TR_ASSIGNMENT:
		case TR_PROMOTE:
			return actual == T_BOOL && previous == T_LONG ? TR_INT_TO_LONG : UsualTypeConversionToProduce(previous);
		case TR_SAME:
			/* Actually shouldn't matter, but err on the side of caution - */
			return StrictTypeConversionFor(previous);
		default:
			assert(!Contextual(required));
			return required;
	}
}

SimpleType TargetType(enum TypeRule required, SimpleType source)
{
	if(source & required)
		/* No conversion required. */
		return source;
	else {
		const struct PrimitiveTypeCharacteristics *stc = GetTypeCharacteristicsByCode(source);
		
		if(!TypeMayBeOK(stc, required))
			return T_MISSING;
		else if(required & stc->closestPeer)
			return stc->closestPeer;
		else {
			const struct PrimitiveTypeCharacteristics *best = MISSING_CHARACTERISTICS;
			int closest = 0, i;
			unsigned sourceNumeric = stc->properties & T_IS_NUMERIC, sourceExact = stc->properties & T_IS_EXACT,
				sourceTextual = stc->properties & T_IS_TEXTUAL;
			
			/* Assume not converting to an internal type - hence T_BOOL is the upper limit - */
			for(i = 0; i <= 6; i++) {
				const struct PrimitiveTypeCharacteristics *prospect = m_DomainSizeOrder[i];			
				if((prospect->code & required) && prospect->code != source) {
					int closeness = ((prospect->properties & T_IS_NUMERIC) == sourceNumeric)
						+ ((prospect->properties & T_IS_EXACT) == sourceExact)
						+ ((prospect->properties & T_IS_TEXTUAL) == sourceTextual);
					if(closeness >= closest) { /* note >= here; favour smaller-sized types */
						closest = closeness;
						best = prospect;
					}
				}
			}
			return best->code;
		}
	}
}

/* applied may be a subprogram, function, operator, or array variable */
const struct Parameter *GetPrototype(const BObject *applied, int *numFormals)
{
	const struct Parameter *proto;
	
	assert(applied != NULL);
	assert(numFormals != NULL);

	if(applied->category == OPERATOR) {
		proto = ParametersForOperator(applied->value.opRef);
		*numFormals = OperandCount(applied->value.opRef);
	}
	else if(applied->category == FUNCTION) {
		proto = applied->value.function->parameter;
		if((*numFormals = applied->value.function->numArgs) == FN_VAR_ARGS) {
			proto = &m_AnyArgs;
			*numFormals = 1;
		}
	}
	else if(IsVariable(applied)) {
		bool array = IsArray(applied);
		proto = array ? &m_ArrayIndex : NULL;
		*numFormals = array ? 1 : 0;
	}
	else if(applied->category == STATEMENT) {
		proto = applied->value.statement->formal;
		*numFormals = applied->value.statement->formalCount;
		if(IsMacro(applied->value.statement)) {
			proto = &m_AnyArgs;
			*numFormals = 1;
		}
	}
	else {
		proto = NULL;
		*numFormals = INT_MIN;
	}

	return proto;
}

/* Assumes a C-like convention that a 'varargs' parameter (see statements.c/UNLIMITED),
if present, is last in the vector. */
const struct Parameter *FormalForActual(const struct Parameter *formal, int formalCount, unsigned actual)
{
	int fn;
	for(fn = 0; formal != NULL && fn < formalCount; fn++)
		if(formal[fn].maxCount > actual)
			return &formal[fn];
		else
			actual -= formal[fn].maxCount;
	return NULL;
}

/* These rely on ConvertToObject conventions - */
#define Unsupplied(actual) ((actual)->value.scalar.type == T_MISSING)
#define Undefined(actual) ((actual)->value.scalar.type == T_EMPTY)

/* f = formal, a = actual ... */
static Error Satisfy(const struct Parameter *f, BObject *a, const BObject *prevActual, bool varArgs)
{	
	assert(f == NULL || f->kind == LABEL || f->kind == LITERAL || IsVarParam(f));
	assert(a != NULL);

	if(ObjectIsError(a))
		return ObjectAsError(a);
	else if(f == NULL)
		return varArgs ? SUCCESS : BADARGCOUNT;
	else if(f->kind == LITERAL) {
		Error error = DereferenceObject(a);
		if(error != SUCCESS)
			return error;
		else if(Unsupplied(a)) {
			if(f->defaultValue != NULL)
				CopyScalar(&a->value.scalar, f->defaultValue);
			else
				return ER_PARAMETER_NOT_OPTIONAL;
		}
		else if(Undefined(a))
			return UNDEFINEDVARORFUNC;
		
		return ChangeType(&a->value.scalar,
			ConcreteConversionFor(a->value.scalar.type,
				prevActual == NULL ? T_MISSING : GetSimpleType(prevActual), f->type));
	}
	else if(IsVarParam(f)) {
		if(!IsVariable(a))
			return Undefined(a) ? UNDEFINEDVARORFUNC : ER_EXPECTED_VARIABLE;
		else if(((f->kind & VARIABLE_IS_ARRAY) != 0) != IsArray(a))
			return (f->kind & VARIABLE_IS_ARRAY) ? ARRAYEXPECTED : SCALAREXPECTED;
		else if(!TypeMayBeOKWithContext(NonPointer(VarData(a)->type),
		  prevActual == NULL ? T_MISSING : GetSimpleType(prevActual), f->type))
			return BADARGTYPE;
	}
	else if(f->kind == LABEL)
		return a->category == LABEL ? SUCCESS : UNDEFINEDLABEL;
	
	return SUCCESS;
}

#define MAX_FORMALS (CHAR_BIT * sizeof(unsigned long))

/* Attempts to make a sequence of actual parameters conform to a set of formals.
Returns an error if a kind or type check or conversion fails.
Any default placeholders are assumed to have been inserted already. */
Error Conform(const struct Parameter *formal, int formalCount, BObject *actual, unsigned actualCount)
{
	Error error = SUCCESS;
	unsigned an;
	int fn;
	unsigned long supplied = 0, allFormals = (1 << formalCount) - 1;
	const BObject *prevActual;

	assert(formal != NULL || formalCount <= 0);
	assert(actual != NULL || actualCount == 0);
	assert(formalCount <= MAX_FORMALS);

	/* First pass: match actuals to formals and check/convert/insert defaults. */
	
	for(an = 0, prevActual = NULL; an < actualCount && error == SUCCESS; prevActual = &actual[an], an++) {
		const struct Parameter *f = FormalForActual(formal, formalCount, an);
		if(f != NULL)
			supplied |= (1 << (f - formal));
		error = Satisfy(f, &actual[an], prevActual, formalCount < 0);
	}
	
	if(error == ER_PARAMETER_NOT_OPTIONAL)
		sprintf(Proc()->additionalErrorInfo, "Parameter #%u has no default value and must be supplied", an);
		
	/* Second pass: check that an actual was supplied for each required formal. */
	
	if(supplied != allFormals) {
		for(fn = 0; fn < formalCount && error == SUCCESS; fn++)
			if(!(supplied & (1 << fn))) {
				sprintf(Proc()->additionalErrorInfo,
					"Parameter #%d has no default value and must be supplied", fn + 1);
				error = BADARGCOUNT;
			}
	}
	
	return error;
}

Error ConformForApplication(const BObject *applied, BObject *actual, unsigned actualCount)
{
	int numFormals;
	const struct Parameter *proto = GetPrototype(applied, &numFormals);
	/* Error result here assumes usage in expr rather than command context - */
	if(proto == NULL)
		return IsEmpty(applied) ? UNDEFINEDVARORFUNC : ARRAYEXPECTED;
	else if(numFormals == actualCount) /* assumes operator or function parameter conventions! */
		return ConformQuickly(proto, actual, numFormals);
	else
		return Conform(proto, numFormals, actual, actualCount);
}

Error ConformQuickly(const struct Parameter *formal, BObject *actual, int count)
{
	Error error = SUCCESS;
	int n;

	assert(formal != NULL);
	assert(actual != NULL && count >= 1);
	assert(count <= MAX_FORMALS);

	for(n = 0; n < count && error == SUCCESS; n++) {
		const struct Parameter *f = &formal[n];
		BObject *a = &actual[n];

		if(f->kind == LITERAL && (error = DereferenceObject(a)) == SUCCESS) { /* deref propagates errors */
			SimpleType prevType = n == 0 ? T_MISSING : GetSimpleType(&actual[n - 1]);
			error = ChangeType(&a->value.scalar, ConcreteConversionFor(a->value.scalar.type, prevType, f->type));
		}
		else if(ObjectIsError(a))
			error = ObjectAsError(a);
	}
	
	return error;
}

#if 0
Error ConformForAssignment(BObject *actual)
{
	Error error = SUCCESS;
	
	assert(actual != NULL);
	
	if(ObjectIsError(&actual[0])) /* array index out of bounds, or variable not found */
		error = ObjectAsError(&actual[0]);
	else {
		error = DereferenceObject(&actual[1]);
		if(error == SUCCESS)
			error = ChangeType(&actual[1]->value.scalar, TD_CHECKED | NonPointer(VarData(&actual[0])->type));
	}
	
	return error;
}
#endif

SimpleType TypeOfToken(const QString *t)
{
	if(IsTypeSpecifier(QsGetLast(t)))
		return TypeFromSpecifier(QsGetLast(t));
	else {
		BObject obj;
		SimpleType st;
		
		ConvertToObject(t, &obj, Proc()->callNestLevel);
		st = GetSimpleType(&obj);
		RemoveObject(&obj, FALSE);
		return st;
	}
}

bool SemanticallyPredictable(const struct TokenSequence *ts)
{
	unsigned short tn;
	bool predictable = TRUE;
	SimpleType tPrev = T_EMPTY;

	if(IsMacro(ts->command))
		return FALSE;
	if(ts->command->formalCount == 0)
		return TRUE;
	for(tn = 0; tn + 1 < ts->length && predictable; tn++) {
		const QString *t = &ts->rest[tn];
		const struct Parameter *f = FormalForActual(ts->command->formal, ts->command->formalCount, tn);
		enum TypeRule tr = ConcreteConversionFor(TypeOfToken(t), tPrev, f->type);
		predictable &= (QsGetFirst(t) != '('
			&& (f->kind == LABEL || ((TypeOfToken(t) & tr) && ((f->kind & IS_VARIABLE) != 0) == IsName(t))));
		tPrev = TypeOfToken(t);
	}
	return predictable;
}
