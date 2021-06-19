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
#define T_IS_DEFAULT 2 /* If the default type is changed, also change the DefaultType function! */
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

/* This could be encoded in the enumeration order, but that seems fragile. 
	T_CHAR is given a bigger domain than T_ERROR, which is incorrect with one-byte chars, but keeps more of the
	'visible' types ahead of the 'internal' ones. */
static const SimpleType m_DomainSizeOrder[] = {T_STRING, T_DOUBLE, T_SINGLE, T_LONG, T_INT, T_CHAR, T_ERROR,  
												T_BOOL, T_EMPTY, T_MISSING};

/* Special parameter definitions - */
static const struct Parameter m_ArrayIndex = {LITERAL, TR_SUBSCRIPT, NULL, NO_NAME, MAX_DIMENSIONS, FALSE};

static const struct Parameter m_AnyArgs = {LITERAL, TR_ANY, NULL, NO_NAME, MAX_TOKENS, FALSE};

INLINE const struct PrimitiveTypeCharacteristics *GetTypeCharacteristicsByCode(SimpleType t)
{
	const struct PrimitiveTypeCharacteristics *ptc;

	assert((t & T_POINTER) == 0);
	
	for(ptc = m_Type; t != ptc->code && ptc < &m_Type[NUM_SIMPLE_TYPES]; ptc++)
		;

	return ptc == &m_Type[NUM_SIMPLE_TYPES] ? NULL : ptc;
}

/* Special-cased because called frequently. */
SimpleType TypeFromSpecifier(char c)
{
	const struct PrimitiveTypeCharacteristics *ptc;
	
	for(ptc = m_Type; ptc < &m_Type[NUM_SIMPLE_TYPES]; ptc++)
		if(c != NUL && c == ptc->specifier)
			return ptc->code;
	return T_EMPTY;
}

bool IsTypeSpecifier(char c)
{
	SimpleType t = TypeFromSpecifier(c);
	return t != T_MISSING && t != T_EMPTY;
}

SimpleType DefaultType(const QString *forName)
{
	char initial = forName != NULL && !QsIsNull(forName) ? (char)toupper(QsGetFirst(forName)) : NUL;
	
	return 'A' <= initial && initial <= 'Z' && Proc()->defaultTypeForLetter[initial - 'A'] != NUL /* TODO ASCII ordering only ... */
		? Proc()->defaultTypeForLetter[initial - 'A']
		: T_SINGLE;
}

SimpleType TypeForName(const QString *forName)
{
	return IsTypeSpecifier(QsGetLast(forName)) ? TypeFromSpecifier(QsGetLast(forName)) : DefaultType(forName);
}

char SpecifierFromType(SimpleType t) { return GetTypeCharacteristicsByCode(t)->specifier; }

size_t StorageSize(SimpleType t) { return GetTypeCharacteristicsByCode(t)->storageSize; }

SimpleType NextMoreCapaciousType(SimpleType t) { return GetTypeCharacteristicsByCode(t)->nextMoreCapacious; }

/*bool TypeIsInternal(SimpleType t) { return (GetTypeCharacteristicsByCode(t)->properties & T_IS_INTERNAL) != 0; }*/

bool TypeIsVisible(SimpleType t) { return (GetTypeCharacteristicsByCode(t)->properties & T_IS_VISIBLE) != 0; }

bool TypeIsNumeric(SimpleType t) { return (GetTypeCharacteristicsByCode(t)->properties & T_IS_NUMERIC) != 0; }

bool TypeIsComparable(SimpleType t) { return (GetTypeCharacteristicsByCode(t)->properties & T_IS_COMPARABLE) != 0; }

bool TypeIsOrderable(SimpleType t) { return (GetTypeCharacteristicsByCode(t)->properties & T_IS_ORDERABLE) != 0; }

bool TypeIsExact(SimpleType t) { return (GetTypeCharacteristicsByCode(t)->properties & T_IS_EXACT) != 0; }

bool TypeIsTextual(SimpleType t) { return (GetTypeCharacteristicsByCode(t)->properties & T_IS_TEXTUAL) != 0; }

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

SimpleType LargerType(SimpleType t1, SimpleType t2)
{
	int i;
	for(i = 0; i < NUM_SIMPLE_TYPES; i++)
		if(t1 == m_DomainSizeOrder[i] || t2 == m_DomainSizeOrder[i])
			return m_DomainSizeOrder[i];
	
	assert(FALSE);
	return t1; /* May well be wrong ... but shouldn't happen. */
}

SimpleType TypeUsuallyProducedBy(enum TypeRule tc)
{
	if(!Contextual(tc) && tc != TR_NON_TEXT) {
		int i;
		for(i = 0; i < NUM_SIMPLE_TYPES; i++)
			if(tc & m_DomainSizeOrder[i])
				return m_DomainSizeOrder[i];
#ifdef DEBUG
		fprintf(stderr, "[Unexpected TypeRule %d]\n", tc);
#endif
		assert(FALSE);
	}
	return DefaultType(NULL);
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
	
	if(required == TR_SAME)
		/* Actually shouldn't matter, but err on the side of caution - */
		return StrictTypeConversionFor(previous);
	else if(required == TR_ASSIGNMENT || required == TR_PROMOTE)
		return previous == T_LONG && actual == T_BOOL ? TR_INT_TO_LONG : UsualTypeConversionToProduce(previous);
	else
		/* Ensure conversion fails if haven't been able to make concrete - */
		return Contextual(required) ? TR_IRRELEVANT : required;
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
			SimpleType best = T_MISSING;
			int closest = 0, i;
			unsigned sourceNumeric = stc->properties & T_IS_NUMERIC, sourceExact = stc->properties & T_IS_EXACT,
				sourceTextual = stc->properties & T_IS_TEXTUAL;
			
			for(i = 0; i < NUM_SIMPLE_TYPES; i++) {
				const struct PrimitiveTypeCharacteristics *ptc;
				SimpleType prospect = m_DomainSizeOrder[i];
				
				if((prospect & required)
				&& prospect != source
				&& !((ptc = GetTypeCharacteristicsByCode(prospect))->properties & T_IS_INTERNAL)) {
					int closeness = ((ptc->properties & T_IS_NUMERIC) == sourceNumeric)
						+ ((ptc->properties & T_IS_EXACT) == sourceExact)
						+ ((ptc->properties & T_IS_TEXTUAL) == sourceTextual);
					if(closeness >= closest) { /* note >= here; favour smaller-sized types */
						closest = closeness;
						best = prospect;
					}
				}
			}
			return best;
		}
	}
}

/* applied may be a subprogram, function, operator, or array variable */
static const struct Parameter *GetPrototype(const BObject *applied, int *numFormals)
{
	const struct Parameter *proto;
	
	*numFormals = INT_MIN;

	if(applied == NULL)
		proto = NULL;
	else if(IsVariable(applied)) {
		bool array = IsArray(applied);
		proto = array ? &m_ArrayIndex : NULL;
		*numFormals = array ? 1 : 0;
	}
	else if(applied->category == STATEMENT) {
		if(IsMacro(applied->value.statement)) {
			proto = &m_AnyArgs;
			*numFormals = 1;
		}
		else {
			proto = applied->value.statement->formal;
			*numFormals = applied->value.statement->formalCount;
		}
	}
	else if(applied->category == OPERATOR) {
		proto = ParametersForOperator(applied->value.opRef);
		*numFormals = OperandCount(applied->value.opRef);
	}
	else if(applied->category == FUNCTION) {
		if(applied->value.function->numArgs == FN_VAR_ARGS) {
			proto = &m_AnyArgs;
			*numFormals = 1;
		}
		else {
			proto = applied->value.function->parameter;
			*numFormals = applied->value.function->numArgs;
		}
	}
	else
		proto = NULL;

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
static Error Satisfy(const struct Parameter *f, BObject *a, SimpleType prevType, bool varArgs)
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
			if(f->defaultValue != NULL) {
				/*printf("Default retrieved: ");
				WriteScalar(stdout, f->defaultValue);*/
				CopyScalar(&a->value.scalar, f->defaultValue);
			}
			else {
				/*printf("Default val null\n");*/
				return ER_PARAMETER_NOT_OPTIONAL;
			}
		}
		else if(Undefined(a))
			return UNDEFINEDVARORFUNC;
		
		return ChangeType(&a->value.scalar, ConcreteConversionFor(a->value.scalar.type, prevType, f->type));
	}
	else if(IsVarParam(f)) {
		if(!IsVariable(a))
			return Undefined(a) ? UNDEFINEDVARORFUNC : ER_EXPECTED_VARIABLE;
		else if(((f->kind & VARIABLE_IS_ARRAY) != 0) != IsArray(a))
			return (f->kind & VARIABLE_IS_ARRAY) ? ARRAYEXPECTED : SCALAREXPECTED;
		else if(!TypeMayBeOKWithContext(NonPointer(VarData(a)->type), prevType, f->type))
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
	/* This would be a good place to use a dynamically sized auto array - */
	/*bool supplied[MAX_FORMALS];*/

	assert(formal != NULL || formalCount <= 0);
	assert(actual != NULL || actualCount == 0);
	assert(formalCount <= MAX_FORMALS);

	/* First pass: match actuals to formals and check/convert/insert defaults. */
	
	/*if(formalCount > 0)
		memset(supplied, 0, sizeof(bool) * formalCount);*/
	
	for(an = 0; an < actualCount && error == SUCCESS; an++) {
		const struct Parameter *f = FormalForActual(formal, formalCount, an);
		SimpleType typeOfPrevious = an == 0 ? T_MISSING : GetSimpleType(&actual[an - 1]);
		
		if(f != NULL)
			/*supplied[f - formal] = TRUE;*/ supplied |= (1 << (f - formal));
		
		error = Satisfy(f, &actual[an], typeOfPrevious, formalCount < 0);
	}
	
	if(error == ER_PARAMETER_NOT_OPTIONAL)
		sprintf(Proc()->additionalErrorInfo, "Parameter #%u has no default value and must be supplied", an);
		
	/* Second pass: check that an actual was supplied for each required formal. */
	
	if(supplied != allFormals) {
		for(fn = 0; fn < formalCount && error == SUCCESS; fn++)
			if(!(supplied & (1 << fn))) { /* !supplied[fn] */
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
	return proto == NULL ? ARRAYEXPECTED : Conform(proto, numFormals, actual, actualCount);
}

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
	short f;
	unsigned short tn, an = 0;
	int nesting = 0;
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

#if 0
		const struct Parameter *f;
		const QString *t = &ts->rest[tn];
		
		Nest(t, &nesting);
		an += QsGetFirst(t) == ';';
		f = FormalForActual(ts->command->formal, ts->command->formalCount, an);
		assert(f != NULL); /* Since should already have been run. */
		if(f == NULL)
			return FALSE;
		if(f->kind == LITERAL) {
			return FALSE;
			/*if(nesting == 0) {
				BObject o;
				ConvertToObject(t, &o, Proc()->callNestLevel);
				predictable &= (o.category == LITERAL && (f->type & GetSimpleType(&o)));
				RemoveObject(&o, FALSE);
			}
			else
				predictable = FALSE;*/
		}
	}
	
	return predictable;
}
#endif