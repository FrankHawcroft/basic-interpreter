/****** vars.c ******/

/*
	$VER: vars.c 0.16A (6.30.2015)

	Variables, including arrays and CONSTs, and related statements (DATA etc.)
*/

#include <string.h>
#include <ctype.h>
#include "interpreter.h"
#include "builtin.h"
#include "process.h"
#include "platform.h"
#include "heap.h"
#include "buffer.h"
#include "options.h"

static void PackDimensions(union Dimension *d, const ArraySubscript *expanded);
static void UnpackDimensions(const union Dimension *d, ArraySubscript *expanded);

/* Check whether a variable can be defined - i.e. doesn't clash with an existing
definition.
	Variables must be uniquely named within a scope, but local variables can hide
global ones unless the global variable is a shared array created using DIM SHARED.

Arguably, local variables should be able to mask functions (and even subprograms),
but don't think that would be consistent with BASIC convention, though it's more
sensible. */
bool CanDefineVariable(const QString *mootedName, short mootedCallNestLevel)
{
	const BObject *existing = LookUpIgnoringType(mootedName, mootedCallNestLevel);
	return existing == NULL
		|| ((mootedCallNestLevel > SCOPE_MAIN || mootedCallNestLevel == SCOPE_STATIC)
			&& IsVariable(existing)
			&& !(existing->category & VARIABLE_IS_SHARED)
			&& LookUpIgnoringType(mootedName, SCOPE_MAIN) == existing);
}

void InitVariable(struct Variable *var, SimpleType type, bool isPointer)
{
	int i;
	InitScalar(&var->value, type, isPointer);
	for(i = 0; i != PACKED_DIM_COUNT; i++)
		var->dim.few[i] = -1;
}

BObject *DefineVariable(const QString *name, SimpleType type, short callNestLevel, bool isPointer)
{
	BObject *v = CreateDefinition(name, NULL, SCALAR_VAR, callNestLevel);
	if(v != NULL)
		InitVariable(&v->value.variable, type, isPointer);
	return v;
}

INLINE short EffectiveDefinitionCallNestLevel(short currentCallNestLevel, unsigned flags)
{
	return currentCallNestLevel == SCOPE_MAIN && (flags & (VARIABLE_IS_SHARED | VARIABLE_IS_CONST))
		? SCOPE_GLOBAL : currentCallNestLevel;
}

static void CreateOptionallyTypedVariable(const struct Process *proc,
	BObject *obj, const QString *fullName, unsigned flags, bool isPointer)
{
	BObject *var;
	SimpleType type = TypeForName(proc, fullName);
	short callNestLevel = EffectiveDefinitionCallNestLevel(proc->callNestLevel, flags);
	char last = QsGetLast(fullName);
	
	if(!isalnum(last) && IsTypeSpecifier(last)) {
		QString bareName;
		QsGetSubstring(&bareName, fullName, 0, QsGetLength(fullName) - 1);
		var = DefineVariable(&bareName, type, callNestLevel, isPointer);
		QsDispose(&bareName);
	}
	else
		var = DefineVariable(fullName, type, callNestLevel, isPointer);

	if(var == NULL)
		SetObjectToError(obj, NOMEMORY);
	else {
		var->category |= flags;
		SetSymbolReference(obj, var->category | VARIABLE_IS_POINTER, VarPtr(var));
	}
}

static Error AssignToActual(struct Variable *local, const BObject *argument, enum SymbolType kind)
{
	if(kind & IS_VARIABLE) {
		assert(GetSimpleType(argument) == NonPointer(local->value.type));

		if(IsArray(argument)) {
			ArraySubscript unpacked[MAX_DIMENSIONS + 1];
			UnpackDimensions(&VarPtr(argument)->dim, unpacked);
			PackDimensions(&local->dim, unpacked);
		}
		SetAsPointer(&local->value, VarData(argument));
		
		return SUCCESS;
	}
	else {
		assert(kind == LITERAL);
		assert(argument->category == LITERAL);
		assert(argument->value.scalar.type == local->value.type);

		CopyScalar(&local->value, &argument->value.scalar);
		
		return SUCCESS;
	}
}

/* It's assumed that type and kind checking has been done (see semantics.c/Conform), and that the
	variable has not already been defined in the current context. */
Error CreateArgumentVariable(const struct Parameter *formal, const BObject *argument)
{
	BObject *local = DefineVariable(&formal->name, TypeUsuallyProducedBy(formal->type),
		Proc()->callNestLevel, IsVarParam(formal));
	local->category |= (IsVarParam(formal) ? VARIABLE_IS_REF : 0);
	return local == NULL ? NOMEMORY : AssignToActual(VarPtr(local), argument, formal->kind);
}

Error AssignToStaticParameter(const struct Parameter *formal, struct Variable *local, const BObject *argument)
{
	if(local == NULL) {
		BObject *defn = LookUp(&formal->name, SCOPE_STATIC);	
		local = defn != NULL && IsVariable(defn) ? VarPtr(defn) : NULL;
		if(local == NULL) {
			defn = DefineVariable(&formal->name, TypeUsuallyProducedBy(formal->type), SCOPE_STATIC, IsVarParam(formal));
			if(defn == NULL)
				return NOMEMORY;
			defn->category |= (IsVarParam(formal) ? VARIABLE_IS_REF : 0);
			local = VarPtr(defn);
		}
	}
	else if(!IsPointer(&local->value) && local->value.type == T_STRING)
		QsDispose(&local->value.value.string);
	
	return AssignToActual(local, argument, formal->kind);
}

Error ShareVariable(const QString *name, const BObject *global)
{
	BObject *local = DefineVariable(name, NonPointer(VarData(global)->type), Proc()->callNestLevel, TRUE);
	if(local == NULL)
		return NOMEMORY;
	else {
		local->category = (IsArray(global) ? SHARED_ARRAY : SHARED_VAR) | VARIABLE_IS_REF;
		return AssignToActual(VarPtr(local), global, local->category);
	}
}

/*bool IsVariable(const BObject *obj)
{
	assert(obj != NULL);
	return (obj->category & IS_VARIABLE) != 0;
}*/

struct Variable *VarPtr(const BObject *obj)
{
	assert(IsVariable(obj));
	return (obj->category & VARIABLE_IS_POINTER) ? obj->value.varRef : &obj->value.variable;
}

Scalar *VarData(const BObject *obj)
{
	return &(VarPtr(obj)->value);
}

/* Dimensions follow the convention that the rightmost 'column' dimension is the
'minor' or fastest-varying in memory; i.e. row-major array layout applies. */
static size_t ArraySize(const ArraySubscript *dimension, SimpleType type)
{
	size_t units = 1;
	int subscriptIdx;
	
	for(subscriptIdx = 0;
	    subscriptIdx < MAX_DIMENSIONS && dimension[subscriptIdx] >= 0;
	    subscriptIdx++)
		/* See IndexArrayDirectly for a description of the effect of Proc()->arrayIndexBase. */
		units *= dimension[subscriptIdx] + (Proc()->arrayIndexBase == 0);

	return units * StorageSize(type);
}

/* Calculate number of cells (as opposed to bytes of storage) in the array. 
	Yes, sizeof(char) == 1 but this makes it slightly clearer that the calculation
	is sensitive to the type used for 'character' in EValue. */
#define ArrayExtent(dimensions) (ArraySize((dimensions), T_CHAR) / sizeof(char))

static void DisposeArray(BObject *var)
{
	void *memToFree = GetPointer(VarData(var));
	
	assert(IsArray(var));
	
	if(NonPointer(VarData(var)->type) == T_STRING) {
		size_t numLeftToFree;
		QString *strToFree;
		ArraySubscript dimension[MAX_DIMENSIONS + 1];

		UnpackDimensions(&VarPtr(var)->dim, dimension);

		for(strToFree = (QString *)memToFree, numLeftToFree = ArrayExtent(dimension); 
		  numLeftToFree != 0; numLeftToFree--, strToFree++)
			QsDispose(strToFree);
	}

	Dispose(memToFree);
}

void DisposeVariableObject(BObject *var)
{
	if(!(var->category & VARIABLE_IS_POINTER)) {
		if(IsArray(var) && !(var->category & VARIABLE_IS_REF))
			DisposeArray(var);
		else
			DisposeScalar(VarData(var)); /* Doesn't free memory through pointers. */
	}
}

static unsigned short Dimensions(const BObject *var)
{
	ArraySubscript dim[MAX_DIMENSIONS + 1];
	unsigned short dimensions = 0;
	
	if(IsArray(var)) {
		UnpackDimensions(&VarPtr(var)->dim, dim);
		for( ; dimensions < MAX_DIMENSIONS && dim[dimensions] >= 0; dimensions++)
			;
	}
	return dimensions;
}

bool IsArray(const BObject *var)
{
	return IsVariable(var) && ((var->category & VARIABLE_IS_ARRAY) || VarPtr(var)->dim.few[0] != -1);
}

static void PackDimensions(union Dimension *d, const ArraySubscript *expanded)
{
	int i;
	for(i = 0; i < PACKED_DIM_COUNT && expanded[i] != -1; i++)
		d->few[i] = expanded[i];
	for( ; i < PACKED_DIM_COUNT; i++)
		d->few[i] = -1;
}

static void UnpackDimensions(const union Dimension *d, ArraySubscript *expanded)
{
	int i;
	for(i = 0; i < MAX_DIMENSIONS + 1; i++)
		expanded[i] = -1;
	memcpy(expanded, d->few, sizeof(ArraySubscript) * PACKED_DIM_COUNT);
}

/* Index an array, producing a reference to a single element, or a subarray of lower dimension.
	By default, subscripts - indexes of elements or subarrays - start at 1. The OPTION BASE
statement can be used to change this, to start at element 0.
	An array has elements up to and including its dimensioned size. E.g.
	
	DIM A(10)

creates a single row or one dimensional array of ten (OPTION BASE 1) or eleven (OPTION BASE 0) elements,
which can be subscripted with A(1) up to A(10) or A(0) up to A(10), respectively for the two BASE options.
	This array, though:
	
	DIM B(1, 10)

is classed as a two-dimensional array in BASIC, while A is one-dimensional. And if OPTION BASE is 0,
B will be a two-row array with a subarray or row B(0), as well as B(1). */
static Error IndexArrayDirectly(struct Variable *indexer, const struct Variable *array, const ArraySubscript *subscript)
{
	long offset = 0;
	int subscriptIdx;
	ArraySubscript terminatedDimension[MAX_DIMENSIONS + 1];
	
	/* Copy the array's dimensions into a vector terminated with -1. */
	UnpackDimensions(&array->dim, terminatedDimension);

	/* Calculate the offset of the element/subarray. */
	for(subscriptIdx = 0; subscript[subscriptIdx] >= 0; subscriptIdx++) {
		bool lastDimension = terminatedDimension[subscriptIdx + 1] < 0;
			
		/* Check for out-of-bounds access. */
		if(subscript[subscriptIdx] < (ArraySubscript)Proc()->arrayIndexBase
		|| subscript[subscriptIdx] > terminatedDimension[subscriptIdx]) {
			InitScalar(&indexer->value, NonPointer(array->value.type), FALSE);
			return BADSUBSCRIPT;
		}

		/* Add offset for this subarray. */
		offset += (subscript[subscriptIdx] - Proc()->arrayIndexBase)
			* (lastDimension ? 1 : ArrayExtent(&terminatedDimension[subscriptIdx + 1]));
	}
	
	/* Copy across subarray dimensions. */
	PackDimensions(&indexer->dim, &terminatedDimension[subscriptIdx]);

	/* Set the pointer to the element/subarray. */
	SetPointerToElement(&indexer->value, &array->value, offset);
	
	return SUCCESS;
}

#if BIG_ARRAYS
#define SUBSCRIPT_TYPE T_LONG
#else
#define SUBSCRIPT_TYPE T_INT
#endif

Error IndexArray(struct Variable *indexer, const struct Variable *array, const BObject *subscript, unsigned count)
{
	ArraySubscript actualSubscript[MAX_DIMENSIONS + 1]; /* + 1 includes space for a marker ending the subscripts. */
	unsigned s;
			
	assert(array->dim.few[0] != -1);
	assert(count <= MAX_DIMENSIONS);

	for(s = 0; s != count; s++) {
		assert(subscript[s].category == LITERAL);
		assert(subscript[s].value.scalar.type == SUBSCRIPT_TYPE);

#if BIG_ARRAYS
		actualSubscript[s] = subscript[s].value.scalar.value.number.l;
#else	
		actualSubscript[s] = subscript[s].value.scalar.value.number.s;
#endif
	}
		
	/* An end-marker simplifies iterating through the subscripts. */
	actualSubscript[s] = -1; 
	
	return IndexArrayDirectly(indexer, array, actualSubscript);
}

const char *ArraySizeDescription(void)
{
#if BIG_ARRAYS
	return "BIG-ARRAYS";
#else
	return "";
#endif
}

void Let_(BObject *arg, unsigned count)
{
	CopyDereferencingBoth(VarData(&arg[0]), &arg[1].value.scalar);
}

void Swap_(BObject *arg, unsigned count)
{
	Scalar t, *x = VarData(&arg[0]), *y = VarData(&arg[1]);
	SetToValue(&t, x);
	SetDereferencingBoth(x, y);
	SetDereferencingBoth(y, &t);
}

void Dim_(BObject *arg, unsigned count)
{
	struct Variable *newArray = VarPtr(&arg[0]);
	ArraySubscript dimension[MAX_DIMENSIONS + 1];
	unsigned argIdx, dimIdx;

	/* Check the array dimensions are legal. */
	for(argIdx = 1, dimIdx = 0; argIdx < count && dimIdx < MAX_DIMENSIONS; argIdx++, dimIdx++)
		if((dimension[dimIdx] = GetLong(&arg[argIdx].value.scalar)) < 0) {
			CauseError(BADSUBSCRIPT);
			return;
		}
	
	if(dimIdx >= MAX_DIMENSIONS && argIdx + 1 < count) {
		CauseError(BADSUBSCRIPT);
		return;
	}

	for( ; dimIdx <= MAX_DIMENSIONS; dimIdx++)
		dimension[dimIdx] = -1;

	if(newArray->dim.few[0] != -1)
		CauseError(REDIMENSION);
	else {
		size_t requiredMem = ArraySize(dimension, NonPointer(newArray->value.type));
		void *mem = TolerantNew((long)requiredMem);

		if(mem != NULL) {
			memset(mem, NUL, requiredMem);
			SetPointerTo(&newArray->value, mem, NonPointer(newArray->value.type));
			PackDimensions(&newArray->dim, dimension);
		}
		else
			CauseError(NOMEMORY);
	}
}

void Erase_(BObject *arg, unsigned count)
{
	unsigned n;
	
	for(n = 0; n != count; n++) {
		if(Proc()->callNestLevel > SCOPE_MAIN && (arg[n].category & (VARIABLE_IS_REF | VARIABLE_IS_SHARED))) {
			CauseError(ER_ERASE_IN_SUB);
			return;
		}
		else {
			DisposeArray(&arg[n]);
			InitVariable(VarPtr(&arg[n]), NonPointer(VarData(&arg[n])->type), TRUE);
		}
	}
}

void OptionBase_(BObject *arg, unsigned count)
{
	ArraySubscript baseSubscript = GetLong(&arg[0].value.scalar);

	if(baseSubscript != 0 && baseSubscript != 1)
		CauseError(BADSUBSCRIPT);
	else
		Proc()->arrayIndexBase = baseSubscript;
}

void LBound_(Scalar *result, const BObject *arg, unsigned count)
{
	SetFromLong(result, Proc()->arrayIndexBase, SUBSCRIPT_TYPE);
}

void UBound_(Scalar *result, const BObject *arg, unsigned count)
{
	const struct Variable *a = VarPtr(&arg[0]);
	ArraySubscript d = GetLong(&arg[1].value.scalar) - 1; /* Doesn't use OPTION BASE. */
	ArraySubscript unpacked[MAX_DIMENSIONS + 1];
	
	UnpackDimensions(&a->dim, unpacked);
	if(d >= MAX_DIMENSIONS || unpacked[d] < 0)
		SetError(result, BADSUBSCRIPT);
	else
		SetFromLong(result, unpacked[d], SUBSCRIPT_TYPE);
}

void Data_(const QString *toks, unsigned nToks)
{
	if(nToks > 1 && Proc()->readPosition == NULL)
		Proc()->readPosition = Proc()->currentStatementStart;
}

void ResetDataReadPointer(void)
{
	Proc()->readPosition = NULL;
}

void Read_(BObject *arg, unsigned count)
{
	unsigned varIdx = 0;
	struct TokenSequence tokens;
	struct Process *proc = Proc();

	CreateTokenSequence(&tokens, 40);
	
	/* Avoid reading DATA defined in the prelude, if not executing the prelude.
		Should the prelude contain DATA definitions, assume that its use of them will be self-contained,
		and it doesn't intend to make data available to the main program - this helps avoid unexpected
		data being used by main program READ statements, as well as saving scanning time. */
	if(proc->readPosition == NULL)
		proc->readPosition = proc->currentStatementStart <= PrimaryBufferBase(proc->buffer)
			? FileBufferBase(proc->buffer) : PrimaryBufferBase(proc->buffer);

	do {
		const char *nextStmt = proc->readPosition;
		Error result = SUCCESS;
		const struct TokenSequence *cached = GetFromCache(proc, nextStmt);
		struct TokenSequence tokenSource;
		bool isDataStatement;
		
		if(cached != NULL) {
			isDataStatement = !IsSubprogram(cached->command) && cached->command->method.macro == Data_;
			tokenSource = *cached;
			nextStmt = cached->next;
		}
		else {
			result = Tokenise(&nextStmt, &tokens, FALSE);
			isDataStatement = result == SUCCESS && QsEqNoCase(&tokens.statementName, &g_DataKeyword);
			tokenSource = tokens;
		}

		/*PrintTokSeq(&tokenSource);*/

		if(isDataStatement) {
			unsigned short tokNum;
			
			for(tokNum = proc->tokenIndex != -1 ? (unsigned short)proc->tokenIndex : 0;
				tokNum < tokenSource.length && varIdx < count && result == SUCCESS;
				tokNum += 2, varIdx++) {
				Scalar nextLiteral;
				const QString *t = &tokenSource.rest[tokNum];

				/* Do a superficial syntactic check on the sequence of tokens. */
				if(QsGetFirst(t + 1) != ',' && !IsTerminator(t + 1)) {
					DisposeTokenSequence(&tokens);
					CauseError(BADSYNTAX);
					return;
				}

				/* Parse the next token. Traditionally, unquoted strings are allowed in DATA statements,
				so treat anything which fails to parse as a string.*/
				if(ParseToken(t, &nextLiteral) != SUCCESS) {
					InitScalarAsString(&nextLiteral);
					QsCopy(&nextLiteral.value.string, t);
				}

				/* Convert the parsed value to the variable's type. */
				result = ChangeType(&nextLiteral, UsualTypeConversionToProduce(GetSimpleType(&arg[varIdx])));
				
				if(result != SUCCESS) {
					DisposeTokenSequence(&tokens);
					CauseError(result);
					return;
				}

				/* Assign it to the variable. */
				CopyDereferencingBoth(VarData(&arg[varIdx]), &nextLiteral);
			}

			/* Move to next statement if all tokens processed from this one. */

			if(tokNum >= tokenSource.length) {
				proc->tokenIndex = -1;
				proc->readPosition = nextStmt;
			}
			else
				proc->tokenIndex = tokNum;
		}
		else
			/* Not a DATA statement, so continue searching. */
			proc->readPosition = nextStmt;

		if(cached == NULL)
			ClearTokenSequence(&tokens);
	}
	while(WithinFileBuffer(proc->buffer, proc->readPosition) && varIdx < count);

	DisposeTokenSequence(&tokens);

	if(!WithinFileBuffer(proc->buffer, proc->readPosition - 1) && varIdx < count)
		CauseError(ER_OUT_OF_DATA);
}

void Restore_(const QString *toks, unsigned nToks)
{
	Error error = SUCCESS;
	
	Proc()->readPosition = nToks != 0 && IsName(&toks[0]) ? FindReferencedLabel(&toks[0], &error) : NULL;
	Proc()->tokenIndex = -1;
	
	if(error != SUCCESS)
		CauseError(error);
}

static char Letter(const QString *s)
{
	char c = QsGetLength(s) == 1 ? toupper(QsGetFirst(s)) : NUL;
	return 'A' <= c && c <= 'Z' ? c : NUL;
}

static void DefTypeImpl(SimpleType type, const QString *toks, unsigned nToks)
{
	unsigned n;

	if(nToks < 1) {
		CauseError(BADSYNTAX);
		return;
	}

	for(n = 0; n + 1 < nToks; n++) {
		char c = Letter(&toks[n]), from = 1, to = 0;

		if(c == NUL && QsGetFirst(&toks[n]) != ',') {
			CauseError(BADSYNTAX);
			return;
		}

		if('A' <= c && c <= 'Z')
			from = to = c;

		if(QsGetFirst(&toks[n + 1]) == '-') {
			if(n + 3 >= nToks) {
				CauseError(BADSYNTAX);
				return;
			}
			to = Letter(&toks[n + 2]);
			if(to == NUL || to <= from) {
				CauseError(BADSYNTAX);
				return;
			}
			n += 2; /* Skip 'from' letter and '-'; then will be incremented again in for loop */
		}

		for(c = from; c <= to; c++)
			SetDefaultType(c, type);
	}
}

void DefBln_(const QString *toks, unsigned nToks) { DefTypeImpl(T_BOOL, toks, nToks); }
void DefChr_(const QString *toks, unsigned nToks) { DefTypeImpl(T_CHAR, toks, nToks); }
void DefDbl_(const QString *toks, unsigned nToks) { DefTypeImpl(T_DOUBLE, toks, nToks); }
void DefInt_(const QString *toks, unsigned nToks) { DefTypeImpl(T_INT, toks, nToks); }
void DefLng_(const QString *toks, unsigned nToks) { DefTypeImpl(T_LONG, toks, nToks); }
void DefSng_(const QString *toks, unsigned nToks) { DefTypeImpl(T_SINGLE, toks, nToks); }
void DefStr_(const QString *toks, unsigned nToks) { DefTypeImpl(T_STRING, toks, nToks); }

#define PeekImpl(name, type, typecode) \
	void name(Scalar *result, const BObject *arg, unsigned count) \
	{ \
		if(!Opts()->unsafe) \
			SetError(result, ER_UNSAFE); \
		else \
			SetFromLong(result, *(type *)arg[0].value.scalar.value.number.l, typecode); \
	}

PeekImpl(Peek_, char, T_INT)
PeekImpl(PeekW_, short, T_INT)
PeekImpl(PeekL_, long, T_LONG)

#define PokeImpl(name, type, member) \
	void name(BObject *arg, unsigned count) \
	{ \
		if(!Opts()->unsafe) \
			CauseError(ER_UNSAFE); \
		else \
			*((type *)arg[0].value.scalar.value.number.l) = (type)arg[1].value.scalar.value.number.member; \
	}

PokeImpl(Poke_, char, s)
PokeImpl(PokeW_, short, s)
PokeImpl(PokeL_, long, l)

void VarPtr_(Scalar *result, const BObject *arg, unsigned count)
{
	if(!Opts()->unsafe)
		SetError(result, ER_UNSAFE);
	else
		SetFromLong(result, (long)GetPointer(VarData(&arg[0])), T_LONG);
}

void ConstConvert(unsigned index, const QString *token, BObject *result)
{
	const struct Process *proc = Proc();
	if(index == 0 && IsValidName(token)) {
		if(CanDefineVariable(token, proc->callNestLevel))
			CreateOptionallyTypedVariable(proc, result, token, VARIABLE_IS_CONST, FALSE);
		else {
			const BObject *existing = LookUpIgnoringType(token, proc->callNestLevel);
			SetObjectToError(result, IsVariable(existing) && (existing->category & VARIABLE_IS_CONST)
				? MODIFYCONST : REDEFINE);
		}
	}
	else
		ConvertToObject(token, result, SCOPE_CURRENT);
}

void ConvertWithArrayVarCreation(unsigned index, const QString *token, BObject *result, short callNestLevel, bool shared)
{
	if(index == 0 && IsValidName(token)) {
		if(CanDefineVariable(token, callNestLevel))
			CreateOptionallyTypedVariable(Proc(), result, token, VARIABLE_IS_ARRAY | (shared ? VARIABLE_IS_SHARED : 0), TRUE);
		else {
			const BObject *existing = LookUpIgnoringType(token, shared ? SCOPE_MAIN : callNestLevel);
			if(!IsVariable(existing)
			|| (IsTypeSpecifier(QsGetLast(token)) && NonPointer(VarData(existing)->type) != TypeForName(Proc(), token))
			|| !IsPointer(VarData(existing)))
				SetObjectToError(result, REDEFINE);
			else
				SetSymbolReference(result, existing->category | VARIABLE_IS_POINTER, VarPtr(existing));
		}
	}
	else
		ConvertToObject(token, result, callNestLevel);
}

void DimConvert(unsigned index, const QString *token, BObject *result)
{
	ConvertWithArrayVarCreation(index, token, result, Proc()->callNestLevel, FALSE);
}

void DimSharedConvert(unsigned index, const QString *token, BObject *result)
{
	ConvertWithArrayVarCreation(index, token, result, SCOPE_MAIN, TRUE);
}

void AssignConvert(unsigned index, const QString *token, BObject *result)
{
	bool isArray = index == 1 && QsGetFirst(token - 1) == '(';
	bool isScalar = index == 0 && isalpha(QsGetFirst(token));
	if((isArray || isScalar) && IsValidName(token)) {
		const struct Process *proc = Proc();
		if(CanDefineVariable(token, proc->callNestLevel))
			CreateOptionallyTypedVariable(proc, result, token, isArray ? VARIABLE_IS_ARRAY : 0, FALSE);
		else {
			const BObject *existing = LookUpIgnoringType(token, proc->callNestLevel);
			if(!IsVariable(existing)
			|| (IsTypeSpecifier(QsGetLast(token)) && NonPointer(VarData(existing)->type) != TypeForName(proc, token)))
				SetObjectToError(result, REDEFINE);
			else if(existing->category & VARIABLE_IS_CONST)
				SetObjectToError(result, MODIFYCONST);
			else if(isScalar && IsArray(existing))
				SetObjectToError(result, ARRAYEXPECTED);
			else if(isArray && !IsArray(existing))
				SetObjectToError(result, SCALAREXPECTED);
			else
				SetSymbolReference(result, existing->category | VARIABLE_IS_POINTER, VarPtr(existing));
		}
	}
	else
		ConvertToObject(token, result, SCOPE_CURRENT);
}

extern BObject *LookUpLocal(const QString *symbol, short callNestLevel);

void LocalScalarAssignConvert(unsigned index, const QString *token, BObject *result)
{
	if(index == 0) {
		const struct Process *proc = Proc();
		const BObject *existing = LookUpLocal(token, proc->callNestLevel);
		if(existing == NULL)
			CreateOptionallyTypedVariable(proc, result, token, 0, FALSE);
		else
			SetSymbolReference(result, existing->category | VARIABLE_IS_POINTER, VarPtr(existing));
	}
	else
		ConvertToObject(token, result, SCOPE_CURRENT);
}
