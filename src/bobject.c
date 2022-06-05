/****** bobject.c ******/

/*
	$VER: bobject.c 0.16A (13.5.2015)

	A convenience container, to hold any BASIC object.
*/

#include <stdio.h>
#include <string.h>
#include "interpreter.h"
#include "process.h"
#include "buffer.h"

void InitObject(BObject *obj, enum SymbolType kind)
{
	assert(obj != NULL);
	
	obj->category = kind;
	if(kind == LITERAL)
		obj->value.scalar = *g_Empty;
	else if((kind & IS_VARIABLE) && !(kind & VARIABLE_IS_POINTER))
		InitVariable(&obj->value.variable, T_EMPTY, FALSE); 
	else
		obj->value.labelPos = NULL;
}

bool IsEmpty(const BObject *obj)
{
	return obj == NULL  || (obj->category == LITERAL && GetSimpleType(obj) <= T_MISSING);
}

/* Duplicate an object. Literal values are cloned, but for other types, a simple structure copy is sufficient.
	The order of source type checking is optimised for EvalPreconverted. */
void CopyObject(BObject *dest, const BObject *source)
{
	assert(dest != NULL && source != NULL);

	if(source->category & VARIABLE_IS_POINTER)
		*dest = *source;
	else if(source->category == LITERAL) {
		dest->category = LITERAL;
		CopyScalar(&dest->value.scalar, &source->value.scalar);
	}
	else if(IsVariable(source)) { /* embedded variable */
		dest->category = source->category;
		CopyScalar(&dest->value.variable.value, &source->value.variable.value);
	}
	else
		*dest = *source;
}

/* Returns the BASIC-visible 'type' of the object, or T_EMPTY if the concept of a
scalar type is not appropriate. */
SimpleType GetSimpleType(const BObject *obj)
{
	assert(obj != NULL);

	if(obj->category == LITERAL)
		return obj->value.scalar.type;
	else if(IsVariable(obj))
		return NonPointer(VarData(obj)->type);
	else if(obj->category == OPERATOR)
		return TypeUsuallyProducedBy(Range(obj->value.opRef));
	else if(obj->category == FUNCTION)
		return TypeUsuallyProducedBy(obj->value.function->type);
	else
		return T_EMPTY;
}

void DisposeObjectContents(BObject *obj)
{
	assert(obj != NULL);

	if(IsVariable(obj))
		DisposeVariableObject(obj);
	else if(obj->category == STATEMENT)
		DisposeStatement(obj->value.statement);
	else if(obj->category == FUNCTION)
		DisposeFunction(obj->value.function);
	else
		DisposeIfScalar(obj);
}

void DisposeIfScalar(BObject *obj)
{
	if(obj->category == LITERAL)
		DisposeScalar(&obj->value.scalar);
}

bool Dynamic(const BObject *obj)
{
	return obj->category == LITERAL && TypeIsTextual(GetSimpleType(obj));
}

/* Returns the error represented by the object, if it is one; else SUCCESS. */
Error ObjectAsError(const BObject *obj)
{
	assert(obj != NULL);
	
	return GetSimpleType(obj) == T_ERROR ? obj->value.scalar.value.error : SUCCESS;
}

/* Sets the object to the given error. The object passed is assumed to be uninitialised. */
void SetObjectToError(BObject *obj, Error error)
{
	assert(obj != NULL);

	obj->category = LITERAL;
	SetError(&obj->value.scalar, error);
	Proc()->additionalErrorInfo[0] = NUL; /* Try to avoid an old additional message being printed. */
}

void SetObjectToErrorWithAdditionalMessage(BObject *obj, Error error, const char *msgFmt, const QString *contextualObject)
{
	assert(obj != NULL);
	assert(msgFmt != NULL);
	
	SetObjectToError(obj, error);
	SetAdditionalErrorMessage(msgFmt, QsGetData(contextualObject), QsGetLength(contextualObject));
}

bool IsUserDefined(const BObject *obj)
{
	return IsVariable(obj)
		|| obj->category == LABEL
		|| (obj->category == STATEMENT && IsSubprogram(obj->value.statement))
		|| (obj->category == FUNCTION && IsDefFunction(obj->value.function)); 
}

/* Converts a token to a BObject. */
void ConvertToObject(const QString *token, BObject *obj, short callNestLevel)
{
	if(IsName(token) || !IsLiteral(token)) {
		/* A symbol. Look up only here - creation as a side
		effect of particular kinds of statements must be handled by
		a specialised function - see AssignConvert etc.
			Because the named Boolean constants TRUE and FALSE are defined
		in the prelude, it's safe to assume that something which looks like
		a name should be looked up, rather than parsed. If TRUE and FALSE
		were treated as literals, this assumption would need to change. */
			
		BObject *definition = LookUpCheckingType(token, callNestLevel);
		if(definition != NULL) {
			if(IsVariable(definition))
				SetSymbolReference(obj, definition->category | VARIABLE_IS_POINTER, VarPtr(definition));
			else
				*obj = *definition;
		}
		else if(IsTypeSpecifier(QsGetLast(token)) && LookUpIgnoringType(token, callNestLevel) != NULL)
			SetObjectToErrorWithAdditionalMessage(obj, BADARGTYPE, "Defined type differs for: %.*s", token);
		else {
			obj->category = LITERAL;
			InitScalar(&obj->value.scalar, QsEqNoCase(token, &g_Missing) ? T_MISSING : T_EMPTY, FALSE);
			/*if(!QsEqNoCase(token, &g_Missing))*/
				SetAdditionalErrorMessage("Not found: %.*s", QsGetData(token), QsGetLength(token));
		}
	}
	else {
		obj->category = LITERAL;
		ParseToken(token, &obj->value.scalar);
	}
}

/* Only meaningful for symbolic objects (non-literals). */
void SetSymbolReference(BObject *obj, enum SymbolType kind, void *value)
{
	assert(obj != NULL);
	assert(value != NULL);
		
	if(kind & VARIABLE_IS_POINTER)
		obj->value.varRef = value;
	else if(kind == LABEL)
		obj->value.labelPos = (const char *)value;
	else if(kind == OPERATOR)
		obj->value.opRef = (const struct Operator *)value;
	else if(kind == STATEMENT)
		obj->value.statement = value;
	else if(kind == FUNCTION)
		obj->value.function = value;
	else if(kind == PUNCTUATION)
		obj->value.punctuation = (const struct PunctuationSymbol *)value;
	else
		assert(FALSE);
		
	obj->category = kind;
}

INLINE Error CopyScalarToObject(BObject *obj, const Scalar *src)
{
	CopyDereferencingSource(&obj->value.scalar, src);
	obj->category = LITERAL;
	return SUCCESS;
}

/* Split into a separate function because it's a minority case, to keep DereferenceObject
small - when variables are looked up, a pointer object is created rather than an embedded
variable. */
static Error DereferenceScalarVariable(BObject *obj)
{
	/* Need to save the scalar out of the embedded variable before copying it into the same object. */
	Scalar val = obj->value.variable.value;
	
	assert(IsVariable(obj));
	assert(!(obj->category & VARIABLE_IS_ARRAY));
	
	return CopyScalarToObject(obj, &val);
}

/* Yields, where possible, a scalar value from an object.
	Returns an error if this is not a meaningful operation for the type of object:
it must be a non-array variable, if not already a literal. Parameterless functions
are NOT called. An existing error is propagated. */
Error DereferenceObject(BObject *obj)
{
	assert(obj != NULL);
	
	if(obj->category == LITERAL) /* should be the most common case - the result of an expression */
		return !ObjectIsError(obj) ? SUCCESS : ObjectAsError(obj);
	else if((obj->category & VARIABLE_IS_ARRAY) && VarPtr(obj)->dim.few[0] != -1) /* insufficiently subscripted array */
		return SCALAREXPECTED;
	else if(obj->category & VARIABLE_IS_POINTER)
		/* obj does not need to be deleted, because it's just a variable reference. */
		return CopyScalarToObject(obj, &obj->value.varRef->value);
	else if(IsVariable(obj))
		return DereferenceScalarVariable(obj);
	else
		return SCALAREXPECTED; /* assume an unevaluated function, or label, subprogram, etc. */
}

#ifdef DEBUG

short GetCallNestLevel(const BObject *obj)
{
	/* TODO */
	return Proc()->callNestLevel;
}

void DumpObject(const BObject *obj)
{
	const char *location = NULL, *file = NULL;
	int line = -1;
	
	if(obj->category == LABEL)
		location = obj->value.labelPos;
	else if(obj->category == FUNCTION && IsDefFunction(obj->value.function))
		location = obj->value.function->def->defStart;
	else if(obj->category == STATEMENT && IsSubprogram(obj->value.statement))
		location = obj->value.statement->method.sub;
	
	if(location != NULL)
		GetLocationInfo(Proc()->buffer, location, &line, &file);

	switch(obj->category) {
		case LABEL:			
			fprintf(stderr, "<label: file %s, line %d>", file, line);
			break;
		case OPERATOR:
			fprintf(stderr, "<op: %p>", (void *)obj->value.opRef);
			break;
		case LITERAL:
			fprintf(stderr, "<lit(%d): ", obj->value.scalar.type);
			WriteScalar(stderr, &obj->value.scalar);
			fputc('>', stderr);
			break;
		case PUNCTUATION:
			fprintf(stderr, "<punct: %c>", obj->value.punctuation->token);
			break;
		case FUNCTION:
			if(IsDefFunction(obj->value.function))
				fprintf(stderr, "<def func: file %s, line %d>", file, line);
			else
				fprintf(stderr, "<func: %p>", (void *)obj->value.function);
			break;
		case STATEMENT:
			if(IsSubprogram(obj->value.statement))
				fprintf(stderr, "<sub: file %s, line %d>", file, line);
			else
				fprintf(stderr, "<cmd: %p>", (void *)obj->value.statement->method.builtIn);
			break;
		default:
			if(obj->category & IS_VARIABLE)
				fprintf(stderr, "<var(%d) cat=%d @ %d: %p>",
					VarData(obj)->type,
					obj->category,
					GetCallNestLevel(obj),
					VarPtr(obj));
			else
				fprintf(stderr, "<UNKNOWN CATEGORY!>");
			break;
	}
}

#endif /* DEBUG */
