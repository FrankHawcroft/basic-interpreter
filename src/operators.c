/****** operators.c ******/

/*
	$VER: operators.c 0.16A (5.18.2015)
*/

#include <limits.h>
#include <string.h>
#include <math.h>
#include <float.h>
#include "interpreter.h"
#include "sign.h"

/* The AmigaBASIC manual describes functions as working in single precision when the parameters are s.p.,
	double-precision if they're d.p. In C, mathematical functions tend to operate in d.p., so we usually
	convert to d.p., then back. We may however want more tolerant behaviour, based on the magnitude of the
	result. */
#define AMIGABASIC_COMPATIBLE_FP_PROMOTION TRUE

static void Exponentiation_(Scalar *, const Scalar *, const Scalar *);
static void UnaryPlus_(Scalar *, const Scalar *);
static void UnaryNegation_(Scalar *, const Scalar *);
static void Multiplication_(Scalar *, const Scalar *, const Scalar *);
static void Division_(Scalar *, const Scalar *, const Scalar *);
static void Addition_(Scalar *, const Scalar *, const Scalar *);
static void Subtraction_(Scalar *, const Scalar *, const Scalar *);
static void LogicalConjunction_(Scalar *, const Scalar *, const Scalar *);
static void LogicalDisjunction_(Scalar *, const Scalar *, const Scalar *);
static void LogicalExclusiveDisjunction_(Scalar *, const Scalar *, const Scalar *);
static void LogicalMaterialImplication_(Scalar *, const Scalar *, const Scalar *);
static void LogicalEquivalence_(Scalar *, const Scalar *, const Scalar *);
static void LogicalNegation_(Scalar *, const Scalar *);
static void BitwiseOR_(Scalar *, const Scalar *, const Scalar *);
static void BitwiseAND_(Scalar *, const Scalar *, const Scalar *);
static void BitwiseXOR_(Scalar *, const Scalar *, const Scalar *);
static void BitwiseNOT_(Scalar *, const Scalar *);
static void Concatenation_(Scalar *, const Scalar *, const Scalar *);
static void CharSetMembership_(Scalar *, const Scalar *, const Scalar *);
static void Modulo_(Scalar *, const Scalar *, const Scalar *);
static void WholeDivision_(Scalar *, const Scalar *, const Scalar *);
static void GreaterOrEqual_(Scalar *, const Scalar *, const Scalar *);
static void Equal_(Scalar *, const Scalar *, const Scalar *);
static void LessOrEqual_(Scalar *, const Scalar *, const Scalar *);
static void Greater_(Scalar *, const Scalar *, const Scalar *);
static void Less_(Scalar *, const Scalar *, const Scalar *);
static void NotEqual_(Scalar *, const Scalar *, const Scalar *);

enum NumOperands {
	UNARY = 1,
	BINARY = 2
};

enum Associativity {
	LEFTTORIGHT,
	RIGHTTOLEFT,
	NONASSOC
};

enum Priority {
	LOGICALBINARY,			/* AND, OR, XOR, IMP, EQV */
	LOGICALNEGATION,		/* NOT */
	EQUALITY,				/* =, <> */
	ORDERING,				/* >=, >, <=, < */
	ADDITION,				/* +, - (binary), &, IN */
	MULTIPLICATION,			/* *, /, MOD, \ */
	EXPONENTIATION,			/* ^ */
	BITOR,					/* BITOR */
	BITXOR,					/* BITXOR */
	BITAND,					/* BITAND */
	UNARYARITHMETIC			/* +, - (unary), BITNOT */
};

union OperatorMethod {
	void (*binaryMethod)(Scalar *, const Scalar *, const Scalar *);
	void (*unaryMethod)(Scalar *, const Scalar *);
};

struct Operator {
	enum NumOperands numOperands;
	enum Associativity associativity;
	enum Priority relativePriority;
	struct Parameter operandTypes[2]; /* Second element is TR_IRRELEVANT for unary operators. */
	unsigned range; /* Not currently used ... */
	union OperatorMethod method;
};

struct OperatorDefinition {
	const char *name;
	struct Operator body;
};

/* Initialisation helper macros - BOPD and UOPD initialise Parameter structures for Binary and Unary operators respectively. */
#define BOPD(t1, t2) {{LITERAL, t1, NULL, NO_NAME, 1, FALSE}, {LITERAL, t2, NULL, NO_NAME, 1, FALSE}}
#define UOPD(t1) {{LITERAL, t1, NULL, NO_NAME, 1, FALSE}, {LITERAL, TR_IRRELEVANT, NULL, NO_NAME, 0, FALSE}}
#define OPIMPL(f) {(void (*)(Scalar *, const Scalar *, const Scalar *))f}

static const struct OperatorDefinition m_OperatorDefinition[] = {
	{"^", {BINARY, RIGHTTOLEFT, EXPONENTIATION, BOPD(TR_NUMERIC, TR_NUMERIC), T_SINGLE | T_DOUBLE, OPIMPL(Exponentiation_)}},
	{KW_UNAMBIGUOUS_UNARY_MINUS, {UNARY, RIGHTTOLEFT, UNARYARITHMETIC, UOPD(TR_NUMERIC), NUMERIC_TYPES, OPIMPL(UnaryNegation_)}},
		/* Note artificial name to avoid overloading. */
	{KW_MINUS, {BINARY, LEFTTORIGHT, ADDITION, BOPD(TR_NUMERIC, TR_NUMERIC), NUMERIC_TYPES, OPIMPL(Subtraction_)}},
	{KW_UNAMBIGUOUS_UNARY_PLUS, {UNARY, RIGHTTOLEFT, UNARYARITHMETIC, UOPD(TR_NUMERIC), NUMERIC_TYPES, OPIMPL(UnaryPlus_)}},
		/* Note artificial name to avoid overloading. */
	{KW_PLUS, {BINARY, LEFTTORIGHT, ADDITION, BOPD(TR_ANY, TR_ANY), NUMERIC_TYPES | T_STRING, OPIMPL(Addition_)}},
	{"&", {BINARY, LEFTTORIGHT, ADDITION, BOPD(TR_ANY_TO_STRING, TR_ANY_TO_STRING), T_STRING, OPIMPL(Concatenation_)}},
	{"*", {BINARY, LEFTTORIGHT, MULTIPLICATION, BOPD(TR_NUMERIC, TR_NUMERIC), NUMERIC_TYPES, OPIMPL(Multiplication_)}},
	{"/", {BINARY, LEFTTORIGHT, MULTIPLICATION, BOPD(TR_FLOATING, TR_FLOATING), T_SINGLE | T_DOUBLE, OPIMPL(Division_)}},
	{"AND", {BINARY, LEFTTORIGHT, LOGICALBINARY, BOPD(TR_LOGICAL, TR_LOGICAL), T_BOOL, OPIMPL(LogicalConjunction_)}},
	{"OR", {BINARY, LEFTTORIGHT, LOGICALBINARY, BOPD(TR_LOGICAL, TR_LOGICAL), T_BOOL, OPIMPL(LogicalDisjunction_)}},
	{"XOR", {BINARY, LEFTTORIGHT, LOGICALBINARY, BOPD(TR_LOGICAL, TR_LOGICAL), T_BOOL, OPIMPL(LogicalExclusiveDisjunction_)}},
	{"IMP", {BINARY, LEFTTORIGHT, LOGICALBINARY, BOPD(TR_LOGICAL, TR_LOGICAL), T_BOOL, OPIMPL(LogicalMaterialImplication_)}},
	{"EQV", {BINARY, LEFTTORIGHT, LOGICALBINARY, BOPD(TR_LOGICAL, TR_LOGICAL), T_BOOL, OPIMPL(LogicalEquivalence_)}},
	{"NOT", {UNARY, RIGHTTOLEFT, LOGICALNEGATION, UOPD(TR_LOGICAL), T_BOOL, OPIMPL(LogicalNegation_)}},
	{"BITOR", {BINARY, LEFTTORIGHT, BITOR, BOPD(TR_INTEGRAL, TR_INTEGRAL), T_INT | T_LONG, OPIMPL(BitwiseOR_)}},
	{"BITAND", {BINARY, LEFTTORIGHT, BITAND, BOPD(TR_INTEGRAL, TR_INTEGRAL), T_INT | T_LONG, OPIMPL(BitwiseAND_)}},
	{"BITXOR", {BINARY, LEFTTORIGHT, BITXOR, BOPD(TR_INTEGRAL, TR_INTEGRAL), T_INT | T_LONG, OPIMPL(BitwiseXOR_)}},
	{"BITNOT", {UNARY, RIGHTTOLEFT, UNARYARITHMETIC, UOPD(TR_INTEGRAL), T_INT | T_LONG, OPIMPL(BitwiseNOT_)}},
	{"IN", {BINARY, LEFTTORIGHT, ADDITION, BOPD(TR_STRING_TO_CHAR, TR_STRING_ONLY), T_BOOL, OPIMPL(CharSetMembership_)}},
	{"MOD", {BINARY, LEFTTORIGHT, MULTIPLICATION, BOPD(TR_INT_OR_LONG, TR_INT_OR_LONG), T_INT | T_LONG, OPIMPL(Modulo_)}},
	{"\\", {BINARY, LEFTTORIGHT, MULTIPLICATION, BOPD(TR_INT_OR_LONG, TR_INT_OR_LONG), T_INT | T_LONG, OPIMPL(WholeDivision_)}},
	{">=", {BINARY, LEFTTORIGHT, ORDERING, BOPD(TR_ANY, TR_ANY), T_BOOL, OPIMPL(GreaterOrEqual_)}},
	{KW_EQUALS, {BINARY, LEFTTORIGHT, EQUALITY, BOPD(TR_ANY, TR_ANY), T_BOOL, OPIMPL(Equal_)}},
	{"<=", {BINARY, LEFTTORIGHT, ORDERING, BOPD(TR_ANY, TR_ANY), T_BOOL, OPIMPL(LessOrEqual_)}},
	{">", {BINARY, LEFTTORIGHT, ORDERING, BOPD(TR_ANY, TR_ANY), T_BOOL, OPIMPL(Greater_)}},
	{"<", {BINARY, LEFTTORIGHT, ORDERING, BOPD(TR_ANY, TR_ANY), T_BOOL, OPIMPL(Less_)}},
	{"<>", {BINARY, LEFTTORIGHT, EQUALITY, BOPD(TR_ANY, TR_ANY), T_BOOL, OPIMPL(NotEqual_)}},
	{NULL, {UNARY, LEFTTORIGHT, LOGICALBINARY, BOPD(TR_ANY, TR_ANY), 0, {NULL}}} /* sentinel */
};

/* Returns as for strcmp(), based on the relative priorities of the operators. */
int ComparePriority(const struct Operator *left, const struct Operator *right)
{
	if(left == NULL)
		return right == NULL ? 0 : -1;
	else if(right == NULL)
		return 1;
	else if(left->relativePriority != right->relativePriority)
		return (int)left->relativePriority - (int)right->relativePriority;
	else if(left->associativity == right->associativity && left->associativity != NONASSOC)
		return left->associativity == LEFTTORIGHT ? 1 : -1;
	else
		return 0;
}

unsigned OperandCount(const struct Operator *op)
{
	assert(op != NULL);
	assert(op->numOperands == UNARY || op->numOperands == BINARY);

	return (unsigned)op->numOperands;
}

unsigned Range(const struct Operator *op)
{
	assert(op != NULL);

	return op->range;
}

const struct Parameter *ParametersForOperator(const struct Operator *op)
{
	assert(op != NULL);

	return op->operandTypes;
}

/* Evaluate a unary or binary operation.
	Type conversion and error checking is assumed to have been done already - see Conform and ConformQuickly.
	All operand methods follow the convention that 'result' is an uninitialised out parameter.
	If evaluation fails, an error will be left in 'result'. */
void EvalOperation(Scalar *result, const struct Operator *operation, const Scalar *a, const Scalar *b)
{
	assert(operation != NULL);
	assert(result != NULL);
	assert(a != NULL);
	assert(operation->numOperands == UNARY || operation->numOperands == BINARY); /* Paranoia. */
	assert((operation->numOperands == UNARY) == (b == NULL));
	assert(!ScalarIsError(a) && (b == NULL || !ScalarIsError(b)));

	if(operation->numOperands == BINARY)
		(*operation->method.binaryMethod)(result, a, b);
	else
		(*operation->method.unaryMethod)(result, a);
}

void DefineOperators(void)
{
	const struct OperatorDefinition *currentOp;
	for(currentOp = &m_OperatorDefinition[0]; currentOp->name != NULL; currentOp++) {
		QString nameAsStr;
		QsInitStatic(&nameAsStr, currentOp->name, strlen(currentOp->name));
		RequireSuccess(DefineSymbol(&nameAsStr, (void *)&currentOp->body, OPERATOR, SCOPE_BUILTIN));
	}
}

bool AttemptToDefineOperator(const QString *candidate)
{
	const struct OperatorDefinition *currentOp;
	for(currentOp = &m_OperatorDefinition[0]; currentOp->name != NULL; currentOp++) {
		QString nameAsStr;
		QsInitStatic(&nameAsStr, currentOp->name, strlen(currentOp->name));
		if(QsEqNoCase(candidate, &nameAsStr)) {
			DefineSymbol(&nameAsStr, (void *)&currentOp->body, OPERATOR, SCOPE_BUILTIN);
			return TRUE;
		}
	}
	return FALSE;
}

const struct Operator *ResolveOperator(const QString *token)
{
	EnsureExistsIfBuiltIn(token);
	
	{
		const BObject *defn = IsLiteral(token) || IsSeparator(token) ? NULL : LookUp(token, SCOPE_BUILTIN);
		return defn != NULL && defn->category == OPERATOR ? defn->value.opRef : NULL;
	}
}

/* Assume that if the result can't be stored as an int or long, will then attempt floating point. */
INLINE void SetIntResult(Scalar *result, long val, bool overflow, SimpleType t1, SimpleType t2)
{
	if(!overflow)
		SetFromLong(result, val, 
			NonPointer(t1) == T_INT && NonPointer(t2) == T_INT && val >= SHRT_MIN && val <= SHRT_MAX ? T_INT : T_LONG);
}

/* Floating point overflow detection is avoided on the Amiga because a bit slow. */
#if defined(FP_NORMAL) && !defined(VBCC) && !defined(AMIGA)
#define DetectFloatOverflow(val) (fpclassify(val) != FP_NORMAL && fpclassify(val) != FP_ZERO)
#else
#define DetectFloatOverflow(val) FALSE
#endif

#if AMIGABASIC_COMPATIBLE_FP_PROMOTION
#define ResultTypeIsDouble(t1, t2, val) (NonPointer(t1) == T_DOUBLE || NonPointer(t2) == T_DOUBLE)
#else
#define ResultTypeIsDouble(t1, t2, val) (NonPointer(t1) == T_DOUBLE || NonPointer(t2) == T_DOUBLE || fabs(val) > FLT_MAX)
#endif
 
INLINE void SetFPResult(Scalar *result, double val, SimpleType t1, SimpleType t2)
{
#if defined(FP_NORMAL) && !defined(VBCC)
	int classification;
	
	if((classification = fpclassify(val)) != FP_NORMAL && classification != FP_ZERO)
		SetError(result, OVERFLOWERR);
	else
#endif
		SetFromDouble(result, val, ResultTypeIsDouble(t1, t2, val) ? T_DOUBLE : T_SINGLE);
}

static void Exponentiation_(Scalar *result, const Scalar *a, const Scalar *b)
{
	const double MaxAllowedExponent = SHRT_MAX, MinAllowedExponent = (double)SHRT_MIN + 1;
	double base = GetDouble(a), exponent = GetDouble(b);

	if(exponent > MaxAllowedExponent || exponent < MinAllowedExponent)
		SetError(result, OVERFLOWERR);
	else if((base == 0.0 && exponent <= 0.0) || (base < 0.0 && floor(exponent) != exponent))
		SetError(result, BADEXPONENT);
	else
		SetFPResult(result, pow(base, exponent), a->type, b->type);
}

static void UnaryPlus_(Scalar *result, const Scalar *a)
{
	CopyScalar(result, a);
}

static void UnaryNegation_(Scalar *result, const Scalar *a)
{
	long la;
	if(TypeIsExact(a->type) && (la = GetLong(a)) != LONG_MIN)
		SetIntResult(result, -la, FALSE, a->type, a->type);
	else if(a->type == T_SINGLE) {
		result->type = T_SINGLE;
		result->value.number.f = -a->value.number.f;
	}
	else
		SetFPResult(result, -GetDouble(a), a->type, a->type);
}

/* No special cases for single precision floats here because the GCC M68K code generator or Amiga runtime has a bug in
	float multiplication and division when compiling with -Os */
static void Multiplication_(Scalar *result, const Scalar *a, const Scalar *b)
{
	bool overflow = TRUE; /* assume f.p. unless both integral */
	
	if(TypeIsExact(a->type) && TypeIsExact(b->type)) {
		long la = GetLong(a), lb = GetLong(b); 
		long product = la * lb;
		overflow = Sign(la) * Sign(lb) != Sign(product);
		SetIntResult(result, product, overflow, a->type, b->type);	
	}

	if(overflow)
		SetFPResult(result, GetDouble(a) * GetDouble(b), a->type, b->type);
}

static void Division_(Scalar *result, const Scalar *a, const Scalar *b)
{
	if(GetDouble(b) == 0.0 && TypeIsNumeric(NonPointer(b->type)))
		SetError(result, ZERODIVISOR);
	else
		SetFPResult(result, GetDouble(a) / GetDouble(b), a->type, b->type);
}

static void Addition_(Scalar *result, const Scalar *a, const Scalar *b)
{
	if(TypeIsTextual(NonPointer(a->type)) || TypeIsTextual(NonPointer(b->type)))
		Concatenation_(result, a, b);
	else {
		bool overflow = TRUE; /* assume f.p. unless both integral */

		if(TypeIsExact(a->type) && TypeIsExact(b->type)) {
			long la = GetLong(a), lb = GetLong(b);
			long sum = la + lb;
			overflow = (lb > 0 && sum < la) || (lb < 0 && sum > la);
			SetIntResult(result, sum, overflow, a->type, b->type);	
		}
		else if(a->type == T_SINGLE && b->type == T_SINGLE) {
			result->type = T_SINGLE;
			result->value.number.f = a->value.number.f + b->value.number.f;
			overflow = DetectFloatOverflow(result->value.number.f);
		}

		if(overflow)
			SetFPResult(result, GetDouble(a) + GetDouble(b), a->type, b->type);
	}
}

static void Subtraction_(Scalar *result, const Scalar *a, const Scalar *b)
{
	bool overflow = TRUE; /* assume f.p. unless both integral */

	if(TypeIsExact(NonPointer(a->type)) && TypeIsExact(NonPointer(b->type))) {
		long la = GetLong(a), lb = GetLong(b);
		long diff = la - lb;
		overflow = (lb > 0 && diff > la) || (lb < 0 && diff < la);
		SetIntResult(result, diff, overflow, a->type, b->type);	
	}
	else if(a->type == T_SINGLE && b->type == T_SINGLE) {
		result->type = T_SINGLE;
		result->value.number.f = a->value.number.f - b->value.number.f;
		overflow = DetectFloatOverflow(result->value.number.f);
	}

	if(overflow)
		SetFPResult(result, GetDouble(a) - GetDouble(b), a->type, b->type);
}

static void LogicalConjunction_(Scalar *result, const Scalar *a, const Scalar *b)
{
	SetBoolean(result, GetBoolean(a) && GetBoolean(b));
}

static void LogicalDisjunction_(Scalar *result, const Scalar *a, const Scalar *b)
{
	SetBoolean(result, GetBoolean(a) || GetBoolean(b));
}

static void LogicalExclusiveDisjunction_(Scalar *result, const Scalar *a, const Scalar *b)
{
	SetBoolean(result, GetBoolean(a) ^ GetBoolean(b));
}

static void LogicalMaterialImplication_(Scalar *result, const Scalar *a, const Scalar *b)
{
	SetBoolean(result, !GetBoolean(a) || GetBoolean(b));
}

static void LogicalEquivalence_(Scalar *result, const Scalar *a, const Scalar *b)
{
	SetBoolean(result, GetBoolean(a) == GetBoolean(b));
}

static void LogicalNegation_(Scalar *result, const Scalar *a)
{
	SetBoolean(result, !GetBoolean(a));
}

/* Set the value to the long integer provided; the type of the result is the
'largest' of the two types provided. The magnitude of the value is not
considered. If it is too large to fit in the result type, it will be 
truncated, rather than an error being set. */
INLINE void SetTruncated(Scalar *dest, long v, SimpleType t1, SimpleType t2)
{
	assert(TypeIsNumeric(t1) && TypeIsNumeric(t2) && TypeIsExact(t1) && TypeIsExact(t2));

	if(t1 == T_LONG || t2 == T_LONG)
		SetFromLong(dest, v, T_LONG);
	else
		SetFromShort(dest, (short)v, T_INT);
}

static void BitwiseOR_(Scalar *result, const Scalar *a, const Scalar *b)
{
	SetTruncated(result, GetLong(a) | GetLong(b), NonPointer(a->type), NonPointer(b->type));
}

static void BitwiseAND_(Scalar *result, const Scalar *a, const Scalar *b)
{
	SetTruncated(result, GetLong(a) & GetLong(b), NonPointer(a->type), NonPointer(b->type));
}

static void BitwiseXOR_(Scalar *result, const Scalar *a, const Scalar *b)
{
	SetTruncated(result, GetLong(a) ^ GetLong(b), NonPointer(a->type), NonPointer(b->type));
}

static void BitwiseNOT_(Scalar *result, const Scalar *a)
{
	SetTruncated(result, ~GetLong(a), NonPointer(a->type), NonPointer(a->type));
}

static void Concatenation_(Scalar *result, const Scalar *a, const Scalar *b)
{
	Error error = BADARGTYPE;
	
	InitScalarAsString(result);
	if(NonPointer(a->type) == T_STRING && NonPointer(b->type) == T_STRING)
		error = QsJoin(&result->value.string,
					(const QString *)GetPointer((Scalar *)a), (const QString *)GetPointer((Scalar *)b))
			? SUCCESS : NOMEMORY;
	else {
		QString sa, sb;
		QsInitNull(&sa); QsInitNull(&sb);
		if((error = ScalarToString(a, &sa)) == SUCCESS && (error = ScalarToString(b, &sb)) == SUCCESS)
			error = QsJoin(&result->value.string, &sa, &sb) ? SUCCESS : NOMEMORY;
		QsDispose(&sa); QsDispose(&sb);
	}
	
	if(error != SUCCESS)
		SetError(result, error);
}

static void CharSetMembership_(Scalar *result, const Scalar *a, const Scalar *b)
{
	const QString *s = NonPointer(a->type) == T_STRING ? (const QString *)GetPointer((Scalar *)a) : NULL;
	if(s != NULL && QsGetLength(s) != 1)
		SetError(result, OUTSIDEDOMAIN);
	else {
		QsChar ch = s != NULL ? QsGetFirst(s) : GetCharacter(a);
		SetBoolean(result, QsSearchForChar((const QString *)GetPointer((Scalar *)b), ch) != NULL);
	}
}

static void Modulo_(Scalar *result, const Scalar *a, const Scalar *b)
{
	long modulus = GetLong(b);
	if(modulus == 0 && TypeIsNumeric(NonPointer(b->type)))
		SetError(result, ZERODIVISOR);
	else if(a->type == T_INT && b->type == T_INT)
		SetFromShort(result, a->value.number.s % b->value.number.s, T_INT);
	else
		SetFromLong(result, GetLong(a) % modulus, T_LONG);
}

static void WholeDivision_(Scalar *result, const Scalar *a, const Scalar *b)
{
	long divisor = GetLong(b);
	if(divisor == 0 && TypeIsNumeric(NonPointer(b->type)))
		SetError(result, ZERODIVISOR);
	else if(a->type == T_INT && b->type == T_INT)
		SetFromShort(result, a->value.number.s / b->value.number.s, T_INT);
	else
		SetFromLong(result, GetLong(a) / divisor, T_LONG);
}

INLINE bool SetComparisonOrError(Scalar *result, const Scalar *a, const Scalar *b)
{
	Error error = SUCCESS;
	int comparison = Compare(a, b, &error);
	if(error == SUCCESS)
		SetFromLong(result, comparison, T_LONG);
	else
		SetError(result, error);
	return error == SUCCESS;
}

static void GreaterOrEqual_(Scalar *result, const Scalar *a, const Scalar *b)
{
	if(SetComparisonOrError(result, a, b)) SetBoolean(result, GetLong(result) >= 0);
}

static void Equal_(Scalar *result, const Scalar *a, const Scalar *b)
{
	if(SetComparisonOrError(result, a, b)) SetBoolean(result, GetLong(result) == 0);
}

static void LessOrEqual_(Scalar *result, const Scalar *a, const Scalar *b)
{
	if(SetComparisonOrError(result, a, b)) SetBoolean(result, GetLong(result) <= 0);
}

static void Greater_(Scalar *result, const Scalar *a, const Scalar *b)
{
	if(SetComparisonOrError(result, a, b)) SetBoolean(result, GetLong(result) > 0);
}

static void Less_(Scalar *result, const Scalar *a, const Scalar *b)
{
	if(SetComparisonOrError(result, a, b)) SetBoolean(result, GetLong(result) < 0);
}

static void NotEqual_(Scalar *result, const Scalar *a, const Scalar *b)
{
	if(SetComparisonOrError(result, a, b)) SetBoolean(result, GetLong(result) != 0);
}
