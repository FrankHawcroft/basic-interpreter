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
	{"^", {BINARY, RIGHTTOLEFT, EXPONENTIATION, BOPD(TR_EXTEND_NUM, TR_EXTEND_NUM), T_SINGLE | T_DOUBLE, OPIMPL(Exponentiation_)}},
	{KW_UNAMBIGUOUS_UNARY_MINUS, {UNARY, RIGHTTOLEFT, UNARYARITHMETIC, UOPD(TR_NUMERIC), NUMERIC_TYPES, OPIMPL(UnaryNegation_)}},
		/* Note artificial name to avoid overloading. */
	{KW_MINUS, {BINARY, LEFTTORIGHT, ADDITION, BOPD(TR_EXTEND_NUM, TR_EXTEND_NUM), NUMERIC_TYPES, OPIMPL(Subtraction_)}},
	{KW_UNAMBIGUOUS_UNARY_PLUS, {UNARY, RIGHTTOLEFT, UNARYARITHMETIC, UOPD(TR_NUMERIC), NUMERIC_TYPES, OPIMPL(UnaryPlus_)}},
		/* Note artificial name to avoid overloading. */
	{KW_PLUS, {BINARY, LEFTTORIGHT, ADDITION, BOPD(TR_ANY, TR_ANY), NUMERIC_TYPES | T_STRING, OPIMPL(Addition_)}},
	{"&", {BINARY, LEFTTORIGHT, ADDITION, BOPD(TR_ANY_TO_STRING, TR_ANY_TO_STRING), T_STRING, OPIMPL(Concatenation_)}},
	{"*", {BINARY, LEFTTORIGHT, MULTIPLICATION, BOPD(TR_EXTEND_NUM, TR_EXTEND_NUM), NUMERIC_TYPES, OPIMPL(Multiplication_)}},
	{"/", {BINARY, LEFTTORIGHT, MULTIPLICATION, BOPD(TR_SINGLE_TO_DOUBLE, TR_SINGLE_TO_DOUBLE), T_SINGLE | T_DOUBLE, OPIMPL(Division_)}},
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
	{"MOD", {BINARY, LEFTTORIGHT, MULTIPLICATION, BOPD(TR_INT_TO_LONG, TR_INT_TO_LONG), T_INT | T_LONG, OPIMPL(Modulo_)}},
	{"\\", {BINARY, LEFTTORIGHT, MULTIPLICATION, BOPD(TR_INT_TO_LONG, TR_INT_TO_LONG), T_INT | T_LONG, OPIMPL(WholeDivision_)}},
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
	Type conversion and error checking is assumed to have been done already - see Conform.
	All operand methods follow the convention that 'result' is an out parameter and does not have
to be initialised before calling. */
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

INLINE void SetIntResult(Scalar *result, long val, bool overflow, SimpleType t1, SimpleType t2)
{
	if(overflow)
		SetError(result, OVERFLOWERR);
	else
		SetFromLong(result, val, 
			t1 == T_INT && t2 == T_INT && val >= SHRT_MIN && val <= SHRT_MAX ? T_INT : T_LONG);
}

INLINE void SetFPResult(Scalar *result, double val, SimpleType t1, SimpleType t2)
{
#if defined(FP_NORMAL) && !defined(VBCC)
	int classification;
	
	if((classification = fpclassify(val)) != FP_NORMAL && classification != FP_ZERO)
		SetError(result, OVERFLOWERR);
	else
#endif
		SetFromDouble(result, val,
			t1 == T_DOUBLE || t2 == T_DOUBLE || fabs(val) > FLT_MAX ? T_DOUBLE : T_SINGLE);
}

static void Exponentiation_(Scalar *result, const Scalar *a, const Scalar *b)
{
	double base = GetDouble(a), exponent = GetDouble(b);

	if(exponent > SHRT_MAX || exponent < SHRT_MIN + 1)
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
	if(TypeIsExact(a->type) && GetLong(a) != LONG_MIN)	
		SetIntResult(result, -GetLong(a), FALSE, a->type, a->type);
	else
		SetFPResult(result, -GetDouble(a), a->type, a->type);
}

static void Multiplication_(Scalar *result, const Scalar *a, const Scalar *b)
{
	bool intOverflow = TRUE; /* i.e. assume f.p. */
	
	if(TypeIsExact(a->type) && TypeIsExact(b->type)) {
		long product = GetLong(a) * GetLong(b);
		intOverflow = Sign(GetLong(a)) * Sign(GetLong(b)) != Sign(product);
		SetIntResult(result, product, intOverflow, a->type, b->type);	
	}
	
	if(intOverflow)
		SetFPResult(result, GetDouble(a) * GetDouble(b), a->type, b->type);
}

static void Division_(Scalar *result, const Scalar *a, const Scalar *b)
{
	if(GetDouble(b) == 0.0)
		SetError(result, ZERODIVISOR);
	else
		SetFPResult(result, GetDouble(a) / GetDouble(b), a->type, b->type);
}

static void Addition_(Scalar *result, const Scalar *a, const Scalar *b)
{
	if(TypeIsTextual(a->type) || TypeIsTextual(b->type)) {
		Scalar op1str, op2str;
		CopyScalar(&op1str, a);
		CopyScalar(&op2str, b);
		if(ChangeType(&op1str, TR_CHAR_TO_STRING) == SUCCESS && ChangeType(&op2str, TR_CHAR_TO_STRING) == SUCCESS)
			Concatenation_(result, &op1str, &op2str);
		else
			SetError(result, BADARGTYPE);
		DisposeScalar(&op1str);
		DisposeScalar(&op2str);
	}
	else {
		bool intOverflow = TRUE; /* i.e. assume f.p. */

		if(TypeIsExact(a->type) && TypeIsExact(b->type)) {
			long sum = GetLong(a) + GetLong(b);
			intOverflow = (GetLong(b) > 0 && sum < GetLong(a))
						|| (GetLong(b) < 0 && sum > GetLong(a));
			SetIntResult(result, sum, intOverflow, a->type, b->type);	
		}

		if(intOverflow)
			SetFPResult(result, GetDouble(a) + GetDouble(b), a->type, b->type);
	}
}

static void Subtraction_(Scalar *result, const Scalar *a, const Scalar *b)
{
	bool intOverflow = TRUE; /* i.e. assume f.p. */

	if(TypeIsExact(a->type) && TypeIsExact(b->type)) {
		long diff = GetLong(a) - GetLong(b);
		intOverflow = (GetLong(b) > 0 && diff > GetLong(a))
					|| (GetLong(b) < 0 && diff < GetLong(a));
		SetIntResult(result, diff, intOverflow, a->type, b->type);	
	}

	if(intOverflow)
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

static void BitwiseOR_(Scalar *result, const Scalar *a, const Scalar *b)
{
	SetTruncated(result, GetLong(a) | GetLong(b), a->type, b->type);
}

static void BitwiseAND_(Scalar *result, const Scalar *a, const Scalar *b)
{
	SetTruncated(result, GetLong(a) & GetLong(b), a->type, b->type);
}

static void BitwiseXOR_(Scalar *result, const Scalar *a, const Scalar *b)
{
	SetTruncated(result, GetLong(a) ^ GetLong(b), a->type, b->type);
}

static void BitwiseNOT_(Scalar *result, const Scalar *a)
{
	SetTruncated(result, ~GetLong(a), a->type, a->type);
}

static void Concatenation_(Scalar *result, const Scalar *a, const Scalar *b)
{
	InitScalarAsString(result);
	if(!QsJoin(&result->value.string, &a->value.string, &b->value.string))
		SetError(result, OVERFLOWERR);
}

static void CharSetMembership_(Scalar *result, const Scalar *a, const Scalar *b)
{
	SetBoolean(result, QsSearchForChar(&b->value.string, a->value.character) != NULL);
}

static void Modulo_(Scalar *result, const Scalar *a, const Scalar *b)
{
	if(GetLong(b) == 0)
		SetError(result, ZERODIVISOR);
	else
		SetIntOrLong(result, GetLong(a) % GetLong(b));
}

static void WholeDivision_(Scalar *result, const Scalar *a, const Scalar *b)
{
	if(GetLong(b) == 0)
		SetError(result, ZERODIVISOR);
	else
		SetIntOrLong(result, GetLong(a) / GetLong(b));
}

static void GreaterOrEqual_(Scalar *result, const Scalar *a, const Scalar *b)
{
	int comparison = Compare(result, a, b);
	if(!ScalarIsError(result))
		SetBoolean(result, comparison >= 0);
}

static void Equal_(Scalar *result, const Scalar *a, const Scalar *b)
{
	int comparison = Compare(result, a, b);
	if(!ScalarIsError(result))
		SetBoolean(result, comparison == 0);
}

static void LessOrEqual_(Scalar *result, const Scalar *a, const Scalar *b)
{
	int comparison = Compare(result, a, b);
	if(!ScalarIsError(result))
		SetBoolean(result, comparison <= 0);
}

static void Greater_(Scalar *result, const Scalar *a, const Scalar *b)
{
	int comparison = Compare(result, a, b);
	if(!ScalarIsError(result))
		SetBoolean(result, comparison > 0);
}

static void Less_(Scalar *result, const Scalar *a, const Scalar *b)
{
	int comparison = Compare(result, a, b);
	if(!ScalarIsError(result))
		SetBoolean(result, comparison < 0);
}

static void NotEqual_(Scalar *result, const Scalar *a, const Scalar *b)
{
	int comparison = Compare(result, a, b);
	if(!ScalarIsError(result))
		SetBoolean(result, comparison != 0);
}
