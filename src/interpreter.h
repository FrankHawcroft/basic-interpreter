/****** interpreter.h ******/

/* 	
	$VER: interpreter.h 0.16A (5.13.2015)

	Declarations for the BASIC interpreter.
*/

#ifndef BAS_INTERPRETER_H_INCLUDED
#define BAS_INTERPRETER_H_INCLUDED

#include <stdio.h> /* required for references to FILE * in prototypes, unfortunately */
#include "common.h"
#include "qstring.h"
#include "errors.h"

/*** Limits ***/

/* Maximum tokens allowed per line, not including line number, label or statement name. */
#define MAX_TOKENS 255

/* Support large arrays by using 'long' as the subscript type. Otherwise, 'short' is used. */
#define BIG_ARRAYS TRUE

/* Maximum dimensionality of an array. */
#define MAX_DIMENSIONS 4

/*** SimpleType ***/

/* The primitive data types usable by BASIC programs, plus some internal-use ones. */

typedef enum SimpleType_enum {
	T_EMPTY = 0, /* An uninitialised value. */
	T_MISSING = 1, /* An unsupplied parameter. */
	T_ERROR = 2, /* An error code. */
	T_INT = 4, /* % Short signed integer. */
	T_LONG = 8, /* & Long signed integer. */
	T_SINGLE = 16, /* ! Single-precision floating point. */
	T_DOUBLE = 32, /* # Double-precision floating point. */
	T_STRING = 64, /* $ String of characters. */
	T_CHAR = 128, /* @ A single character. */
	T_BOOL = 256, /* ? True or false. */
	T_POINTER = 512 /* Combines to indicate value is a reference. */
} SimpleType;

#define NUM_SIMPLE_TYPES 10

#define TEXTUAL_TYPES (T_STRING | T_CHAR)
#define NUMERIC_TYPES (T_INT | T_LONG | T_SINGLE | T_DOUBLE)
#define INTEGRAL_TYPES (T_INT | T_LONG | T_BOOL)
#define EXACT_TYPES (INTEGRAL_TYPES | T_ERROR)
#define USABLE_TYPES (NUMERIC_TYPES | TEXTUAL_TYPES | T_BOOL)
#define ALL_TYPES (T_MISSING | T_ERROR | USABLE_TYPES)

#define DEFAULT_IMPLIED_TYPE T_SINGLE

/*** TypeDiscipline ***/

/* Define the semantics of type checking/conversion. These values must not overlap with SimpleType. */

enum TypeDiscipline {
	TD_STRICT = 1024, /* Exact match required - suitable for reference parameters. */
	TD_PRECISE = 2048, /* Only non-lossy (integral) conversions allowed. */
	TD_CHECKED = 4096, /* Conversions allowed between textual/numeric types, with domain checking. */
	TD_LOOSE = 8192, /* Conversions allowed between textual/numeric/Boolean types, without domain checking. */
	TD_FLEXIBLE = 16384 /* Always attempt conversion. */
};

/*** TypeRule ***/

/* A type checking and/or conversion rule, defined as a level of 'discipline' and (optionally) a set
of allowed output types. If no concrete output types are specified, the rule relies on 'left context'. */

enum TypeRule {
	/* Abstract (context-dependent) rules - */
	TR_ASSIGNMENT = TD_CHECKED, /* Promote to previous - for LET etc. */
	TR_SAME = TD_STRICT, /* Type must be exactly the same as previous. */	
	TR_PROMOTE = TD_LOOSE, /* Not really 'loose'. Promote to previous using standard rules. */
	/* Concrete rules - */
	TR_IRRELEVANT = TD_STRICT | T_MISSING, /* Will always fail. */
	TR_STRING_ONLY = TD_STRICT | T_STRING,
	TR_INT_ONLY = TD_STRICT | T_INT,
	TR_LONG_ONLY = TD_STRICT | T_LONG,
	TR_SINGLE_ONLY = TD_STRICT | T_SINGLE,
	TR_DOUBLE_ONLY = TD_STRICT | T_DOUBLE,
	TR_CHAR_ONLY = TD_STRICT | T_CHAR,
	TR_BOOL_ONLY = TD_STRICT | T_BOOL,
	TR_NUMERIC = TD_STRICT | NUMERIC_TYPES,
	TR_INTEGRAL = TD_STRICT | INTEGRAL_TYPES,
	TR_NON_TEXT = TD_STRICT | NUMERIC_TYPES | T_BOOL,
	TR_INT_TO_LONG = TD_PRECISE | T_LONG,
	TR_CHAR_TO_STRING = TD_CHECKED | T_STRING,
	TR_STRING_TO_CHAR = TD_CHECKED | T_CHAR,
#if BIG_ARRAYS
	TR_SUBSCRIPT = TD_CHECKED | T_LONG,
#else
	TR_SUBSCRIPT = TD_CHECKED | T_INT,
#endif
	TR_SINGLE_TO_DOUBLE = TD_CHECKED | T_DOUBLE,
	TR_NUM_TO_SINGLE = TD_CHECKED | T_SINGLE,
	TR_EXTEND_NUM = TD_CHECKED | T_LONG | T_DOUBLE,
	TR_NUM_TO_INT = TD_CHECKED | T_INT,
	TR_NUM_TO_LONG = TD_CHECKED | T_LONG,
	TR_ANY = TD_FLEXIBLE | USABLE_TYPES,
	TR_ANY_TO_STRING = TD_FLEXIBLE | T_STRING,
	TR_LOGICAL = TD_FLEXIBLE | T_BOOL
};

/*** NumericalValue ***/

union NumericalValue {
	short	s;	/* % */
	long	l;	/* & */
	float	f;	/* ! */
	double	d;  /* # */
};

/*** Pointer ***/

/* Needed for arrays, subscripts, and subprogram reference parameters. */

union Pointer {
	short	*sp;
	long	*lp;
	float	*fp;
	double	*dp;
	QString	*tp;
	char	*cp;
	bool	*bp;
};

/*** EValue ***/

/* A value without type information: 'expression value', or 'evaluand' if you prefer. */

union EValue {
	union NumericalValue number; /* %, &, !, # */
	QString string; /* $ */
	char character; /* @ */
	bool boolean; /* ? */
	Error error;
	union Pointer pointer;
};

/*** Scalar ***/

/* A type-tagged value. */

typedef struct Scalar_struct {
	SimpleType type;
	union EValue value;
} Scalar;

/*** ArraySubscript ***/

#if BIG_ARRAYS
typedef long ArraySubscript;
#else
typedef short ArraySubscript;
#endif

/*** Dimension ***/

/* Relies on mystical pointer interpretation to tell which (if either) is valid. */

#define PACKED_DIM_COUNT 2

union Dimension {
	ArraySubscript *many; /* TODO support many dimensions */
	ArraySubscript few[PACKED_DIM_COUNT];
};

/*** Variable ***/

/* A variable or named constant. */

struct Variable {
	Scalar value;
	union Dimension dim;
};

/*** PunctuationSymbol ***/

/* Parentheses, parameter separators, and statement terminators. */

struct PunctuationSymbol {
	int nests;
	const char token; 
	bool introducesNestedExpressionSequence;
	bool terminatesExpressionSequence;
};

/*** Operator ***/

/* +, -, *, /, AND, OR, NOT, etc. etc. */

struct Operator; /* Private to operators.c */

/*** SymbolType ***/

/* The allowed kinds of symbols or objects in a BASIC program. */

#define IS_VARIABLE 8
#define VARIABLE_IS_POINTER 16
#define VARIABLE_IS_CONST 32
#define VARIABLE_IS_SHARED 64
#define VARIABLE_IS_ARRAY 128
#define VARIABLE_IS_REF 256

enum SymbolType {
	LITERAL,		/* Constant appearing in program code - a number or quoted string. */
	STATEMENT,		/* SUB or built-in statement (aka command). */
	OPERATOR,		/* Operator. */
	LABEL,			/* Line number or label. */
	FUNCTION,		/* Function. */
	PUNCTUATION,	/* Argument separator. */
	SCALAR_VAR = IS_VARIABLE,
	NAMED_CONST = IS_VARIABLE | VARIABLE_IS_CONST,
	ARRAY = IS_VARIABLE | VARIABLE_IS_ARRAY,
	SHARED_VAR = IS_VARIABLE | VARIABLE_IS_SHARED,
	SHARED_ARRAY = IS_VARIABLE | VARIABLE_IS_ARRAY | VARIABLE_IS_SHARED
};

/*** BObject ***/

/* A 'BASIC object'. Extends 'Scalar' to encompass all the types the interpreter deals with. */

typedef struct BObject_struct {
	enum SymbolType category;
	union {
		Scalar scalar;
		struct Variable variable;
		struct Variable *varRef;		
		struct Statement *statement;
		struct Function *function;
		const char *labelPos;
		const struct Operator *opRef;
		const struct PunctuationSymbol *punctuation;
	} value;
} BObject;

/*** Interner ***/

/* Converts from a token to a BObject. */

typedef void (*Interner)(unsigned, const QString *, BObject *);

/*** Parameter ***/

/* Defines a formal parameter for a statement, function, or subprogram. */

struct Parameter {
	enum SymbolType kind; /* Restricted to variables, labels, and literals (expressions) -
							following tradition; though no particular reason for this limited allowance. */
	enum TypeRule type;	/* Type expected - TR_IRRELEVANT if label. */
	const Scalar *defaultValue; /* For optional parameters. NULL if mandatory or no concept of default. */
	QString name; /* Name, for a sub or user-defined function parameter; or NO_NAME for built-ins. */
	unsigned short maxCount; /* Match up to this many parameters. > 1 not allowed if named. */
	bool explicitlyTyped;
};

#define NO_NAME QS_NULL /* Parameters for built in statements and functions don't have defined names. */
#define IsVarParam(f) ((f)->kind >= SCALAR_VAR)

/*** Statement ***/

/* A statement or subprogram. */

struct HashTable;

struct Statement {
	union {
		const char *sub;
		void (*builtIn)(BObject *arg, unsigned count);
		void (*macro)(const QString *token, unsigned nToken);
	} method; /* The start of the subprogram's body, or the implementation of the command. */
	Interner convert; /* Token --> BObject conversion. */
	bool (*inactive)(bool); /* Control flow management for block statements. Called when in non-taken branch. */
	struct Parameter *formal;
	short formalCount;
	bool userDefined; /* Subprogram? */
	bool staticSub;
	struct HashTable *localStatics;
	struct Variable **predefinedParameter;
};

/* Special formalCount, meaning the statement is a macro and expects its arguments to be passed as tokens
(QStrings), without being converted into internal "binary" form - */	   
#define TOKENISED_ARGUMENTS -1

/*** CompiledExpr ***/

union CompiledExpr {
	const QString *s;
	const BObject *obj;
};

/*** Piece ***/

/* A piece is a condition (guard) and an associated value-generating expression for a function. */

struct Piece {
	struct Piece *next;
	union CompiledExpr condition; /* If NULL, always execute this one. */
	union CompiledExpr value; /* Terminated by an end-of-statement delimiter. */
	short condExprLength, valExprLength;
	
	/* Location of the piece in code - this is used for error reporting when the piece is compiled,
		for profiling, and when printing debug info about the function. */

	const char *defStart;
	const char *defFinish;
	
	SmallBitField compiled : 1; /* True if the expressions have been converted. */
	SmallBitField tailCall : 1; /* True if this case consists of a simple self-invocation of the function. */
};

/*** Function ***/

/* TODO think about unifying with Operator and possibly also Statement 
	- is there any value in representing three, not terribly different, kinds of procedure? */

struct Function	{
	void (*method)(Scalar *result, const BObject *args, unsigned argCount);	/* For built-in functions. */
	enum TypeRule type; /* Result type required. */
	struct Piece *def; /* List of conditions and bodies. Only used if a DEF. */
	struct Parameter *parameter; /* NULL if numArgs is 0 or FN_VAR_ARGS. */
	short numArgs;
	unsigned short workSpaceSize; /* Maximum of lengths of condition and value expressions in all pieces. */
	struct Variable **predefinedParameter;
	bool staticFunction;
};

/* Special numArgs value, meaning argument count/type can vary from call to call. */
#define FN_VAR_ARGS -1
	
/*** SpecialScope ***/

/* Special scope values that may apply to definitions in the symbol table. */

enum SpecialScope {
	SCOPE_NONEXISTENT = -4,
	SCOPE_BUILTIN = -3, /* built-ins: commands, functions, operators, punctuation */
	SCOPE_GLOBAL = -2, /* globals: main program constants, SHARED arrays, DEF functions, SUBs */
	SCOPE_STATIC = -1,
	SCOPE_MAIN = 0, /* main program, but not implicitly globally visible */
	SCOPE_CURRENT = 9999 /* pseudo: behaviour for looking up things which may be locals hiding globals or built-ins */
};

/*** TokenSequence ***/

/* The tokens that make up a statement. Produced by the lexer, and further processed by other steps. */

struct TokenSequence {
	const char *start; /* Position of statement in buffer. */
	const char *next;

	QString lineNumber; /* Optional old-style line number. */
	QString label; /* Optional label. */
	QString statementName; /* Optional statement. Populated with the default (LET) if empty - see sugar.c. */
	QString *rest; /* All other tokens in the statement. Conventionally, terminated by a '|'. */
	unsigned short length; /* Number of tokens in 'rest'. */
	unsigned short capacity; /* Allocated tokens. */

	unsigned ops;
	const struct Statement *command;
	BObject *preconverted; /* Tokens converted to objects. */	
};

/*** Stack ***/

/* Used for the interpreter's two kinds of stack - the (singleton) control
flow stack, and the (potentially numerous) expression evaluation stacks. */
struct Stack;

/*** Process ***/

/* Sometimes passed in, for efficiency - defined elsewhere (process.h, natch) */

struct Process;

/*** Characteristics of types -- semantics.c ***/

/* Yields a non-reference type. */
#define NonPointer(t) ((SimpleType)((t) & ~T_POINTER))

/* Set a default type for objects with names beginning with the given letter (A-Z). */
extern void SetDefaultType(char initialLetter, SimpleType);

/* Get the type applying to the given name, allowing for DEFtype rules as well as type specifiers. */
extern SimpleType TypeForName(const struct Process *, const QString *forObjectName);

/* Maps '$' to T_STRING, and so on. */
extern SimpleType TypeFromSpecifier(char);

/* Whether the given character is a valid type specifier. */
extern bool IsTypeSpecifier(char);

/* E.g. '$' for string, '%' for integer, etc. NUL for internal-only types. */
extern char SpecifierFromType(SimpleType);

/* Size of an unadorned value of type in memory. */
extern size_t StorageSize(SimpleType);

/* Immediately 'larger' type in terms of its domain size. */
extern SimpleType NextMoreCapaciousType(SimpleType);

/* Larger of the two types in terms of domain (pseudo-)size. Doesn't imply that conversion between the types is legal! */
extern SimpleType LargerType(SimpleType, SimpleType);

/* I.e. conversion of a value may be possible. */
extern bool ComparableTypes(SimpleType, SimpleType);

/* Only accessible to the interpreter; values can't be (willingly!) instantiated in programs. */
#define TypeIsInternal(t) ((t) < T_INT)

/* Can be input and/or displayed to the user. */
extern bool TypeIsVisible(SimpleType);

/* Some subset of the real numbers. */
extern bool TypeIsNumeric(SimpleType);

/* Meaningful to compare values for equality. */
extern bool TypeIsComparable(SimpleType);

/* I.e. to define less than, >= ... */
extern bool TypeIsOrderable(SimpleType);

/* Comparison is reliable; i.e., is not a floating-point representation. */
#define TypeIsExact(t) (((t) & EXACT_TYPES) != 0)

/* Values consist of character(s). */
#define TypeIsTextual(t) (((t) & TEXTUAL_TYPES) != 0)

/* The type is considered the global default, regardless of SetDefaultType. */
#define TypeIsDefault(t) ((t) == DEFAULT_IMPLIED_TYPE)

/*** Rules about type conversion -- semantics.c ***/

/* True if the conversion (TypeRule) needs (left) context to take place. */
#define Contextual(tr) (((tr) & ALL_TYPES) == 0)

/* 'Realises' a contextual conversion. */
extern enum TypeRule ConcreteConversionFor(SimpleType actual, SimpleType typeOfPrev, enum TypeRule required);

/* What could be considered the conventional or most common type conversion which produces the given type. */
extern enum TypeRule UsualTypeConversionToProduce(SimpleType);

/* A strict conversion, i.e. one that will only match the given type. */
extern enum TypeRule StrictTypeConversionFor(SimpleType);

/* The 'inverse' of UsualTypeConversionToProduce. In cases where the type conversion relies on context,
generally the default type is returned. Caveat caller. */
extern SimpleType TypeUsuallyProducedBy(enum TypeRule);

/* Given a non-contextual conversion and a source type, returns the type which should be produced,
assuming no overflow etc. */
extern SimpleType TargetType(enum TypeRule, SimpleType actual);

/*** Scalar operations -- scalar.c ***/

/* Setters, copiers, and mutators: */

extern void InitScalar(Scalar *v, SimpleType t, bool isPtr);
#define InitScalarAsString(v) InitScalar((v), T_STRING, FALSE)
extern void SetToValue(Scalar *d, const Scalar *s);
extern void SetDereferencingBoth(Scalar *d, const Scalar *s);
extern void SetAsPointer(Scalar *d, const Scalar *s);
extern void SetPointerTo(Scalar *v, void *p, SimpleType t);
extern void SetPointerToElement(Scalar *indexer, const Scalar *vector, long offset);
extern void SetFromLong(Scalar *v, long val, SimpleType t);
extern void SetFromDouble(Scalar *v, double val, SimpleType t);
extern void SetCharacter(Scalar *v, char c);
extern void SetBoolean(Scalar *v, bool b);
extern Error SetError(Scalar *v, Error e);
extern void SetIntOrLong(Scalar *v, long value);
extern void SetTruncated(Scalar *v, long value, SimpleType t1, SimpleType t2);
extern void CopyScalar(Scalar *dest, const Scalar *src);
extern void CopyDereferencingSource(Scalar *dest, const Scalar *src);
extern void CopyDereferencingBoth(Scalar *dest, const Scalar *src);
extern void DisposeScalar(Scalar *v);

/* Type conversion: */

extern Error ChangeType(Scalar *v, enum TypeRule tnew);
extern Error ChangeTypeWithContext(Scalar *v, SimpleType tpreceding, enum TypeRule tnew);
extern Error ScalarToString(const Scalar *, QString *);
extern Error ScalarToToken(const Scalar *, QString *);
extern void NumberToCString(const union NumericalValue *value, SimpleType type, char *buffer, bool padPositive);
extern Error ParseNumber(const QString *, Scalar *);
extern Error ParseAs(const QString *, Scalar *, SimpleType);
extern Error ParseToken(const QString *, Scalar *);

/* Accessors: */

#define ScalarIsError(s) ((s)->type == T_ERROR)
extern long GetLong(const Scalar *);
extern double GetDouble(const Scalar *);
extern char GetCharacter(const Scalar *);
extern bool GetBoolean(const Scalar *);
#define IsPointer(s) (((s)->type & T_POINTER) != 0)
extern void *GetPointer(Scalar *);
extern Error WriteScalar(FILE *, const Scalar *);

/* Comparison: */

extern int Compare(Scalar *result, const Scalar *left, const Scalar *right);

/*** Useful scalar constant values -- scalar.c ***/

/* Initialise the following singleton global 'constant' values. */
extern void InitConstants(void);

extern const Scalar *const g_Empty;
extern const Scalar *const g_ZeroInt;
extern const Scalar *const g_OneInt;
extern const Scalar *const g_NegOneInt;
extern const Scalar *const g_MinimumInt; /* i.e. SHRT_MIN */
extern const Scalar *const g_MaximumInt; /* i.e. SHRT_MAX */
extern const Scalar *const g_ZeroLongInt;
extern const Scalar *const g_OneLongInt;
extern const Scalar *const g_NegOneLongInt;
extern const Scalar *const g_NullString; /* 'BASIC null' or zero-byte string */
extern const Scalar *const g_EmptyString; /* zero-length C string, "" */
extern const Scalar *const g_CurDirString; /* platform-dependent */
extern const Scalar *const g_NulChar;
extern const Scalar *const g_TrueBoolean;
extern const Scalar *const g_FalseBoolean;

/*** BObject manipulation -- bobject.c ***/

extern void InitObject(BObject *, enum SymbolType);
extern bool IsEmpty(const BObject *);
extern void CopyObject(BObject *dest, const BObject *source);
extern SimpleType GetSimpleType(const BObject *);
extern void RemoveObject(BObject *, bool);
extern Error ObjectAsError(const BObject *);
#define ObjectIsError(obj) ((obj)->category == LITERAL && (obj)->value.scalar.type == T_ERROR)
#define IndicatesError(obj) (ObjectAsError(obj) != SUCCESS) /* subtly different from the above */
extern void SetObjectToError(BObject *, Error);
extern void SetObjectToErrorWithAdditionalMessage(BObject *, Error, const char *msgFmt, const QString *obj);
extern bool IsUserDefined(const BObject *);
extern void ConvertToObject(const QString *token, BObject *obj, short callNestLevel);
extern void SetSymbolReference(BObject *, enum SymbolType, void *);
extern Error DereferenceObject(BObject *);
DIAGNOSTIC_FN_DECL(void DumpObject(const BObject *));

/*** Symbol table -- symtab.c ***/

extern BObject *LookUp(const QString *, short callNestLevel);
extern BObject *LookUpIgnoringType(const QString *, short callNestLevel);
extern BObject *LookUpCheckingType(const QString *, short callNestLevel);
extern BObject *CreateDefinition(const QString *, void *value, enum SymbolType, short callNestLevel);
extern Error DefineSymbol(const QString *, void *value, enum SymbolType, short callNestLevel);
extern void ClearOutOfContextItems(short);
extern void EnsureExistsIfBuiltIn(const QString *);
/* Is the symbol guaranteed to be defined by the interpreter as part of the language'n'runtime, rather than programmatically? */
#define LexicallyGuaranteedBuiltIn(name) !isalnum(QsGetFirst(name))
DIAGNOSTIC_FN_DECL(void PrintSymTabStatus(void));
DIAGNOSTIC_FN_DECL(void PrintSymTab(void));

/*** Variables -- vars.c ***/

extern bool CanDefineVariable(const QString *mootedName, short mootedCallNestLevel);
extern void InitVariable(struct Variable *, SimpleType type, bool isPointer);
extern BObject *DefineVariable(const QString *name, SimpleType type, short callNestLevel, bool isPointer);
extern Error CreateArgumentVariable(const struct Parameter *, const BObject *actual);
extern Error AssignToStaticParameter(const struct Parameter *, struct Variable *predefinedLocal, const BObject *actual);
extern Error ShareVariable(const QString *name, const BObject *global);
extern void DisposeVariableObject(BObject *);
#define IsVariable(obj) (((obj)->category & IS_VARIABLE) != 0)
extern bool IsArray(const BObject *);
extern struct Variable *VarPtr(const BObject *);
extern Scalar *VarData(const BObject *);
extern Error IndexArray(struct Variable *indexer, const struct Variable *array, const BObject *subscript, unsigned count);
extern void ResetDataReadPointer(void);

/*** Operators -- operators.c ***/

extern const struct Operator *ResolveOperator(const QString *token);
extern int ComparePriority(const struct Operator *left, const struct Operator *right);
extern unsigned OperandCount(const struct Operator *);
extern unsigned Range(const struct Operator *);
extern const struct Parameter *ParametersForOperator(const struct Operator *);
extern void EvalOperation(Scalar *result, const struct Operator *operation, const Scalar *op1, const Scalar *op2);

/*** Statements -- statements.c ***/

#define IsSubprogram(s) ((s)->userDefined)
#define IsMacro(s) ((s)->formalCount == TOKENISED_ARGUMENTS)
extern Error GetStatement(const QString *, const struct Statement **);
extern void DisposeStatement(struct Statement *);
extern bool StatementIsEmpty(const struct Statement *command);

/*** Subprograms -- subs.c ***/

extern void CallSubprogram(const struct Statement *, const BObject *param, unsigned count, bool firstTime);
extern void UnwindForErrorReport(const char **file, int *line, const char **stmt);

/*** Functions -- functions.c ***/

#define IsDefFunction(f) ((f)->method == NULL)
extern void CallFunction(BObject *result, const struct Function *, BObject *param, unsigned count, struct Stack *);
extern void DisposeFunction(struct Function *);
extern void ResetStaticFunctionParams(void);

/*** Compile and execute a statement -- repl.c ***/

extern Error Prepare(struct TokenSequence *tokSeq);
extern void Do(struct Process *proc, struct TokenSequence *tokSeq, struct Stack *exprStack);

/*** Performance Improving Transformations -- pit.c ***/

extern bool EligibleForCaching(const struct TokenSequence *, short callNestLevelWhenExecuted);
extern bool NoDynamicallyAllocatedMemory(const struct TokenSequence *);
extern void StorePreconvertedObjects(struct TokenSequence *, short callNestLevelWhenExecuted);
extern bool IsAssignmentStatement(const struct Statement *);
extern const BObject *AssignmentTarget(const struct TokenSequence *ts, short callNestLevel);
extern void ImproveIfAssignmentStatement(struct TokenSequence *ts, const BObject *vdef, short callNestLevelWhenExecuted);
extern void Improve(struct TokenSequence *);

/*** Control flow stack -- controlflow.c ***/

extern void CreateControlFlowStack(unsigned short height);
extern void DisposeControlFlowStack(void);
extern void PushActivationRecord(const struct Statement *);
extern const char *StartOfCurrentSubprogram(void);
extern bool InStaticContext(const struct Process *proc);
extern struct HashTable *CurrentStaticContext(const struct Process *);
extern void DiscardCurrentControlFlow(void);
extern void ReturnFromSubprogram(void);
extern Error CheckForUnbalancedBlocks(bool inSubprogram);
extern long StackSpaceNeverUsed(void);
/* Needs to peek at the control flow stack, hence lives in controlflow.c - */
extern bool InPotentiallyHotPath(void);
/*extern bool GuaranteedLive(void);*/

/*** Labels -- label.c ***/

extern bool AddLabel(const QString *, const char *labelledPosition, short callNestLevel);
extern const char *FindReferencedLabel(const QString *, Error *);
extern void FindLabelPreceding(/*out*/ QString *label, /*in*/ const char *limit);

/*** Formal against actual parameter checking and transformation -- semantics.c ***/

extern Error Conform(const struct Parameter *formal, int formalCount, BObject *actual, unsigned actualCount);
extern Error ConformQuickly(const struct Parameter *formal, BObject *actual, int count);
extern Error ConformForApplication(const BObject *applied, BObject *actual, unsigned actualCount);
extern const struct Parameter *FormalForActual(const struct Parameter *formal, int formalCount, unsigned actual);
extern bool SemanticallyPredictable(const struct TokenSequence *ts);
extern SimpleType TypeOfToken(const QString *);

/*** Lexical analyser -- lexer.c ***/

extern void CreateTokenSequence(struct TokenSequence *, unsigned short initialCapacity);
extern Error Tokenise(const char **position, struct TokenSequence *, bool skipOnError);
extern struct TokenSequence *Duplicate(const struct TokenSequence *);
extern void ClearTokenSequence(struct TokenSequence *);
extern void DisposeTokenSequence(struct TokenSequence *);
extern void ExpandTokenSequence(struct TokenSequence *, unsigned short newSize);
extern void ReplaceTokens(struct TokenSequence *, QString *newTokens, unsigned short count);
extern void DeleteToken(struct TokenSequence *, unsigned short index);
DIAGNOSTIC_FN_DECL(void PrintTokSeq(const struct TokenSequence *));

/*** Token classification -- lexer.c ***/

extern bool IsSimpleTerminator(char);
extern bool IsTerminator(const QString *);
extern bool IsSeparator(const QString *t);
extern bool IsWhiteSpace(char);
extern bool IsNumeric(const QString *);
extern bool IsName(const QString *s);
extern bool IsValidName(const QString *);
extern bool IsLParen(const QString *t);
extern bool IsRParen(const QString *t);
extern bool IsLiteral(const QString *t);
extern bool IsValidLabelOrLineNumber(const QString *);
#define IsQuote(c) ((c) == '\"')
#define IsQuotedLiteral(s) IsQuote(QsGetFirst(s)) /* i.e. a string or character literal */

/*** Punctuation -- punctuation.c ***/

extern bool IsPunctuation(const QString *t);
extern void Nest(const QString *, int *nesting);

/*** Standardise syntactic sugar and idiosyncracies of BASIC -- sugar.c ***/

extern void MakeSavoury(struct TokenSequence *);
extern bool IsParameterSeparatingKeyword(const QString *token);

/*** Syntax and parsing -- syntax.c ***/

extern Error CheckExpressionSyntax(const QString *infixExpr, int nTokens, const char *context);
extern Error CheckStatementSyntax(const struct TokenSequence *);
extern bool RequiresSyntaxCheck(const struct TokenSequence *);
extern QString *InfixToPrefix(const QString *start, int nTokens, unsigned *count);
extern Error CheckNameList(const QString *first, int nTokens, bool allowExplicitValueParameters);
extern struct Parameter *ParseNameList(const QString *first, int nTokens, short *nParams, enum SymbolType defaultKind);

/*** Expression evaluation -- eval.c ***/

extern const QString *Eval(const QString *toks, Interner intern, unsigned tokIndex, struct Stack *exprStack);
extern const BObject *EvalPreconverted(const BObject *exprSeq, struct Stack *exprStack);
	
extern void CreateExprStk(struct Stack *, unsigned maxHeight);
extern void ClearExprStk(struct Stack *);
extern void DisposeExprStk(struct Stack *);
extern void CutExprStk(struct Stack *, int count);
#define PeekExprStk(stk, offset) ((BObject *)StkPeek(stk, offset))
DIAGNOSTIC_FN_DECL(void DumpExprStk(const struct Stack *));

/*** Some keywords and other special symbols -- sugar.c ***/

extern const char KW_PLUS[];
extern const char KW_MINUS[];
extern const char KW_UNAMBIGUOUS_UNARY_PLUS[]; /* +u */
extern const char KW_UNAMBIGUOUS_UNARY_MINUS[]; /* -u */
extern const char KW_LPAREN[];
extern const char KW_RPAREN[];
extern const char KW_COMMA[];
extern const char KW_EQUALS[];
extern const char KW_AREA[];
extern const char KW_CLOSE[];
extern const char KW_CONST[];
extern const char KW_DATA[];
extern const char KW_DEF[];
extern const char KW_DIM[];
extern const char KW_DISABLE[];
extern const char KW_ELSE[];
extern const char KW_ELSEIF[];
extern const char KW_EMPTY_STMT[]; /* e~ (the empty statement) */
extern const char KW_ENABLE[];
extern const char KW_END[];
extern const char KW_FOR[];
extern const char KW_FPRINT[];
extern const char KW_GOSUB[];
extern const char KW_GOTO[];
extern const char KW_IF[];
extern const char KW_IFGOTO[];
extern const char KW_IFTHENELSE[];
extern const char KW_IFTHENLET[];
extern const char KW_INPUT[];
extern const char KW_INSTR[];
extern const char KW_LEN[];
extern const char KW_LETQ_LOCAL[]; /* Internal only - faster interning. */
extern const char KW_LETQ_PREDEF[]; /* Internal only - faster interning. */
extern const char KW_LET[];
extern const char KW_MID[];
extern const char KW_NAME[];
extern const char KW_NEXT[];
extern const char KW_NEXTVAR[];
extern const char KW_ON[];
extern const char KW_ONGOSUB[];
extern const char KW_ONGOTO[];
extern const char KW_OPEN[];
extern const char KW_PRINT[];
extern const char KW_RETURN[];
extern const char KW_RETURNTO[];
extern const char KW_SCREEN[];
extern const char KW_STOP[];
extern const char KW_SUSPEND[];

extern const QString g_LParen;
extern const QString g_RParen;
extern const QString g_Pipe;
extern const QString g_Semicolon;
extern const QString g_AsKeyword;
extern const QString g_CallKeyword;
extern const QString g_DataKeyword;
extern const QString g_ElseKeyword;
extern const QString g_EndKeyword;
extern const QString g_EndSubKeyword;
extern const QString g_GoToKeyword;
extern const QString g_StaticKeyword;
extern const QString g_SubKeyword;
extern const QString g_ThenKeyword;
extern const QString g_WhereKeyword;
extern const QString g_ScalarYieldingFunction; /* v~ (internal function) */
extern const QString g_Missing; /* m~ (internal placeholder for an unsupplied parameter) */

/*** I/O statement and function support -- io.c ***/

extern void InitStreams(void);
extern void CloseAllStreams(void);

/*** Event trapping -- events.c ***/

extern void InitEventTraps(void);
extern void DisposeEventTraps(void);
extern bool CheckForEvents(struct Process *);
extern void ReenableEventTraps(struct Process *, short callNestLevel);
extern void CauseError(Error);
extern bool ValidEventName(const QString *name);

/*** Profiling -- profile.c ***/

struct Statistics;
struct Buffer;

extern void InitProfile(struct Statistics **);
extern void DisposeProfilingData(struct Statistics **);
extern void IncrExecutionCount(struct Statistics **, const struct Buffer *, const char *stmtStart, const char *stmtEnd, float seconds);
extern void PrintProfile(struct Statistics *, const struct Buffer *, FILE *);
DIAGNOSTIC_FN_DECL(void PrintProfilerStatisticsList(struct Statistics *, const struct Buffer *));

/*** Dead-end error handling -- main.c ***/

extern void RequireSuccess(Error);

/*** Module location and loading -- loader.c ***/

extern char *ResolveModuleLocation(const char *module);
extern Error LoadProgram(const char *source, bool isOneLiner, bool isHidden);

/*** Bootstrapping ***/

extern void DefineBuiltIns(void);
extern void DefinePunctuation(void);
extern void DefineBuiltInStatements(void);
extern void DefineOperators(void);
extern void DefineBuiltInFunctions(void);
extern void DisposeSymbolTable(void);
	
#endif /* BAS_INTERPRETER_H_INCLUDED */
