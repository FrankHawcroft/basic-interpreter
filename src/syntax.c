/****** syntax.c ******/

/*
	$VER: syntax.c 0.16 (9.7.2014)

	Syntax checking, expression parsing, and parameter list parsing.
*/

#include <ctype.h>
#include <limits.h>
#include "interpreter.h"
#include "heap.h"
#include "hashtable.h"
#include "options.h"
#include "buffer.h"
#include "process.h"

struct Tag {
	const QString *tok;
	const struct Operator *op; /* NULL if not a real operator. */
	signed char nesting;
	unsigned char spaceRequired;
	bool isLiteral;
	bool isTerm;
	bool isSeparator;
	bool isStatementParameterSeparator;
	bool isInteresting; /* Not removed in parsing. */
	bool isFunctor; /* Array indexing or function call. */
};

const struct Tag m_OutOfBoundsDelimiter = { &g_Semicolon, NULL, 0, 0, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE };

static void Categorise(struct Tag *tag, const QString *token, const QString *next, int *nesting)
{
	Nest(token, nesting);
	tag->nesting = (char)*nesting;
	
	tag->tok = token;
	
	/* Set flags in this order to avoid looking up symbol (ResolveOperator) unless necessary - */
	tag->isLiteral = IsLiteral(token);
	tag->isSeparator = IsSeparator(token);
	tag->isStatementParameterSeparator = QsGetFirst(token) == ';' || IsTerminator(token);
	tag->op = tag->isLiteral || tag->isSeparator || tag->isStatementParameterSeparator
		? NULL : ResolveOperator(token);
	tag->isTerm = tag->op == NULL && (tag->isLiteral || IsName(token));
	tag->isInteresting = tag->isSeparator || tag->isTerm || tag->op != NULL;
	tag->isFunctor = next != NULL && QsGetFirst(next) == '(';
	
	/* Since a <op> b --> (<op> a b), and separators aren't needed: */
	tag->spaceRequired = tag->op != NULL ? 3 : !tag->isSeparator;
}

static unsigned TrueOperandCount(const struct Tag *t)
{
	return t->op != NULL ? OperandCount(t->op) : 0;
}

Error CheckExpressionSyntax(const QString *infixExprSeq, int nTokens, const char *context)
{
	Error error = SUCCESS;
	int nesting = 0, i;
	struct Tag prev = m_OutOfBoundsDelimiter,
		curr = m_OutOfBoundsDelimiter,
		next = m_OutOfBoundsDelimiter;

	if(nTokens > 0)
		Categorise(&curr, infixExprSeq, nTokens > 1 ? &infixExprSeq[1] : NULL, &nesting);
	if(nTokens > 1)
		Categorise(&next, &infixExprSeq[1], nTokens > 2 ? &infixExprSeq[2] : NULL, &nesting);
	
	for(i = 0; i < nTokens && error == SUCCESS; i++) {
		if(curr.nesting < 0)
			error = MISMATCHEDPARENS;
		else if(curr.isStatementParameterSeparator)
			error = 0 <= curr.nesting && curr.nesting <= 1 ? SUCCESS : MISMATCHEDPARENS;
		else if(IsLParen(curr.tok))
			error = !(IsRParen(next.tok) || next.isSeparator || next.isStatementParameterSeparator)
				? SUCCESS : BADSYNTAX;
		else if(curr.isTerm)
			error = !prev.isTerm && !next.isTerm ? SUCCESS : BADSYNTAX;
		else if(TrueOperandCount(&curr) == 2)
			error = (prev.isTerm || IsRParen(prev.tok))
			  && (next.isTerm || IsLParen(next.tok) || TrueOperandCount(&next) == 1)
				? SUCCESS : BADARGCOUNT;
		else if(TrueOperandCount(&curr) == 1)
			error = !(prev.isTerm || IsRParen(prev.tok))
			  && (next.isTerm || IsLParen(next.tok) || TrueOperandCount(&next) == 1)
				? SUCCESS : BADARGCOUNT;

		if(error != SUCCESS)
			/* TODO could interpolate prev and next, or report > 1 token as offending -
				as this won't work if a generated token */
			error = PositionError(error, context, QsGetData(curr.tok));

		prev = curr;
		curr = next;
		if(i + 2 < nTokens)
			Categorise(&next, &infixExprSeq[i + 2], i + 3 < nTokens ? &infixExprSeq[i + 3] : NULL, &nesting);
		else
			next = m_OutOfBoundsDelimiter;
	}
	
	if(nesting != 0)
		/* No point recording a position (column) in the error. */
		error = MISMATCHEDPARENS;
	
	return error;
}

static bool WithinInitiallyLoadedProgram(const char *position)
{
	/* Don't include the prelude - */
	 return /*PrimaryBufferBase(Proc()->buffer) <= position && */ position < Proc()->initialTextExtent;
}

bool CanAssumeCommonSyntax(const struct Statement *cmd)
{
	return cmd == NULL || !IsMacro(cmd);
}

bool RequiresSyntaxCheck(const struct TokenSequence *tokens)
{
	return tokens->ops == 0 /* i.e. hasn't been previously executed and cached */
		&& tokens->length >= 2
		&& CanAssumeCommonSyntax(tokens->command)
		&& !(Opts()->initialSyntaxCheck && WithinInitiallyLoadedProgram(tokens->start));
}

Error CheckStatementSyntax(const struct TokenSequence *tokens)
{
	return CheckExpressionSyntax(tokens->rest, (int)tokens->length, tokens->start);
}

static int ComparePseudoPriority(const struct Tag *t1, const struct Tag *t2)
{
	if(t1->nesting > t2->nesting) return 1;
	if(t2->nesting > t1->nesting) return -1;
	if(t1->op != NULL && t2->op != NULL) return ComparePriority(t1->op, t2->op);
	if(t1->isTerm) return 1;
	if(t2->isTerm) return -1;
	if(t1->op != NULL) return 1;
	if(t2->op != NULL) return -1;
	if(t1->isSeparator) return 1;
	if(t2->isSeparator) return -1;
	return 0;
}

static const struct Tag *FinalOperator(const struct Tag *start, const struct Tag *end)
{
	const struct Tag *scan, *finalPseudoOp = start;
	
	for(scan = start + 1; scan <= end; scan++)
		if(scan->isInteresting && ComparePseudoPriority(finalPseudoOp, scan) > 0)
			finalPseudoOp = scan;
	
	return finalPseudoOp->isInteresting ? finalPseudoOp : NULL;
}

static unsigned PseudoOperandCount(const struct Tag *t)
{
	return t->isSeparator ? 2 : TrueOperandCount(t);
}

static bool InfixToPrefixRec(const struct Tag *start, int nTokens, QString *out, unsigned *count)
{
	const struct Tag *end = start + nTokens - 1, *pseudoOperator;
	unsigned countBefore;
	bool closingRParen = FALSE;
	
	if(start > end)
		return TRUE;

	if((pseudoOperator = FinalOperator(start, end)) == NULL)
		return FALSE;
		
	assert(start <= pseudoOperator && pseudoOperator <= end);
	
	/* Output the start of an 'apply' expression, (f <x> <y> ...), if necessary - */

	if(pseudoOperator->isFunctor || pseudoOperator->op != NULL) {
		closingRParen = TRUE;
		QsCopy(out++, &g_LParen);
		++*count;
	}
	
	/* Output the term or operator being applied - */
	
	if(!pseudoOperator->isSeparator) {
		QsCopy(out++, pseudoOperator->tok);
		++*count;
	}
	
	/* Output the left subexpression or preceding expression sequence - */

	if(PseudoOperandCount(pseudoOperator) == 2) {
		countBefore = *count;
		if(!InfixToPrefixRec(start, pseudoOperator - start, out, count))
			return FALSE;
		out += *count - countBefore;
	}

	/* Output the right subexpression or following expression sequence - */

	countBefore = *count;
	if(!InfixToPrefixRec(pseudoOperator + 1 + pseudoOperator->isFunctor,
		end - pseudoOperator - 2 * pseudoOperator->isFunctor, out, count))
		return FALSE;
	out += *count - countBefore;

	/* Balance parentheses - */
	
	if(closingRParen) {
		QsCopy(out++, &g_RParen);
		++*count;
	}
	
	return TRUE;
}

#define PREALLOCATED_TAGS 40

/* Allocates a vector of strings which is filled with a prefix, Lisp-like form of the sequence of expressions.
	The number of tokens in the returned expression sequence is set in *generatedCount (if supplied). 
	Returns NULL if no tokens supplied (nTokens == 0), or a syntax error was found which prevented parsing. */
QString *InfixToPrefix(const QString *tok, int nTokens, unsigned *generatedCount)
{
	struct Tag *tag;
	struct Tag tagSpace[PREALLOCATED_TAGS];
	unsigned spaceRequired = 1, localGeneratedCount;
	
	assert(nTokens >= 0);
	assert(tok != NULL || nTokens == 0);
	
	if(generatedCount == NULL)
		generatedCount = &localGeneratedCount;
	
	*generatedCount = 0;
	
	if(nTokens == 0)
		return NULL;
	
	tag = nTokens <= PREALLOCATED_TAGS ? &tagSpace[0] : New(sizeof(struct Tag) * nTokens);
	
	{
		int nesting = 0, t;
		for (t = 0; t < nTokens; t++) {
			Categorise(&tag[t], &tok[t], t + 1 < nTokens ? &tok[t + 1] : NULL, &nesting);
			spaceRequired += tag[t].spaceRequired;
		}
	}
	
	{
		QString *prefixForm = New(sizeof(QString) * spaceRequired);
		bool success = InfixToPrefixRec(tag, nTokens, prefixForm, generatedCount);

		assert(*generatedCount < spaceRequired);

		if(success) {
			/* Put a standard delimiter at the end so expression evaluation terminates. */
			QsCopy(prefixForm + *generatedCount, &g_Pipe);
			++*generatedCount;
		}
		else {
			Dispose(prefixForm);
			prefixForm = NULL;
			*generatedCount = 0;
		}

		if(nTokens > PREALLOCATED_TAGS)
			Dispose(tag);

		return prefixForm;
	}
}

/* Check the syntax and uniqueness of a list of parameters.
	If allowExplicitValueParameters is true, sequences like
		(x)
are allowed, indicating that x will be passed by value. */
Error CheckNameList(const QString *tok, int nTokens, bool allowExplicitValueParameters)
{
	struct HashTable *names = HtCreate(11, NULL, &QsEqNoCase);
	Error result = SUCCESS;
	int i;
	bool dummyDefinition = TRUE, anyParams = FALSE;
	
	assert(tok != NULL);
	assert(nTokens > 0);

	for(i = 0; i < nTokens && result == SUCCESS; i++) {
		const QString *t = &tok[i];
		bool syntaxOK = FALSE;
	
		if(IsName(t)) {
			anyParams = TRUE;
			syntaxOK = t == tok
				|| QsGetFirst(t - 1) == ','
				|| (allowExplicitValueParameters && IsLParen(t - 1));
				
			if(syntaxOK) {
				if(HtLookUp(names, t) != NULL)
					result = REDEFINE;
				else
					HtAdd(names, t, &dummyDefinition);
			}
		}
		else if(isdigit(QsGetFirst(t)))
			/* Dimension count is allowed, but ignored. */
			syntaxOK = t > tok && IsLParen(t - 1);
		else if(IsLParen(t))
			syntaxOK = i + 1 < nTokens 
				&& ((t > tok && IsName(t - 1))
					|| (allowExplicitValueParameters && IsName(t + 1)));
		else if(IsRParen(t))
			syntaxOK = t > tok 
				&& (IsLParen(t - 1) || isdigit(QsGetFirst(t - 1))
					|| (allowExplicitValueParameters && IsName(t - 1)));
		else if(QsGetFirst(t) == ',')
			syntaxOK = t > tok && i + 1 < nTokens && QsGetFirst(t - 1) != ',';
		
		if(!syntaxOK && result == SUCCESS)
			result = BADSYNTAX;
	}
	
	HtDispose(names);
	
	if(!anyParams && result == SUCCESS) 
		result = BADSYNTAX;
	
	return result;
}

/* Parse a list of parameters.
	It's assumed the syntax of the parameter list is valid (see: CheckNameList),
and there are some parameters present.
	Returns NULL on error (i.e. memory can't be allocated for the parameter vector). */
struct Parameter *ParseNameList(const QString *tok, int nTokens, short *nParams, enum SymbolType defaultKind)
{
	struct Parameter *param = NULL, *p;
	int paramCount = 0, i;
	
	assert(tok != NULL);
	assert(nTokens > 0);
	assert(nParams != NULL);

	*nParams = 0;
		
	for(i = 0; i < nTokens; i++)
		paramCount += IsName(&tok[i]) ? 1 : 0;
	
	if((param = TolerantNew(sizeof(struct Parameter) * paramCount)) == NULL)
		return NULL;
	
	for(p = param, i = 0; i < nTokens; i++) {
		const QString *t = &tok[i];
		if(IsName(t)) {
			SimpleType type = TypeForName(t);
		
			if(t > tok && IsLParen(t - 1)) p->kind = LITERAL;
			else if(i + 1 < nTokens && IsLParen(t + 1)) p->kind = ARRAY; /* vvv */
			else p->kind = defaultKind;
			
			p->type = p->kind == LITERAL ? UsualTypeConversionToProduce(type) : StrictTypeConversionFor(type);
			p->explicitlyTyped = IsTypeSpecifier(QsGetLast(t));
			p->defaultValue = NULL;
			QsGetSubstring(&p->name, t, 0, QsGetLength(t) - p->explicitlyTyped);
			p->maxCount = 1;
			/*p->isArray = IsVarParam(p) && i + 1 < nTokens && IsLParen(t + 1); vvv*/
			
			++p;
		}
	}
		
	*nParams = (short)paramCount;
	
	return param;
}
