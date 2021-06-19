/****** punctuation.c ******/

/*
	$VER: punctuation.c 0.16 (8.4.2014)

	Punctuation tokens.
*/

#include "interpreter.h"

static const struct PunctuationSymbol m_Punctuation[] =
{
	{0, ',', FALSE, TRUE},
	{0, ';', FALSE, TRUE},
	{0, '|', FALSE, TRUE},
	{1, '(', TRUE, FALSE},
	{-1, ')', FALSE, TRUE},
	{0, NUL, FALSE, FALSE} /* Sentinel, aka 'the null punctuator' */
};

/* Null punctuation instance -- for all symbols which aren't - */
static const struct PunctuationSymbol *const m_NullPunctuation = &m_Punctuation[5];

void DefinePunctuation(void)
{
	const struct PunctuationSymbol *p;
	for(p = &m_Punctuation[0]; p->token != NUL; p++) {
		QString name;
		QsInitStatic(&name, &p->token, 1);
		RequireSuccess(DefineSymbol(&name, (void *)p, PUNCTUATION, SCOPE_BUILTIN));
	}
}

bool IsPunctuation(const QString *sym)
{
	const BObject *obj = LookUp(sym, SCOPE_BUILTIN);
	return obj != NULL && obj->category == PUNCTUATION;
}

void Nest(const QString *sym, int *nesting)
{
	char first = QsGetFirst(sym);
	*nesting += (first == '(') - (first == ')');
}
