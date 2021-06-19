/****** symtab.c ******/

/*
	$VER: symtab.c 0.16A (6.4.2016)

	Symbol table management.
*/

#include <stdio.h>
#include <limits.h>
#include <string.h>
#include <ctype.h>
#include "interpreter.h"
#include "process.h"
#include "hashtable.h"
#include "heap.h"
#include "options.h"

#define EnvTableSize(n) (sizeof(struct HashTable *) * (n))

static struct HashTable **AllocEnvTable(short count)
{
	struct HashTable **t = New(EnvTableSize(count));
	memset(&t[0], 0, EnvTableSize(count));
	return t;
}

/* Initialises the symbol tables ready for use. */
static void InitSymbolTable(void)
{
	Proc()->envCount = Opts()->lowMemory ? -SCOPE_NONEXISTENT : 10;
	Proc()->environment = AllocEnvTable(Proc()->envCount);
	Proc()->retainedWarmEnvs = Opts()->lowMemory ? -SCOPE_NONEXISTENT - 1 : 10;
#ifdef DEBUG
	Proc()->maxNestLevel = SCOPE_NONEXISTENT;
	Proc()->definitions = Proc()->hashTableSearches = Proc()->lookUps = 0;
#endif
}

/* Clears the symbol tables, disposing of all the definitions. */
void DisposeSymbolTable(void)
{
	if(Proc()->environment != NULL) {
		ClearOutOfContextItems(SCOPE_BUILTIN);
		Dispose(Proc()->environment);
		Proc()->environment = NULL;
		Proc()->envCount = 0;
	}
}

INLINE short EnvironmentIndex(short callNestLevel)
{
	assert(callNestLevel >= SCOPE_BUILTIN);
	return callNestLevel - SCOPE_BUILTIN;
}

INLINE struct HashTable *ProbeEnvironment(struct Process *proc, short callNestLevel)
{
	/*struct HashTable *staticEnv;*/
	if(callNestLevel == SCOPE_STATIC || (callNestLevel > SCOPE_MAIN && InStaticContext(proc)))
	/*|| (proc->functionCallNesting == SCOPE_MAIN
	  && callNestLevel > SCOPE_MAIN
	  && (staticEnv = CurrentStaticContext(proc)) != NULL))*/
		/* callNestLevel > SCOPE_MAIN && InStaticContext(proc))) */
		return CurrentStaticContext(proc);
	else {
		short index = EnvironmentIndex(callNestLevel);
		return index < proc->envCount ? proc->environment[index] : NULL;
	}
}

INLINE struct HashTable *ResolveEnvironment(struct Process *proc, short callNestLevel)
{
	struct HashTable *env = ProbeEnvironment(proc, callNestLevel);
#ifdef DEBUG
	if(callNestLevel > proc->maxNestLevel)
		proc->maxNestLevel = callNestLevel;
#endif
	if(env != NULL)
		return env;
	else if(EnvironmentIndex(callNestLevel) >= proc->envCount) {
		short increasedEnvCount = (3 * proc->envCount) / 2;
		struct HashTable **newEnvs = AllocEnvTable(increasedEnvCount);
		/*fprintf(stderr, "[Allocating space for %d environments for cnl %d]\n", increasedEnvCount, callNestLevel);*/
		memcpy(newEnvs, proc->environment, EnvTableSize(proc->envCount));
		Dispose(proc->environment);
		proc->environment = newEnvs;
		proc->envCount = increasedEnvCount;
	}
	return ProbeEnvironment(proc, callNestLevel);
}

/* Find a definition of a symbol.

The call nest level determines which symbol table(s) will be searched:

Built-in functions, commands, and operators are visible globally and reside in the
SCOPE_BUILTIN table.
Functions, subprograms, line numbers, shared arrays, and named constants are also visible
globally and are in the SCOPE_GLOBAL table.
Ordinary variables and labels are visible only within the main program, subprogram, or function 
in which they are defined. They are in the SCOPE_MAIN table or, if defined in a subprogram
or function, a higher-indexed table. If a call nest level >= 0 is passed, the given symbol
table will be searched before the globally visible ones. 

This function does not deal with type specifying characters. See the following two variants. */
BObject *LookUp(const QString *symbol, short callNestLevel)
{
	BObject *definition = NULL;
	struct Process *proc = Proc();
	short scope[3], initiallyProbedScope, i;
	
	scope[0] = callNestLevel;
	scope[1] = SCOPE_GLOBAL;
	scope[2] = SCOPE_BUILTIN;

#ifdef DEBUG
	++proc->lookUps;
#endif

	initiallyProbedScope = 0;
	if(callNestLevel == SCOPE_BUILTIN || LexicallyGuaranteedBuiltIn(symbol))
		initiallyProbedScope = 2;
	else if(callNestLevel == SCOPE_GLOBAL)
		initiallyProbedScope = 1;
	
	for(i = initiallyProbedScope; i < 3 && definition == NULL; i++) {
		struct HashTable *relevantTable = ProbeEnvironment(proc, scope[i]);
		definition = relevantTable == NULL ? NULL : HtLookUp(relevantTable, symbol);
#ifdef DEBUG
		proc->hashTableSearches += relevantTable != NULL;
#endif
	}
	
	return definition;
}

INLINE SimpleType TrailingTypeSpec(const QString *symbol)
{
	/* Type specifiers are a pain. 
	The length <= 1 test is so the '&' operator (or any other future operator which also happens to be a 
	type-specifying character) is found. */
	char last;
	return QsGetLength(symbol) <= 1 || isalnum(last = QsGetLast(symbol)) || !IsTypeSpecifier(last)
		? T_MISSING : TypeFromSpecifier(last);
}

/* Find a definition of a symbol, ignoring any trailing type specifier it may have. */
BObject *LookUpIgnoringType(const QString *symbol, short callNestLevel)
{
	if(TrailingTypeSpec(symbol) == T_MISSING)
		return LookUp(symbol, callNestLevel);
	else {
		QString typelessToken;
		QsInitStatic(&typelessToken, QsGetData(symbol), QsGetLength(symbol) - 1);	
		return LookUp(&typelessToken, callNestLevel);
	}
}

/* Find a definition of a symbol, checking that its trailing-character specified type matches if it has one. */
BObject *LookUpCheckingType(const QString *symbol, short callNestLevel)
{
	SimpleType tokenType = TrailingTypeSpec(symbol);
	if(tokenType == T_MISSING)
		return LookUp(symbol, callNestLevel);
	else {
		QString typelessToken;
		BObject *definition;
		
		QsInitStatic(&typelessToken, QsGetData(symbol), QsGetLength(symbol) - 1);	
		definition = LookUp(&typelessToken, callNestLevel);
		return definition != NULL && tokenType == GetSimpleType(definition) ? definition : NULL;
	}
}

/* Go faster - look up only at the specified call nest level, ignoring type specifier. */
BObject *LookUpLocal(const QString *symbol, short callNestLevel)
{
	struct HashTable *relevantTable = ProbeEnvironment(Proc(), callNestLevel);

	assert(relevantTable != NULL);

#ifdef DEBUG
	++Proc()->hashTableSearches;
#endif

	if(TrailingTypeSpec(symbol) == T_MISSING)
		return HtLookUp(relevantTable, symbol);
	else {
		QString typelessToken;
		QsInitStatic(&typelessToken, QsGetData(symbol), QsGetLength(symbol) - 1);
		return HtLookUp(relevantTable, &typelessToken);
	}
}

static void DisposeDefinition(void *object)
{
	RemoveObject(object, TRUE);
}

INLINE unsigned NumBins(bool lowMem, short callNestLevel)
{
	if(callNestLevel == SCOPE_BUILTIN) return lowMem ? 103 : 809;
	else if(callNestLevel == SCOPE_GLOBAL) return lowMem ? 53 : 577; 
	else if(callNestLevel == SCOPE_MAIN) return lowMem ? 23 : 53;
	else if(!lowMem && callNestLevel <= 5) return 13;
	else return 5;
}

static Error DefineQuickly(struct Definition *defn, short callNestLevel)
{
	struct Process *proc = Proc();
	struct HashTable *relevantTable = ResolveEnvironment(proc, callNestLevel);

	assert(defn != NULL);
	
	/* TODO make HtCreate (optionally?) tolerant of mem alloc failure - */
	if(relevantTable == NULL)
		relevantTable = proc->environment[EnvironmentIndex(callNestLevel)]
			= HtCreate(NumBins(proc->opts->lowMemory, callNestLevel), DisposeDefinition, NULL);

	HtAddPreallocated(relevantTable, defn);
	
#ifdef DEBUG
	++proc->definitions;
#endif
	
	return SUCCESS;
}

/* Avoid separate memory allocations for the definition and the object - */
struct AggregateDefinition {
	struct Definition defn;
	BObject obj;
};

BObject *CreateDefinition(const QString *name, void *value, enum SymbolType kind, short callNestLevel)
{
	struct AggregateDefinition *newDefinition = TolerantNew(sizeof(*newDefinition));
	
	assert(name != NULL && (kind >= SCALAR_VAR) == (value == NULL));
	assert(isalnum(QsGetLast(name)) || !IsTypeSpecifier(QsGetLast(name)) || QsGetFirst(name) == '&');
	
	if(newDefinition != NULL) {
		QsCopy(&newDefinition->defn.key, name);
		DefineQuickly((struct Definition *)newDefinition, callNestLevel);
		if(value != NULL)
			SetSymbolReference(&newDefinition->obj, kind, value);
		else
			newDefinition->obj.category = kind;
		newDefinition->defn.value = &newDefinition->obj;
		return &newDefinition->obj;
	}
	return NULL;
}

Error DefineSymbol(const QString *name, void *value, enum SymbolType kind, short callNestLevel)
{
	return CreateDefinition(name, value, kind, callNestLevel) != NULL ? SUCCESS : NOMEMORY;
}

INLINE short MaxEnvIndex(const struct Process *proc)
{
	return proc->envCount + SCOPE_BUILTIN - 1;
}

void ClearOutOfContextItems(short minimumCallNestLevel)
{
	struct Process *proc = Proc();
	short i;

	for(i = MaxEnvIndex(proc); i >= minimumCallNestLevel; i--) {
		struct HashTable *relevantTable = ProbeEnvironment(proc, i);
		if(relevantTable != NULL) {
			if(minimumCallNestLevel == SCOPE_BUILTIN || i >= proc->retainedWarmEnvs) {
				HtDispose(relevantTable);
				proc->environment[EnvironmentIndex(i)] = NULL;
			}
			else
				HtClear(relevantTable); /* Relies on this being efficient! */
		}
	}
}

void DefineBuiltIns(void)
{
	InitSymbolTable();
	
	/* This order of defining built-ins is tuned for the current hash table implementation,
	which, in the event of a collision, adds the new definition at the start of its bin's list.
	Statements are lowest priority (i.e. added first) because they are stored pre-looked-up
	in the statement cache. */
	
	if(!Opts()->lowMemory)
		DefineBuiltInStatements();	
	DefinePunctuation();
	if(!Opts()->lowMemory) {
		DefineBuiltInFunctions();
		DefineOperators();
	}
}

/* Unfortunately, further increases the entanglement of the symtab with the language constructs
which define themselves therein; but this seems inevitable unless I separate the concepts of
statements, functions, etc. into 'stateless' and 'runtime aware' parts. */
extern bool AttemptToDefineBuiltInFunction(const QString *);
extern bool AttemptToDefineOperator(const QString *);
extern bool AttemptToDefineBuiltInStatement(const QString *);

/* Lazily define built-ins to avoid allocating memory at start up.
	Punctuation is assumed always to be defined at start up (see: DefineBuiltIns). */
void EnsureExistsIfBuiltIn(const QString *tok)
{
	if(Opts()->lowMemory && !(IsLiteral(tok) || IsSeparator(tok)) && LookUp(tok, SCOPE_GLOBAL) == NULL)
		AttemptToDefineBuiltInFunction(tok)
		  || AttemptToDefineOperator(tok)
		  || AttemptToDefineBuiltInStatement(tok);
}

#ifdef DEBUG

/* Shows some stats. */
void PrintSymTabStatus(void)
{
	struct Process *proc = Proc();
	short i;
	
	for(i = MaxEnvIndex(proc); i >= SCOPE_BUILTIN; i--) {
		struct HashTable *relevantTable = ProbeEnvironment(proc, i);
		if(relevantTable != NULL) {
			unsigned bins, defns, maxDefns, collisions;
			HtGetLoadInfo(relevantTable, &bins, &defns, &maxDefns, &collisions);
			fprintf(stderr, "For scope %hd: %u bins, %u definitions (%u max), %u collisions.\n", 
				i, bins, defns, maxDefns, collisions);
		}
	}
	
	fprintf(stderr, "Max call nesting %hd.\n", proc->maxNestLevel);
	fprintf(stderr, "Hash table totals: %lu definitions added; %lu full (multi-table) lookups; %lu searches in total.\n",
		proc->definitions, proc->lookUps, proc->hashTableSearches);
}

/* Dumps all the symtabs to stderr. */
void PrintSymTab(void)
{
	struct Process *proc = Proc();
	short i;

	for(i = MaxEnvIndex(proc); i >= SCOPE_BUILTIN; i--) {
		struct HashTable *relevantTable = ProbeEnvironment(proc, i);
		if(relevantTable != NULL) {
			fprintf(stderr, "-- Symbol table for scope %hd:\n", i);
			HtDump(relevantTable);
		}
	}
}

/* Traverses every definition in every environment, so use with care. */
void VisitAllDefinitions(short minimumCallNestLevel, HtVisitor visitor)
{
	struct Process *proc = Proc();
	short i;

	for (i = MaxEnvIndex(proc); i >= minimumCallNestLevel; i--) {
		struct HashTable *relevantTable = ProbeEnvironment(proc, i);
		if (relevantTable != NULL)
			HtVisit(relevantTable, visitor, NULL);
	}
}

#endif /* DEBUG */
