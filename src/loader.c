/****** loader.c ******/

/*
	$VER: loader.c 0.16A (5.8.2014)

	Program loader.
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "interpreter.h"
#include "process.h"
#include "options.h"
#include "buffer.h"
#include "platform.h"
#include "heap.h"
#include "cache.h"

/* Separator between directories in module search path. */
#define MOD_PATH_SEP_CHAR ';'
static const char MOD_PATH_SEP_STR[] = {MOD_PATH_SEP_CHAR, NUL};

/* Name of the module path search environment variable. */
static const char MOD_SEARCH_ENV_VAR[] = "BAS_MODULE_PATH";

/*extern void InitUI(void);
extern void InitAudio(void);*/
extern void CleanUpUI(void);
extern void CleanUpAudio(void);

/* Appends text to the program buffer. Execution flows through from each
program loaded to the next.

'source' is usually the name of a file to load. However, Perl-style 'one liner' 
programs are also supported, in which case the 'source' parameter actually
contains code which is copied into the buffer.

'isHidden' should be TRUE if this is a hidden module and errors within it
should not be displayed. Typically this is used only for the prelude.
				
Pre: the buffer has been initialised. */
Error LoadProgram(const char *source, bool isOneLiner, bool isHidden)
{
	Error error = SUCCESS;
#ifdef DEBUG
	if(Opts()->verbose)
		fprintf(stderr, "-- Loading program from source %s '%s'.\n", isOneLiner ? "one-liner" : "file",
			source);
#endif

	if(Opts()->preludeDebugging)
		isHidden = FALSE;

	if(!(isOneLiner ? AppendToBuffer(Proc()->buffer, source) : AppendFileToBuffer(Proc()->buffer, source, isHidden))) {
		error = isOneLiner || PfRepresentsStdin(source) || PfFileLength(source) >= (long)GetFreeFileBufferSpace(Proc()->buffer)
			? PROGRAMBUFFERFULL : CANTOPENCODEFILE;
		if(!isOneLiner) {
			/* Report name of problematic file. Can't use the supplementary error message in the process here,
				because it may not exist yet - therefore inconsistent, in that this supplementary message will
				appear before the main error message. */
			if(source != NULL && *source != NUL)
				fprintf(stderr, "Unable to load file: %s\n", source);
			else
				fprintf(stderr, "Is the %s environment variable defined?\n", MOD_SEARCH_ENV_VAR);
		}
	}
	return error;
}

static char *ModuleSearchPath(void)
{	
	char *envVar = getenv(MOD_SEARCH_ENV_VAR);
	size_t length = (envVar != NULL ? strlen(envVar) : 0)
	  + (Opts()->modPath != NULL ? strlen(Opts()->modPath) : 0)
	  + (PfHomeDirectory() != NULL ? strlen(PfHomeDirectory()) : 0)
	  + 2 * strlen(MOD_PATH_SEP_STR) + 1;
	char *searchPath = New(length);
	
	searchPath[0] = NUL;

	/* Priority order: first the command line parameter. */
	if(Opts()->modPath != NULL)
		strcpy(searchPath, Opts()->modPath);

	/* Then the environment variable. */
	if(envVar != NULL) {
		if(strlen(searchPath) != 0 && searchPath[strlen(searchPath) - 1] != MOD_PATH_SEP_CHAR)
			strcat(searchPath, MOD_PATH_SEP_STR);
		strcat(searchPath, envVar);
	}

	/* Then the system default location. */
	if(PfHomeDirectory() != NULL) {
		if(strlen(searchPath) != 0 && searchPath[strlen(searchPath) - 1] != MOD_PATH_SEP_CHAR)
			strcat(searchPath, MOD_PATH_SEP_STR);
		strcat(searchPath, PfHomeDirectory());
	}
	
	return searchPath;
}

char *ResolveModuleLocation(const char *module)
{
	char *resolved;
	
	if(PfIsAbsolutePath(module) || PfIsExplicitlyCwdRelativePath(module)) {
		resolved = New(strlen(module) + 1);
		strcpy(resolved, module);
	}
	else {
		char *searchPath = ModuleSearchPath(), *tok;
		
		/*fprintf(stderr, "[Loader: search path: %s]\n", searchPath);*/
		resolved = New(strlen(searchPath) + strlen(PF_PATH_SEP) + strlen(module) + 1);
		resolved[0] = NUL;
		tok = strtok(searchPath, MOD_PATH_SEP_STR);
		while(tok != NULL) {
			strcpy(resolved, tok);
			PfJoinPath(resolved, module);
			/*fprintf(stderr, "[Loader: trying %s ...]\n", resolved);*/
			tok = PfFileExists(resolved) ? NULL : strtok(NULL, MOD_PATH_SEP_STR);
		}
		Dispose(searchPath);
	}

	/*fprintf(stderr, "[Loader: final module location: %s]\n", resolved);*/

	return resolved;
}

Error LoadPrelude(void)
{
	Error result = SUCCESS;
	if(Opts()->prelude != NULL) {
		const char *prelude = ResolveModuleLocation(Opts()->prelude);
		result = LoadProgram(prelude, FALSE, TRUE);
		Dispose((char *)prelude);
	}
	return result;
}

extern void ClearControlFlowStack(void);

void ResetProgram(void)
{
	DisposeProfilingData(&Proc()->stats);
	InitProfile(&Proc()->stats);
	
	ResetDataReadPointer();

	ClearStatementCache();
	if(Proc()->untakenBranchCache != NULL)
		ClearCache(Proc()->untakenBranchCache);
	
	Proc()->callNestLevel
		= Proc()->staticSubCallNesting
		= Proc()->functionCallNesting = SCOPE_MAIN;
	ClearControlFlowStack();
	ClearOutOfContextItems(SCOPE_GLOBAL);
	InitEventTraps();
	CloseAllStreams();

	CleanUpUI();
	CleanUpAudio();	
	/*InitUI();
	InitAudio();*/

	Proc()->currentPosition	= FileBufferBase(Proc()->buffer);
}
	
/* Checks the program for lexical and syntactic errors. Returns true if none found;
reports only up to a limited maximum number of errors to stderr. */

#define MAX_REPORTED_SYNTAX_ERRORS 20

extern bool CanAssumeCommonSyntax(const struct Statement *cmd);

bool ProgramSyntaxCheckPassed(void)
{
	/* Only bother syntax checking the prelude if this option is chosen - */
	const char *position = Opts()->prelude == NULL || Opts()->preludeDebugging 
		? FileBufferBase(Proc()->buffer) : PrimaryBufferBase(Proc()->buffer);
	Error error;
	int errorCount = 0, line;
	const char *file;
	struct TokenSequence tokens;
	
	if(!Opts()->initialSyntaxCheck)
		return TRUE;

	CreateTokenSequence(&tokens, Opts()->lowMemory ? 6 : 40);
	
	while(WithinFileBuffer(Proc()->buffer, position) && errorCount < MAX_REPORTED_SYNTAX_ERRORS) {
		error = Tokenise(&position, &tokens, TRUE);
		
		/*PrintTokSeq(&tokens);*/
		
		if(error == SUCCESS)
			MakeSavoury(&tokens);
		else if(!QsIsNull(&tokens.statementName))
			/* Look up the command even if tokenisation failed, so that block control structure nesting can
				be tracked if possible. */
			GetStatement(&tokens.statementName, &tokens.command);

		if(error == SUCCESS && CanAssumeCommonSyntax(tokens.command))
			error = CheckStatementSyntax(&tokens);
		
		if(error != SUCCESS) {
			GetLocationInfo(Proc()->buffer, tokens.start, &line, &file);
			ReportError(error, file, line, tokens.start, Proc()->additionalErrorInfo);
			
			++errorCount;
		}
		
		/* Track nested block control statements. */
		Proc()->currentStatementStart = tokens.start; /* So that tag is set in CF stack record. */
		if(tokens.command != NULL)
			(*tokens.command->inactive)(TRUE);
		
		ClearTokenSequence(&tokens);
	}
	
	DisposeTokenSequence(&tokens);
	
	if(WithinFileBuffer(Proc()->buffer, Proc()->currentStatementStart) && errorCount >= MAX_REPORTED_SYNTAX_ERRORS) {
		GetLocationInfo(Proc()->buffer, Proc()->currentStatementStart, &line, &file);
		ReportError(ER_TOO_MANY_SYNTAX_ERRORS, file, line, NULL, NULL);
	}
	
	/* Report any unclosed block control statements. */
	if(errorCount < MAX_REPORTED_SYNTAX_ERRORS && (error = CheckForUnbalancedBlocks(FALSE)) != SUCCESS) {
		GetLocationInfo(Proc()->buffer, Proc()->currentStatementStart, &line, &file);
		ReportError(error, file, line, Proc()->currentStatementStart, Proc()->additionalErrorInfo);
	}
	
	/* Reset current position for start of execution. */
	Proc()->currentStatementStart = Proc()->currentPosition = FileBufferBase(Proc()->buffer);
	
	return errorCount == 0;
}
