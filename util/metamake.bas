''
'' METAMAKE.BAS
''
'' Creates a Makefile for a directory of C source files.

defint a-z

''
'' Constants
''

const MaxFiles = 100
const MaxDependencies = 20
const MaxLinesToScan = 100
const SourceFileExt$ = ".c"
const HeaderFileExt$ = ".h"
const Quote@ = chr(34)
const IncludeStmt$ = "#include " + Quote
const IncludeStmtLen = len(IncludeStmt)
const CommentStart$ = "/*"
const Columns = 79
const LineContinuation$ = " \"
const SpacesPerTab = 8

''
'' Functions
''

def Extension$(Filename$) where instr(1, Filename, ".") = 0 as ""
def Extension$(Filename$) as mid2(Filename, instr(1, Filename, "."))

def NamePart$(NameWithExt$) as left(NameWithExt, len(NameWithExt) - len(Extension(NameWithExt)))

def ObjFileName$(SourceFileName$) as NamePart(SourceFileName) + ObjectFileExt

def MakeTempFilename$ as TempDir + "mm_temp_" + mid(str(timer), 2, 100) + ".txt"

def CanonicalName$(FileName$) where CaseMatters as FileName
def CanonicalName$(FileName$) as ucase(FileName)

def Interesting?(Ext$) as CanonicalName(Ext) = CanonicalName(SourceFileExt) _
	or CanonicalName(Ext) = CanonicalName(HeaderFileExt) _
	or CanonicalName(Ext) = CanonicalName(AsmFileExt)

def FileNamesEqual?(A$, B$) as CanonicalName(A) = CanonicalName(B)

''
'' Global variables
''

dim Filename$(MaxFiles)
dim DependsOn$(MaxFiles, MaxDependencies)
dim DependCount(MaxFiles)
dim HeaderFile?(MaxFiles)
dim Complete?(MaxFiles)
dim Processing?(MaxFiles)
FileCount% = 0
NextFileID% = 0
Column% = 1
PriorSpace? = false

''
'' Subprograms
''

sub DetectPlatform(Platform$)
	on error goto WindowsPlatform
	open "i", 1, "env:Kickstart"
	close 1

	Platform = "Amiga"
	goto FinishedPlatformCheck
	
	WindowsPlatform:
	Platform = "Windows"
	resume FinishedPlatformCheck
	
	FinishedPlatformCheck:
	forget error
endsub

sub GetDependencies((Idx%))
	shared Filename(), Complete(), Processing()
	shared NextFileID

	if Processing(Idx) then
		FatalError "circular dependency involving " + Filename(Idx)
	endif

	if Complete(Idx) then
		exitsub
	endif

	Processing(Idx) = true

	'' Open the file:
	
	ID% = NextFileID
	NextFileID = NextFileID + 1
	open "i", ID, Filename(Idx)

	'' Scan the file:

	IncludedFile$ = ""
	LineNum% = 0
	ReadNextInclude ID, LineNum, IncludedFile
	while IncludedFile <> ""
		IncludeIdx% = 0
		FindFile IncludedFile, IncludeIdx
		if IncludeIdx > 0 then
			'' Only files in this dir are read!
			MergeDependency Filename(IncludeIdx), Idx
			GetDependencies IncludeIdx
			CopyDependencies IncludeIdx, Idx
		endif
		ReadNextInclude ID, LineNum, IncludedFile
	wend

	Processing(Idx) = false
	Complete(Idx) = true
	close ID
	NextFileID = ID
endsub

sub ReadNextInclude((FileID%), LineNum%, IncludeName$)
	IncludeName$ = ""
	while not eof(FileID) and LineNum <= MaxLinesToScan and IncludeName = ""
		L$ = ""
		flineinput FileID, L
		LineNum = LineNum + 1
		Pos% = instr(1, L, IncludeStmt)
		if Pos > 0 then
			CommentPos% = instr(1, L, CommentStart)
			if CommentPos = 0 or CommentPos > Pos then
				ScanPos% = Pos + IncludeStmtLen
				SecondQuotePos% = instr(ScanPos + 1, L, Quote)
				IncludeName = mid(L, ScanPos, SecondQuotePos - ScanPos)
			endif
		endif
	wend
endsub

sub FindFile((TheName$), Idx)
	shared Filename(), FileCount

	Idx = 0
	for I = 1 to FileCount
		if FileNamesEqual(Filename(I), TheName) then
			Idx = I
			exitsub
		endif
	next
endsub

sub MergeDependency((TheName$), Idx)
	shared DependsOn(), DependCount()

	for I = 1 to DependCount(Idx)
		if FileNamesEqual(TheName, DependsOn(Idx, I)) then
			exitsub
		endif
	next
	DependCount(Idx) = DependCount(Idx) + 1
	DependsOn(Idx, DependCount(Idx)) = TheName
endsub

sub CopyDependencies((FromIdx%), (ToIdx%))
	shared DependsOn(), DependCount()

	for I = 1 to DependCount(FromIdx)
		MergeDependency DependsOn(FromIdx, I), ToIdx
	next
endsub

'' Print a makefile-style comment:
sub PrintComment((Comment$))
	shared Column

	if Column > 1 then
		print
	endif
	print "# "; Comment
	Column = 1
endsub

'' Print a word to a makefile, adding a line continuation if necessary:
sub PrintWord((Word$))
	shared Column, PriorSpace
	
	if Column + len(Word) + 1 > Columns - len(LineContinuation) then
		'' 1 is added to length of word for leading space.
		print LineContinuation
		print chr(9); Word;
		Column = SpacesPerTab + len(Word)
	else
		if Column > 1 and not PriorSpace then
			print " ";
			Column = Column + 1
		endif
		print Word;
		Column = Column + len(Word)
	endif
	PriorSpace = right(Word, 1) in (" " + chr(9))
endsub

'' End the current group in the makefile, e.g. a list of dependencies in the
'' target section:

sub EndGroup
	shared Column, PriorSpace

	print | print chr(9);
	Column = SpacesPerTab
	PriorSpace = true
endsub

'' End the current section in the makefile, e.g. a list of dependencies for an
'' object file, or the whole target section:

sub EndSection
	shared Column

	print | print
	Column = 1
endsub

sub FatalError((Message$))
	print "** Error: ", Message
	system 1
endsub

''
'' Main program
''

'' Get the name of the target, and optional Makefile format to use:

if argc = 0 then
   print "Error: must supply command line parameter giving target name, and optionally a format."
   print "Format may be 'MINGW', 'MSVC', 'VBCC', or 'AMIGA-GCC'. 'MINGW' is the default."
   system 1
endif
Target$ = argv(1)
if argc > 1 then
	Format$ = ucase(argv(2))
else
	Format$ = "MINGW"
endif

'' Set format-specific options:

if Format = "MINGW"
	MakeSupportsVariables? = true
	OFE$ = "o"
	AFE$ = "s"
	Compile$ = "gcc -o $@"
	DefaultActionOptions$ = "-c $*.c"
	LinkOptions$ = ""
	Clean$ = "rm *.o"
elseif Format = "MSVC"
	MakeSupportsVariables? = true
	OFE$ = "obj"
	AFE$ = "asm"
	Compile$ = "cl"
	OptimisationOptions$ = "" ' "/O2 /Gr /GL /MD /DNDEBUG=1"
	if OptimisationOptions <> "" then
		DefaultActionOptions$ = "/c $*.c " + OptimisationOptions
	else
		DefaultActionOptions$ = ""
	endif
	LinkOptions$ = OptimisationOptions + " /link /out:" + Target + ".exe"
	Clean$ = "del *.obj"
elseif Format = "VBCC"
	MakeSupportsVariables? = false
	OFE$ = "o"
	AFE$ = "s"
	Compile$ = "vc"
	DefaultActionOptions$ = "-c -o $*.o $*.c -DAMIGA -DVBCC -c99 -lmieee -lamiga -lauto"
	LinkOptions$ = "-DAMIGA -DVBCC -c99 -lmieee -lamiga -lauto"
	Clean$ = "delete #?.o"
elseif Format = "AMIGA-GCC" '' cross-compiling on Windows
	MakeSupportsVariables? = true
	OFE$ = "o"
	AFE$ = "s"
	Compile$ = "m68k-amigaos-gcc"
	DefaultActionOptions$ = "-o $@ -c $*.c -resident -noixemul"
	LinkOptions$ = "-o $@ -resident -noixemul -lnixmain -lm" '' TODO now want this order: -resident -noixemul $(NBA_OBJS) -o $@ -lnixmain -lm
	Clean$ = "del *.o"
else
	print "Error: unknown output format "; Format
	system 1
endif

const ObjectFileExt$ = "." + OFE
const AsmFileExt$ = "." + AFE

'' Set host platform-specific options:

Platform$ = ""
DetectPlatform Platform
if Platform = "Amiga"
	'ParentDir$ = "/"
	TD$ = "t:"
	CurDir$ = ""
elseif Platform = "Windows"
	'ParentDir$ = "../"
	TD$ = "C:\Windows\Temp\" '' "/tmp/"
	CurDir$ = "."
else
	print "Error: unable to determine host platform"
	system 1
endif

const TempDir$ = TD
const CaseMatters? = false

'' Generate the listing file for the directory:

ListFilename$ = MakeTempFilename
open "o", 1, ListFilename
files CurDir,, 1
close 1

'' Read the initial list of source and header files:

open "i", 1, ListFilename
NextName$ = ""
while not eof(1)
	flineinput 1, NextName
	Ext$ = Extension(NextName)
	if Interesting(Ext) then
		FileCount = FileCount + 1
		Filename(FileCount) = NextName
		DependCount(FileCount) = 0
		HeaderFile(FileCount) = CanonicalName(Ext) = CanonicalName(HeaderFileExt)

		'' .s files are legitimate source files, but don't bother scanning them for dependencies.
		Complete(FileCount) = CanonicalName(Ext) = CanonicalName(AsmFileExt)

		Processing(FileCount) = false
	endif
wend
close 1
kill ListFilename

'' Recursively scan the source files for their dependencies:

for I = 1 to FileCount
	if not HeaderFile(I) then
		GetDependencies I
	endif
next

'' Write out the makefile:

	'' The comment at the top:

PrintComment "Automatically generated " + Format + " makefile for " + Target
if Format = "VBCC" then PrintComment "For use with NorthC make"
PrintComment "Makefile generated by " + argv(0) + " on " + date + " at " + time
print

	'' The dependencies for the target - all the object files - and how to make it:

Column% = 1

if MakeSupportsVariables then
	ObjVar$ = ucase(Target) + "_OBJS"
	PrintWord ObjVar
	PrintWord "="
else
	PrintWord Target
	PrintWord ":"
endif

for I = 1 to FileCount
	if not HeaderFile(I) then
		PrintWord ObjFileName(Filename(I))
	endif
next

if MakeSupportsVariables then
	EndSection
	PrintWord Target
	PrintWord ":"
	ObjVarSubst$ = "$(" + ObjVar + ")"
	PrintWord ObjVarSubst
	EndGroup
	print Compile; " "; ObjVarSubst; " "; LinkOptions;
elseif Format = "VBCC" then
	EndGroup
	print "execute vbcc_link_nb.amiga"; '' TODO should generate this too!
	'print Compiler;
	'for I = 1 to FileCount
	'	if not HeaderFile(I) then
	'		print " "; ObjFileName(Filename(I));
	'	endif
	'next
	'print " -o "; Target; " "; AdditionalCompilerOptions;
endif
EndSection

	'' How to make 'clean', i.e. delete all the object files:
	'' Excluded for Amiga due to problem with the #? wildcard looking like a Makefile comment ...

if Format <> "VBCC" then
	PrintWord "clean"
	PrintWord ":"
	EndGroup
	print Clean;
	EndSection
endif

	'' The default action to compile 'C' source files:
	'' This old-fashioned syntax is used for backward compatibility

if DefaultActionOptions <> "" then ' for MSVC, only needed for an optimised build
	PrintWord SourceFileExt + ObjectFileExt
	PrintWord ":"
	EndGroup
	print Compile; " "; DefaultActionOptions;
	EndSection
endif

	'' The dependencies for each object file:

for I = 1 to FileCount
	if not HeaderFile(I) and DependCount(I) > 0 then
		PrintWord ObjFileName(Filename(I))
		PrintWord ":"
		for J = 1 to DependCount(I)
			PrintWord DependsOn(I, J)
		next
		EndSection
	endif
Next

    '' Debugging output (not to be included in an actual Makefile):

'print FileCount, " files processed OK!" '' test version - less output!
'xfree
