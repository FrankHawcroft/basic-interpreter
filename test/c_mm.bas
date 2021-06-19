''
'' METAMAKE.BAS
''
'' This is a version of /util/metamake.bas just for testing the interpreter -
'' actual writing out of the makefile content is commented out.

'' TEST CONFIG:
'' This assumes runtests.(cmd|sh|amiga) is run with the 'test' directory as cwd, and that directories
'' are set up in a particular structure ...

DEFINT a-z

on error goto WindowsPlatform
open "i", 1, "env:Kickstart"
close 1

'' Amiga:
print "Platform is Amiga"
ParentDir$ = "/"
TempDir$ = "t:"
CurDir$ = ""
goto FinishedPlatformCheck

WindowsPlatform:
print "Platform is Windows"
ParentDir$ = "../"
TempDir$ = "C:\Windows\Temp\" '' "/tmp/"
CurDir$ = "."
resume FinishedPlatformCheck

FinishedPlatformCheck:
forget error

const SrcDir$ = ParentDir & "src"
const TestDir$ = ParentDir & "test"

''
'' Constants
''

const MaxFiles = 100
const MaxDependencies = 20
const MaxLinesToScan = 100
const SourceFileExt$ = ".c"
const HeaderFileExt$ = ".h"
const ObjectFileExt$ = ".o"
const AsmFileExt$ = ".s"
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

def Extension$(Filename$) static as right(Filename, 2)
def NamePart$(NameWithExt$) static as left(NameWithExt, len(NameWithExt) - 2)
def ObjFileName$(SourceFileName$) static as NamePart(SourceFileName) + ObjectFileExt
def MakeTempFilename$ as TempDir + "mm_temp_" + mid(str(timer), 2, 100) + ".txt"

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

''
'' Subprograms
''

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

sub ReadNextInclude((FileID%), LineNum%, IncludeName$) static
	IncludeName$ = ""
	while not eof(FileID) and LineNum <= MaxLinesToScan _
	and IncludeName = ""
		L$ = ""
		flineinput FileID, L
		LineNum = LineNum + 1
		Pos% = instr(1, L, IncludeStmt)
		if Pos > 0 then
			CommentPos% = instr(1, L, CommentStart)
			if CommentPos = 0 or CommentPos > Pos then
				ScanPos% = Pos + IncludeStmtLen
				SecondQuotePos% = instr(ScanPos + 1, L, Quote)
				IncludeName = mid(L, ScanPos, _
				  SecondQuotePos - ScanPos)
			endif
		endif
	wend
endsub

sub FindFile((TheName$), Idx) static
	shared Filename(), FileCount
	
	Idx = 0
	'TheName = ucase(TheName)
	for I = 1 to FileCount
		if Filename(I) = TheName then  '' distinguish betw UC and lc
			Idx = I
			exitsub
		endif
	next
endsub

sub MergeDependency((TheName$), Idx) static
	shared DependsOn(), DependCount()

	'SearchName$ = ucase(TheName)	'' save a call to ucase in the loop
	for I = 1 to DependCount(Idx)
		if TheName = DependsOn(Idx, I) then
			exitsub
		endif
	next
	DependCount(Idx) = DependCount(Idx) + 1
	DependsOn(Idx, DependCount(Idx)) = TheName
endsub

sub CopyDependencies((FromIdx%), (ToIdx%)) static
	shared DependsOn(), DependCount()

	for I = 1 to DependCount(FromIdx)
		MergeDependency DependsOn(FromIdx, I), ToIdx
	next
endsub

'' Print a makefile-style comment:

sub PrintComment((Comment$)) static
	shared Column

	if Column > 1 then
		'print
	endif
	'print "# ", Comment
	Column = 1
endsub

'' Print a word to a makefile, adding a line continuation if necessary:

sub PrintWord((Word$)) static
	shared Column
	
	if Column + len(Word) + 1 > Columns - len(LineContinuation) then
		'' 1 is added to length of word for leading space.
		'print LineContinuation
		'print chr(9), Word;
		Column = SpacesPerTab + len(Word)
	else
		if Column > 1 then
			'print " ";
			Column = Column + 1
		endif
		'print Word;
		Column = Column + len(Word)
	endif
endsub

'' End the current group in the makefile, e.g. a list of dependencies in the
'' target section:

sub EndGroup static
	shared Column

	'print | print chr(9);
	Column = SpacesPerTab
endsub

'' End the current section in the makefile, e.g. a list of dependencies for an
'' object file, or the whole target section:

sub EndSection static
	shared Column

	'print | print
	Column = 1
endsub

'' Split some text over multiple lines.  The division of the text into words is
'' respected; a word is defined as a sequence of non-blank characters.
''	Text is the input text, which is assumed not to already contain 
'' newlines.
''	LineLength is the maximum length of a line, not including the newline
'' character.
''	Split is some text to add at the end of each split line.
''	First is some text to add to the beginning of each split line; this
'' is _not_ prepended to the first line.  (I made this decision as a
'' convenience for the way dependencies are printed in makefiles.)
''	Result is the output text.

'' *** CURRENTLY NOT USED ***
sub SplitOverLines((Text$), (LineLength), (Split$), (First$), Result$)
	const WS$ =  " " + chr(9)

	Result = ""
	IsFirstLine? = true
	SplitLen% = len(Split)
	FirstLen% = len(First)

	while len(Text) > LineLength
		if IsFirstLine then
			IsFirstLine = false
			TheLine = ""
			CharsToTake% = LineLength - SplitLen
			LineLength = CharsToTake - FirstLen
		else
			TheLine$ = First
			CharsToTake% = LineLength
		endif
		TheLine = TheLine + left(Text, CharsToTake)
		SplitIndex% = len(TheLine)
		while not (mid(TheLine, SplitIndex, 1) in WS)
			SplitIndex = SplitIndex - 1
			CharsToTake = CharsToTake - 1
		wend
		TheLine$ = left(TheLine, SplitIndex) + Split
		Text = mid(Text, CharsToTake + 1, 30000)
		Result = Result + TheLine + chr(10)
	wend
	if Result = "" then
		Result = Text
	else
		Result = Result + First + Text
	endif
endsub

sub FatalError((Message$))
	print "** Error: ", Message
	fail 20
endsub

''
'' Main program
''

'' Get the name of the target:

Target$ = "NB" '' argv(1) '' hardcoded for test version

'' Generate the listing file for the directory:

chdir SrcDir '' TEST VERSION ONLY!

ListFilename$ = MakeTempFilename
'print ListFilename
open "o", 1, ListFilename
files CurDir, , 1
close 1

'' Read the initial list of source and header files:

open "i", 1, ListFilename
NextName$ = ""
while not eof(1)
	flineinput 1, NextName
	Ext$ = Extension(NextName)
	if Ext = SourceFileExt or Ext = HeaderFileExt or Ext = AsmFileExt then
		FileCount = FileCount + 1
		Filename(FileCount) = NextName
		DependCount(FileCount) = 0
		HeaderFile(FileCount) = Ext = HeaderFileExt

		'' .s files are legitimate source files, but we don't bother
		'' scanning them for dependencies.
		Complete(FileCount) = Ext = AsmFileExt

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

chdir TestDir '' TEST VERSION ONLY!

'' Write out the makefile:

	'' The comment at the top:

Comment$ = "Automatically generated NorthC makefile for " + Target
PrintComment Comment
Comment$ = "Makefile generated by " + argv(0) + " on " + date + " at " + time
PrintComment Comment
'print

	'' The dependencies for the target -- all the object files:

Column% = 1
PrintWord Target
PrintWord ":"
for I = 1 to FileCount
	if not HeaderFile(I) then
		PrintWord ObjFileName(Filename(I))
	endif
next
EndGroup
'print "blink with $*.blink SMALLDATA SMALLCODE";
EndSection

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
next

	'' Print commented-out optimiser action: (an Amiga relic; left in this test version)

PrintComment ".c.o : "
PrintComment chr(9) + "NorthC -Ot:$*.s $*.c"
PrintComment chr(9) + "top t:$*.s t:$*.s1"
PrintComment chr(9) + "a68k -g -q -O$*.o t:$*.s1"
PrintComment chr(9) + "delete t:$*.s t:$*.s1"

print FileCount, " files processed OK!" '' test version - less output!

'XFREE
