i% = 1

REM Test comments and continuation lines.
REM Note that this program deliberately doesn't begin with a comment, in case the interpreter has a bug where the first line is handled differently,
REM so a first-line comment would mask a scanning problem.

let i = i _
	+ 1

let i = i - _ '' comments after a continuation are ignored, even if containing a continuation _
	1 REM and further comments on subsequent continued lines are fine too
	
' Comments containing comment-introducers like ' and REM should be OK too. And continuations in comments should be ignored _
let i = i + 1 ' _ like this one
let i = i + 1 REM or this one after a ' and as the final character on the line _
let i = i + 1

' Also labels in comments are ignored. x: !@#
x:
if i < 4 goto x
