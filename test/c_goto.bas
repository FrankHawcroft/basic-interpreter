REM Test traditional control flow statements - GOTO, GOSUB and RETURN

i% = 0

x:
i = i + 1
'print i
if i > 5 goto y
goto x

y:
print "Forward GOTO arrived"
i = i * 2
gosub z
if i <> 100 then print "failed test - i doesn't have expected value of 100, is "; i
END

z:
print "Forward GOSUB arrived"
if i <> 12 then print "failed test - i doesn't have expected value of 12, is "; i
i = 100
return
