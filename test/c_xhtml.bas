'' Test module for XHTML.BAS.

merge "xhtml.bas" | gosub L_XHTML

sub HtError((Msg$))
    print "XHTML.BAS returned error: ", Msg
    system 1
endsub

HtOutputToString
HtStartStandardDocument "Test HTML Document"
HtStartAttrs1 "style", "type", "text/css"
HtEnd "style"
HtEnd "head"
HtStart "body"
HtPlainText "h1", "Test Document"
HtPlainText "p", "And here's a paragraph & a half!"
HtEndDocument
s$ = ""
HtGetAccumulatedOutput s
print s
'xfree
END
