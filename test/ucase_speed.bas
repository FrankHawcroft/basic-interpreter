def pucasealtiter$(s$, out$) static where left(s, 1) in plcase as pucasealtiter$(mid2(s, 2), out & chr(asc(left(s, 1)) - 32))
def pucasealtiter$(s$, out$) static where s <> "" as pucasealtiter$(mid2(s, 2), out & left(s, 1))
def pucasealtiter$(s$, out$) static as out
def ucasealt$(uca$) static as pucasealtiter$(uca, "")

'def pxlititertuned$(first$, rest$, acct$, tfromt$, ttot$) static where first in tfromt as pxlititertuned(mid(rest, 1, 1), mid(rest, 2, pmaxstrlen), acct & mid(ttot, instr(1, tfromt, first), 1), tfromt, ttot)
'def pxlititertuned$(first$, rest$, acct$, tfromt$, ttot$) static where first <> "" as pxlititertuned(mid(rest, 1, 1), mid(rest, 2, pmaxstrlen), acct & first, tfromt, ttot)
'def pxlititertuned$(first$, rest$, acct$, tfromt$, ttot$) static as acct

'const one% = 1 | const two% = 2

'def pxlititertuned$(st$, acct$, tfromt$, ttot$) static where mid(st, 1, 1) in tfromt as pxlititertuned(mid(st, 2, pmaxstrlen), acct & mid(ttot, instr(1, tfromt, mid(st, 1, 1)), 1), tfromt, ttot)
'def pxlititertuned$(st$, acct$, tfromt$, ttot$) static where st <> "" as pxlititertuned(mid(st, 2, pmaxstrlen), acct & mid(st, 1, 1), tfromt, ttot)
'def pxlititertuned$(st$, acct$, tfromt$, ttot$) static as acct

def pxlititertuned$(st$, acct$) static where mid(st, 1, 1) in plcase as pxlititertuned(mid(st, 2, pmaxstrlen), acct & mid(pucase, instr(1, plcase, mid(st, 1, 1)), 1))
def pxlititertuned$(st$, acct$) static where st <> "" as pxlititertuned(mid(st, 2, pmaxstrlen), acct & mid(st, 1, 1))
def pxlititertuned$(st$, acct$) static as acct

def UCASEtuned$(pucast$) static as pxlititertuned(pucast, "") ', plcase, pucase) ' pxlititertuned(mid(pucast, 1, 1) , mid(pucast, 2, pmaxstrlen), "", plcase, pucase)
'def LCASE$(plcas$) static as pxlititer(plcas, "", pucase, plcase)

def randomstr$(length%) where length = 0 as ""
def randomstr$(length%) as chr(32 + random(94)) & randomstr(length - 1)

randomize timer
for i = 1 to 1000
	teststr$ = randomstr(random(101))
	u1$ = UCASEtuned(teststr)
	u2$ = ucasealt(teststr)
	print teststr, "->", u1
	print teststr, "->", u2
	if u1 <> u2 then error 255
next
