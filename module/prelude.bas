const TRUE? = 1
const FALSE? = not TRUE

def ABS#(pabsx#) static where pabsx < 0 as -pabsx
def ABS#(pabsx#) static as pabsx

def SGN%(psgnx#) static where psgnx = 0 as 0
def SGN%(psgnx#) static where psgnx > 0 as 1
def SGN%(psgnx#) static where psgnx < 0 as -1

def FIX&(pfixx#) static as pfixx

const pmaxlng& = 0x7FFFFFFF
const pminlng& = 0x80000000

def INT&(pintx!) static where pminlng < pintx and pintx < 0 as FIX(pintx) - 1
def INT&(pintx!) static where pintx <= pmaxlng as FIX(pintx)

def CDBL#(pcdbx#) static as pcdbx

def CSNG!(pcsnx#) static as pcsnx

def CLNG&(pclnx#) static where pminlng <= pclnx and pclnx <= pmaxlng and ABS(pclnx - FIX(pclnx)) >= 0.5 as FIX(pclnx) + SGN(pclnx)
def CLNG&(pclnx#) static where pminlng <= pclnx and pclnx <= pmaxlng as pclnx

def CINT%(pcinx#) static as CLNG(pcinx)

def CSTR$(pcstx#) static as pcstx & ""

def INSTR2%(pinss$, pinst$) static as instr(1, pinss, pinst)

const pmaxstrlen% = 32767

def MID2$(pmids$, pmidn%) static as mid(pmids, pmidn, pmaxstrlen)

def LEFT$(plefs$, plefn%) static where plefn as mid(plefs, 1, plefn)
def LEFT$(plefs$, plefn%) static where plefn = 0 as ""

def RIGHT$(prigs$, prign%) static where prign > len(prigs) as prigs
def RIGHT$(prigs$, prign%) static where prign as mid(prigs, len(prigs) - prign + 1, prign)
def RIGHT$(prigs$, prign%) static where prign = 0 as ""

def pstringiter$(n%, c@, acc$) static where n > 0 as pstringiter(n - 1, c, acc & c)
def pstringiter$(n%, c@, acc$) static as acc
def STRING$(pstrn%, pstrc@) static where pstrn >= 0 as pstringiter(pstrn, pstrc, "")

def SPACE$(pspan%) static as STRING(pspan, " ")

def SPC$(pspcn%) static as SPACE(pspcn)

def pxlititer$(s$, acc$, tfrom$, tto$) static where mid(s, 1, 1) in tfrom as pxlititer(mid(s, 2, pmaxstrlen), acc & mid(tto, instr(1, tfrom, mid(s, 1, 1)), 1), tfrom, tto)
def pxlititer$(s$, acc$, tfrom$, tto$) static where s <> "" as pxlititer(mid(s, 2, pmaxstrlen), acc & mid(s, 1, 1), tfrom, tto)
def pxlititer$(s$, acc$, tfrom$, tto$) static as acc
const pucase$ = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
const plcase$ = "abcdefghijklmnopqrstuvwxyz"
def UCASE$(pucas$) static as pxlititer(pucas, "", plcase, pucase)
def LCASE$(plcas$) static as pxlititer(plcas, "", pucase, plcase) 

sub LSET(s$, (t$)) static
	sl% = len(s)
	tl% = len(t)
	if tl > sl then error 15 '' bad substring
	s = t & SPACE(sl - tl)
endsub

sub RSET(s$, (t$)) static
    sl% = len(s)
    tl% = len(t)
    if tl > sl then error 15
    s = SPACE(sl - tl) & t 
endsub

sub LETMID(s$, (st%), (cnt%), (t$)) static
    sl% = len(s)
    tl% = len(t)
    if st < 0 or st > sl or cnt < 0 then error 15
    if st + cnt > sl + 1 then cnt = sl - st + 1
    if cnt > tl then cnt = tl
    s = left(s, st - 1) & left(t, cnt) & right(s, sl - (st + cnt) + 1)
endsub

const plittleendian? = cvi("x" & mid(mki(0), 1, 1)) < 255
if plittleendian goto plechr
def CHR@(pchrn%) static where 0 <= pchrn and pchrn <= 255 as right(mki(pchrn), 1)
def ASC%(pascc@) static as cvi(chr(0) & pascc)
goto pskiplechr
plechr:
def CHR@(pchrn%) static where 0 <= pchrn and pchrn <= 255 as left(mki(pchrn), 1)
def ASC%(pascc@) static as cvi(pascc & chr(0))
pskiplechr:

const p2ndbit& = 0x40000000
def plsr&(n&, c%) where n >= 0 as n \ clng(2 ^ c)
def plsr&(n&, c%) where n < 0 and c = 0 as n
def plsr&(n&, c%) where n < 0 and c > 0 as plsr(p2ndbit, c - 1) bitor plsr(n bitand pmaxlng, c)
const phdigit$ = "0123456789ABCDEF"
def p2bstr$(n&, b%) where n bitand (b - 1) = n as mid(phdigit, n + 1, 1)
def p2bstr$(n&, b%) as p2bstr(plsr(n, cint(log(b) / log(2))), b) & p2bstr(n bitand (b - 1), b)
def HEX$(phexn&) static as p2bstr(phexn, 16)
def OCT$(poctn&) static as p2bstr(poctn, 8)
