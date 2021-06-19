''
'' XHTML.BAS
''
'' A module to help with generating XHTML compliant web pages.
''
'' Error handling: modules that use this file must define a subprogram
''	HtError((Msg$))
'' This is called when an error occurs.
''

if executed then
   print "Error: ", argv(0), " cannot be run as a program."
   system 1
endif

L_XHTML:

'' Useful constants:

const h__squote@ = "'"
const h__dquote@ = chr(34) '' " character
const h__nl@ = chr(10)

'' Element nesting state tracking:

const h__MaxElementNesting = 100
dim h__Element$(h__MaxElementNesting)
h__Nesting% = 0

sub h__Push((Elt$))
    shared h__Element$(), h__Nesting

    h__Nesting = h__Nesting + 1
    h__Element(h__Nesting) = Elt
endsub

sub h__Pop(Elt$)
    shared h__Element$(), h__Nesting

    if h__Nesting <= 0 then
       HtError "too many end tags"
    endif

    Elt = h__Element(h__Nesting)
    h__Nesting = h__Nesting - 1
endsub

'' Output control:

h__output_to_file? = false
h__output_string$ = ""
h__output_id% = 0

'' Note that changing the output target implies starting a new document!
sub HtOutputToFile((id%))
    shared h__output_to_file?, h__output_id%, h__Nesting%

    h__output_to_file = true
    h__output_id = id
    h__Nesting = 0
endsub

'' Note that changing the output target implies starting a new document!
sub HtOutputToString
    shared h__output_to_file?, h__output_string$, h__Nesting%

    h__output_to_file = false
    h__output_string = ""
    h__Nesting = 0
endsub

sub HtGetAccumulatedOutput(s$)
    shared h__output_to_file?, h__output_string$

    if h__output_to_file then
       HtError "outputting to file"
    else
		s = h__output_string
    endif
endsub

'' Doesn't encode entities! See HtText.
sub HtPrint((s$))
    shared h__output_to_file?, h__output_string$, h__output_id%

    if h__output_to_file then
       fprint h__output_id, s
    else
       h__output_string = h__output_string + s + h__nl
    endif
endsub

'' Formatting helpers:

def h__Replace$(s$, t$, u$) where instr(1, s, t) _
	as left(s, instr(1, s, t) - 1) & u & h__Replace(mid(s, instr(1, s, t) + len(t), 32767), t, u)
def h__Replace$(s$, t$, u$) as s

'' Transform single quotes to double quotes:
def h__QT$(s$) as h__Replace(s, h__squote, h__dquote)

'' Wrap a string in double quotes:
def h__QW$(s$) as h__dquote + s + h__dquote

'' Encode (the minimal, XML, set of) character entities:
'' Note that &apos; is excluded for HTML 4 compatibility.
def HtEscapeEntities$(s$) as h__Replace(h__Replace(h__Replace(h__Replace( _
    s, "&", "&amp;"), "<", "&lt;"), ">", "&gt;"), h__dquote, "&quot;")

'' Create an attribute name="value" pair:
'' Entities are escaped in the value string.
def h__AVPair$(A$, V$) as lcase(A) + "=" + h__QW(HtEscapeEntities(V))

'' Wrap start/end tag + attr text:
def h__Wrap$(s$) as "<" + s + ">"

'' Wrapping - specifically for tags with no attributes:
def h__WrapStartTag$(t$) as h__Wrap(lcase(t))
def h__WrapEndTag$(t$) as h__Wrap("/" + lcase(t))

'' Build up tags + various numbers of attr-value pairs:

def h__EA1$(E$, A1$, V1$) as lcase(E) + " " + h__AVPair(A1, V1)
def h__EA2$(E$, A1$, V1$, A2$, V2$) as lcase(E) + " " + h__AVPair(A1, V1) _
    + " " + h__AVPair(A2, V2)
def h__EA3$(E$, A1$, V1$, A2$, V2$, A3$, V3$) as lcase(E) + " " _
    + h__AVPair(A1, V1) + " " + h__AVPair(A2, V2) + " " + h__AVPair(A3, V3)

'' Tags with attributes formatted as start tags:

def h__SEA1$(E$, A1$, V1$) as h__Wrap(h__EA1(E, A1, V1))
def h__SEA2$(E$, A1$, V1$, A2$, V2$) as h__Wrap(h__EA2(E, A1, V1, A2, V2))
def h__SEA3$(E$, A1$, V1$, A2$, V2$, A3$, V3$) as _
    h__Wrap(h__EA3(E, A1, V1, A2, V2, A3, V3))

'' TO DO: support other versions, encodings, etc.
def HtStandardDocType$(ver$) where ver = "1.0" or ver = "1" as _
    h__QT("<?xml version='1.0' encoding='UTF-8'?>" + h__nl _
    	+ "<!DOCTYPE html PUBLIC '-//W3C//DTD XHTML 1.0 Transitional//EN' " _
    	+ "'http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd'>")

sub HtPrintStandardDocType((ver$))
    HtPrint HtStandardDocType(ver)
endsub

'' Output helpers for elements with content:

sub HtStart((elt$))
    h__Push elt
    HtPrint h__WrapStartTag(elt)
endsub

sub HtStartAttrs1((Elt$), (A1$), (V1$))
    h__Push Elt
    HtPrint h__SEA1(Elt, A1, V1)
endsub

sub HtStartAttrs2((Elt$), (A1$), (V1$), (A2$), (V2$))
    h__Push Elt
    HtPrint h__SEA2(Elt, A1, V1, A2, V2)
endsub

sub HtStartAttrs3((Elt$), (A1$), (V1$), (A2$), (V2$), (A3$), (V3$))
    h__Push Elt
    HtPrint h__SEA3(Elt, A1, V1, A2, V2, A3, V3)
endsub

sub HtEnd((elt$))
    actual$ = ""
    h__Pop actual
    if actual <> elt then
       HtError "mismatched end tag " + elt + "; element started as " + actual
    endif
    HtPrint h__WrapEndTag(elt)
endsub

sub HtEndDocument
    shared h__Element$(), h__Nesting

    while h__Nesting > 0
    	  HtEnd h__Element(h__Nesting)
    wend
endsub

sub HtText((Text$))
    HtPrint HtEscapeEntities(Text)
endsub

'' An element with simple textual content should have its entities escaped:
sub HtPlainText((Elt$), (Text$))
    HtPrint h__WrapStartTag(Elt) + HtEscapeEntities(Text) + h__WrapEndTag(Elt)
endsub

'' An element containing HTML:
sub HtWrapHTML((Elt$), (Text$))
    HtPrint h__WrapStartTag(Elt) + Text + h__WrapEndTag(Elt)
endsub

sub HtStartStandardDocument((title$))
    HtPrintStandardDocType "1.0"
    HtStartAttrs3 "html", "xmlns", "http://www.w3.org/1999/xhtml", _
    		 "lang", "en", "xml:lang", "en"
    HtStart "head"
    HtPlainText "title", Title
endsub

RETURN '' L_XHTML
