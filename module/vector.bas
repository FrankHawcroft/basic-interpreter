'' General vector data type using strings.
'' This is designed to be more efficient for 'vectors' than the lisp.bas approach.

'' TO DO: change fcn param names to use notational convention that a stripped
'' vector is always called 'sv' and a "full" vector 'v' - will make less
'' confusing which function operates with what.

OPTION BASE 1

'' Testbed:
gosub L_Vector

v1$ = vempty
print "isvector on empty vector: ", visvector(v1)
print "length of empty vector: ", vlength(v1)

dim a$(10)
for i = 1 to 6 | read a(i) | next
data "Hi!", "this", "is", "", "a", "vector."
vinit v1, a, 6
print "Original vector:"
for i = 1 to vlength(v1)
	print i, vget(v1, i)
next
vset v1, 7, "With an extra element at the end!"
print "After adding element to end:"
for i = 1 to vlength(v1)
	print i, vget(v1, i)
next
print "After setting element in middle:"
vset v1, 4, "still"	' replace the empty element
for i = 1 to vlength(v1)
	print i, vget(v1, i)
next

'' Test storing a vector in a vector.
v2$ = vnew3("one", "two", "three") ' use ctor function
vset v1, 3, v2		' replace 'is'

print "With a vector as an element:"
for i = 1 to vlength(v1)
	e$ = vget(v1, i)
	if visvector(e) then
		for j = 1 to vlength(e)
			print i, "<Vector element>", j,  vget(e, j)
		next
	else
		print i, vget(v1, i)
	endif
next

print "Map (recursive):"
v3$ = vmapr("UCASE", v1)
for i = 1 to vlength(v3)
	e$ = vget(v3, i)
	if visvector(e) then
		for j = 1 to vlength(e)
			print i, "<Vector element>", j,  vget(e, j)
		next
	else
		print i, vget(v3, i)
	endif
next

print "Error case - test accessing element in non-vector:"
print vget("some innocuous string", 3)

END

L_Vector:

'' Internal constants.
const v_i_magic$ = "~V10" '' includes version number (currently '1[.]0')
const v_i_longstrlen = 32000
const v_i_binintlen = len(mki(0))
const v_i_errorcode = 252 '' 'unique' error code

''
'' ==== vempty ====
''
'' The empty vector.

const vempty$ = v_i_magic

''
'' ==== visvector ====
''
'' Check whether a string represents a vector.
'' Not thorough! - checks 'magic prefix' only.

def visvector?(v$) as left(v, len(v_i_magic)) = v_i_magic

'' Remove the 'magic prefix' from a vector so it can be scanned easily.
'' TO DO: the visvector() check is generally redundant - could be removed to speed up, but safer to have for now.
def v_i_strip$(v$) where visvector(v) as right(v, len(v) - len(v_i_magic))

''
'' ==== vget ====
''
'' Access an element in a vector.
'' Numbering starts at 1, for consistency with BASIC arrays.

def vget$(v$, idx%) where visvector(v) and idx >= 1 as v_i_get(v_i_strip(v), idx - 1)
'' Otherwise fails with an 'out of domain' error.

def v_i_get$(v$, idx%) where idx = 0 as mid(v, v_i_binintlen + 1, cvi(left(v, v_i_binintlen)))
def v_i_get$(v$, idx%) where idx > 0 as v_i_get(mid(v, v_i_binintlen + cvi(left(v, v_i_binintlen)) + 1, v_i_longstrlen), idx - 1)

''
'' ==== vgetopt ====
''
'' Access an element in a vector with a default value returned if the index is out of bounds.

'' TO DO

''
'' ==== vlength ====
''
'' Get the length of a vector.

def vlength(v$) where visvector(v) as v_i_len(v_i_strip(v), 0)

def v_i_len(v$, acclen%) where v = "" as acclen
def v_i_len(v$, acclen%) as v_i_len(mid(v, v_i_binintlen + cvi(left(v, v_i_binintlen)) + 1, v_i_longstrlen), acclen + 1)

''
'' ==== visempty ====
''
'' True if the vector is empty (i.e. has no elements).
'' TO DO: this implementation is nice and strict, but inefficient -

def visempty?(v$) as vlength(v) = 0 '' i.e. v = vempty BUT ALSO a valid vector

''
'' ==== vnew0, vnew1, vnew2, vnew3, vnew4, vnew5 ====
''
'' Create a vector with the given elements.

def vnew0$ as vempty
def vnew1$(e1$) as vnew0 + v_i_store(e1)
def vnew2$(e1$, e2$) as vnew1(e1) + v_i_store(e2)
def vnew3$(e1$, e2$, e3$) as vnew2(e1, e2) + v_i_store(e3)
def vnew4$(e1$, e2$, e3$, e4$) as vnew3(e1, e2, e3) + v_i_store(e4)
def vnew5$(e1$, e2$, e3$, e4$, e5$) as vnew4(e1, e2, e3, e4) + v_i_store(e5)

'' Prefix a string with its length, for storage in a vector in pseudo-IFF (minimalist IFF?) format.
def v_i_store$(elt$) as mki(len(elt)) + elt

''
'' ==== vmap, vmapr ====
''
'' A new vector with elements formed by calling the named function on each
'' element. The function must take one string and return a string.
'' Requires CALLBYNAME, added in v0.15.
'' vmap requires the elements not to be vectors, while vmapr maps recursively.

def vmap$(f$, v$) as v_i_map(f, v_i_strip(v), 0, vlength(v), "", false)

def vmapr$(f$, v$) as v_i_map(f, v_i_strip(v), 0, vlength(v), "", true)

def v_i_map$(f$, sv$, idx%, limit%, acc$, allowrec?) where idx >= limit as vempty + acc  
def v_i_map$(f$, sv$, idx%, limit%, acc$, allowrec?) where not visvector(v_i_get(sv, idx)) as _
	v_i_map(f, sv, idx + 1, limit, acc + v_i_store(callbyname(f, v_i_get(sv, idx))), allowrec)
def v_i_map$(f$, sv$, idx%, limit%, acc$, allowrec?) where allowrec as _
	v_i_map(f, sv, idx + 1, limit, acc + v_i_store(vmapr(f, v_i_get(sv, idx))), allowrec)

'' vfilter
'' vfilterr

'' TODO

'' vfoldl, vfoldr
'' vfoldlr, vfoldrr

'' TODO
	
''
'' ==== vinit ====
''
'' Initialise a new vector with nelts elements from the array.
'' nelts can be 0 (which will just create an empty vector).

sub vinit(v$, a$(), (nelts%))
	v = vempty
	for i = 1 to nelts
		v = v + v_i_store(a(i))
	next
endsub

''
'' ==== vexpand====
''
'' Expand the vector into an array.

sub vexpand((v$), a$(), nelts%)
	nelts = vlength(v)
	for i = 1 to nelts
		a(i) = vget(v, i)
	next
endsub

''
'' ==== vset ====
''
'' Set an element in a vector.
'' The vector can be expanded by one element only, by setting the element one past the end (vlength) of the vector.
'' TO DO: this is a very simple but very inefficient proof of concept implementation!  Could be considerably improved.

sub vset(v$, (idx%), (elt$))
	veclength% = vlength(v)

	'' Special case if appending to the vector.  This can be done quickly.
	if idx = veclength + 1 then
		v = v + v_i_store(elt)
	elseif idx > veclength + 1 or idx <= 0 then
		error v_i_errorcode	
	else
		dim vectorelts$(veclength)

		checklength% = 0
		vexpand v, vectorelts, checklength
		'' TO DO: debugging check only ...
		if veclength <> checklength then
			print "Inconsistent length calculations: ", veclength, checklength
			error v_i_errorcode
		endif

		vectorelts(idx) = elt
		vinit v, vectorelts, veclength
	endif
endsub

'' vfirst, vsecond, vthird, vrest, etc.

'' TO DO

'' vreverse (command) and vreversed (function)

'' TO DO

'' vpush, vpop, vshift, vunshift, etc.

'' TO DO

'' vsplit, vjoin

'' TO DO

'' Construct vector with / get / set elements of different primitive types.

'' TO DO

RETURN '' Vector
