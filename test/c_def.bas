'' Test - simple programmatically DEFined functions.

def half#(x#) as x / 2

if half(10) <> 5 or half(15) <> 7.5 then | print "Failed test!" | error 999 | endif
'print half(10), half(15)

def fact#(n&) where n <= 1 as 1
def fact#(n&) as n * fact(n - 1)

if fact(4) <> 24 or fact(5) <> 120 then | print "Failed test!" | error 999 | endif
'print fact(3), fact(4), fact(5)

const max_str_len% = 32767

def f$(t$) where t = "" as ""
def f$(t$) where len(t) >= 1 as "x" + f(mid(t, 1, len(t) - 1))

if f("xyz") <> "xxx" then | print "Failed test!" | error 999 | endif

def interleave$(s$, t$) as interleave_iter(s, t, "", 1)
def interleave_iter$(s$, t$, res$, idx%) where idx > len(s) and idx > len(t) as res
def interleave_iter$(s$, t$, res$, idx%) where idx > len(s) as res & mid(t, idx, max_str_len)
def interleave_iter$(s$, t$, res$, idx%) where idx > len(t) as res & mid(s, idx, max_str_len)
def interleave_iter$(s$, t$, res$, idx%) as interleave_iter(s, t, res & mid(s, idx, 1) & mid(t, idx, 1), idx + 1)

if interleave("aaaaaa", "bbb") <> "abababaaa" then | print "Failed test!" | error 999 | endif
'print interleave("aaaaaa", "bbb")

print "DEFined function tests completed OK"
