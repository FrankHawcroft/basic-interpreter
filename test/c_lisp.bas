
merge "lisp.bas" | gosub L_Lisp

Res$ = ""
EvalStrStr "(list 1 2 3 (cons 1 2) (cons 3 4))", Res
print Res
EvalStrStr "(define x 1)", Res
print Res
EvalStrStr "(set! x 10)", Res
print Res
EvalStrStr "(+ x 1 2 3 4 5 6)", Res
print Res | if val(Res) <> 31 | print "Failed test! Expected res = 31" | endif
EvalStrStr "(length (list (if (eq? x 1) (cons 1 2) (list 3 4 5)) 'a 'b 'c))", Res
print Res | if val(Res) <> 4 | print "Failed test! Expected res = 4" | endif
Res2$ = ""
EvalStrStr "(cons 1 2)", Res2
print Res2 | if Res2 <> "( 1 .  2)" | print "Failed test! Expected a pair (1 . 2)" | endif

'xfree

END
