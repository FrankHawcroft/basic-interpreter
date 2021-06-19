'' Utility functions and subprograms.

'' Primitive functions defined in terms of other primitive functions -

def p__len_iter%(s$, acc%) where s = "" as acc
def p__len_iter%(s$, acc%) as p__len_iter(MID(s, 2), 1 + acc)
def P_LEN%(s$) as p__len_iter%(s, 0)

def P_Mid$(s$, idx%, n%) where idx = 1 as left(s, n)
def P_Mid$(s$, idx%, n%) where idx > 1 as P_Mid(right(s, len(s) - 1), idx - 1, n)

def P_INSTR%(I, SearchIn$, Sought$) where I = 0 as P_INSTR(1, SearchIn, Sought)
def P_INSTR%(I, SearchIn$, Sought$) where SearchIn = "" and Sought = "" as 1
def P_INSTR%(I, SearchIn$, Sought$) where I + len(Sought) + 1 > len(SearchIn) as 0
def P_INSTR%(I, SearchIn$, Sought$) where mid(SearchIn, I, len(Sought)) = Sought as I
def P_INSTR%(I, SearchIn$, Sought$) as P_INSTR(I + 1, SearchIn, Sought)

const p_e# = 2.7182818284590452

def P_EXP#(x#) as p_e ^ x ' the built-in version will give a better result, faster!

'' Unfortunately, doesn't work with the Windows or MSYS consoles -
sub CLS
	print chr(27), "[2J", chr(27), "[;H";
	'print chr(12)
endsub

def REPLACE$(S$, T$, U$) where instr(1, S, T) as left(S, instr(1, S, T) - 1) & U & REPLACE(mid(S, instr(1, S, T) + len(T), pmaxstrlen), T, U)
def REPLACE$(S$, T$, U$) as S
