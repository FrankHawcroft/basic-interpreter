# Automatically generated MSVC makefile for nb
# Makefile generated by ..\util\metamake.bas on 09-22-2024 at 14:23:46

NB_OBJS = audio.obj basicio.obj bitvector.obj bobject.obj buffer.obj \
	cache.obj controlflow.obj errors.obj eval.obj events.obj \
	functions.obj gui.obj hashtable.obj heap.obj io.obj label.obj \
	lexer.obj loader.obj main.obj operators.obj options.obj pit.obj \
	platform_amiga.obj platform_generic.obj platform_mingw.obj \
	platform_msvc.obj process.obj profile.obj punctuation.obj qstring.obj \
	repl.obj scalar.obj semantics.obj stack.obj statements.obj subs.obj \
	sugar.obj symtab.obj syntax.obj vars.obj

nb : $(NB_OBJS)
	cl $(NB_OBJS)  /link /out:nb.exe

clean :
	del *.obj

audio.obj : interpreter.h common.h qstring.h errors.h platform.h builtin.h \
	heap.h process.h

basicio.obj : common.h errors.h platform.h qstring.h basicio.h

bitvector.obj : common.h platform.h bitvector.h

bobject.obj : interpreter.h common.h qstring.h errors.h process.h buffer.h

buffer.obj : common.h platform.h heap.h buffer.h

cache.obj : common.h heap.h qstring.h hashtable.h cache.h platform.h

controlflow.obj : interpreter.h common.h qstring.h errors.h builtin.h \
	process.h heap.h stack.h platform.h buffer.h cache.h hashtable.h

errors.obj : common.h errors.h

eval.obj : interpreter.h common.h qstring.h errors.h stack.h platform.h

events.obj : interpreter.h common.h qstring.h errors.h process.h builtin.h \
	platform.h sign.h cqueue.h stack.h heap.h audio.h

functions.obj : interpreter.h common.h qstring.h errors.h process.h \
	builtin.h options.h buffer.h platform.h heap.h stack.h basicio.h \
	hashtable.h

gui.obj : interpreter.h common.h qstring.h errors.h platform.h builtin.h \
	heap.h process.h gui_amiga.h gui_generic.h

hashtable.obj : hashtable.h common.h qstring.h heap.h

heap.obj : common.h platform.h heap.h bitvector.h

io.obj : interpreter.h common.h qstring.h errors.h builtin.h process.h \
	platform.h heap.h basicio.h

label.obj : interpreter.h common.h qstring.h errors.h process.h buffer.h

lexer.obj : interpreter.h common.h qstring.h errors.h process.h heap.h

loader.obj : interpreter.h common.h qstring.h errors.h process.h options.h \
	buffer.h platform.h heap.h cache.h

main.obj : interpreter.h common.h qstring.h errors.h process.h platform.h \
	options.h heap.h buffer.h cache.h hashtable.h stack.h bitvector.h

operators.obj : interpreter.h common.h qstring.h errors.h sign.h

options.obj : common.h platform.h heap.h qstring.h options.h

pit.obj : interpreter.h common.h qstring.h errors.h builtin.h process.h \
	heap.h stack.h platform.h hashtable.h options.h

platform_amiga.obj : common.h platform.h platform_amiga_private.h

platform_generic.obj : common.h platform.h platform_mingw_private.h \
	platform_amiga_private.h platform_msvc_private.h \
	platform_generic_private.h

platform_mingw.obj : common.h platform.h

platform_msvc.obj : common.h platform.h

process.obj : process.h interpreter.h common.h qstring.h errors.h heap.h \
	options.h cache.h buffer.h hashtable.h platform.h

profile.obj : common.h platform.h heap.h buffer.h

punctuation.obj : interpreter.h common.h qstring.h errors.h

qstring.obj : common.h platform.h heap.h qstring.h sign.h

repl.obj : interpreter.h common.h qstring.h errors.h builtin.h process.h \
	heap.h buffer.h stack.h platform.h options.h

scalar.obj : interpreter.h common.h qstring.h errors.h platform.h basicio.h

semantics.obj : interpreter.h common.h qstring.h errors.h process.h

stack.obj : common.h platform.h heap.h stack.h

statements.obj : interpreter.h common.h qstring.h errors.h process.h \
	builtin.h heap.h hashtable.h

subs.obj : interpreter.h common.h qstring.h errors.h process.h builtin.h \
	heap.h buffer.h options.h hashtable.h

sugar.obj : interpreter.h common.h qstring.h errors.h builtin.h

symtab.obj : interpreter.h common.h qstring.h errors.h process.h hashtable.h \
	heap.h options.h

syntax.obj : interpreter.h common.h qstring.h errors.h heap.h hashtable.h \
	options.h buffer.h process.h

vars.obj : interpreter.h common.h qstring.h errors.h builtin.h process.h \
	platform.h heap.h buffer.h options.h

